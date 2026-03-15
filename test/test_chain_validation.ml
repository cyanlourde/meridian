open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-cv-test-%d-%d" (Unix.getpid ()) (Random.int 100000))

let rm_rf dir =
  let rec go path =
    if Sys.is_directory path then begin
      Array.iter (fun f -> go (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
    end else Unix.unlink path
  in
  if Sys.file_exists dir then go dir

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF);
  Bytes.set_uint8 b 1 ((n lsr 8) land 0xFF); b

(* ================================================================ *)
(* Genesis block validation                                          *)
(* ================================================================ *)

let test_genesis_zero_hash () =
  let header = Block_decoder.{
    bh_slot = 0L; bh_block_number = 0L;
    bh_prev_hash = Some (Bytes.make 32 '\x00');
    bh_issuer_vkey = Bytes.empty;
    bh_body_hash = Bytes.empty;
    bh_protocol_version = (0L, 0L); bh_era = Byron;
    bh_vrf_vkey = Bytes.empty; bh_block_signature = Bytes.empty;
    bh_opcert = None; bh_header_body_cbor = Bytes.empty;
  } in
  match Chain_validation.validate_genesis_block header with
  | Ok () -> () | Error e -> Alcotest.fail e

let test_genesis_no_hash () =
  let header = Block_decoder.{
    bh_slot = 0L; bh_block_number = 0L;
    bh_prev_hash = None;
    bh_issuer_vkey = Bytes.empty;
    bh_body_hash = Bytes.empty;
    bh_protocol_version = (0L, 0L); bh_era = Byron;
    bh_vrf_vkey = Bytes.empty; bh_block_signature = Bytes.empty;
    bh_opcert = None; bh_header_body_cbor = Bytes.empty;
  } in
  match Chain_validation.validate_genesis_block header with
  | Ok () -> () | Error e -> Alcotest.fail e

let test_genesis_nonzero_fails () =
  let header = Block_decoder.{
    bh_slot = 0L; bh_block_number = 0L;
    bh_prev_hash = Some (Bytes.make 32 '\xff');
    bh_issuer_vkey = Bytes.empty;
    bh_body_hash = Bytes.empty;
    bh_protocol_version = (0L, 0L); bh_era = Byron;
    bh_vrf_vkey = Bytes.empty; bh_block_signature = Bytes.empty;
    bh_opcert = None; bh_header_body_cbor = Bytes.empty;
  } in
  match Chain_validation.validate_genesis_block header with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

(* ================================================================ *)
(* Epoch arithmetic                                                  *)
(* ================================================================ *)

let test_epoch_of_slot () =
  let p = Epoch.preview_epoch_params in
  Alcotest.(check int64) "slot 0 = epoch 0" 0L (Epoch.slot_to_epoch p 0L);
  Alcotest.(check int64) "slot 431999 = epoch 0" 0L (Epoch.slot_to_epoch p 431999L);
  Alcotest.(check int64) "slot 432000 = epoch 1" 1L (Epoch.slot_to_epoch p 432000L);
  Alcotest.(check int64) "slot 864000 = epoch 2" 2L (Epoch.slot_to_epoch p 864000L)

let test_slot_in_epoch () =
  let p = Epoch.preview_epoch_params in
  Alcotest.(check int64) "slot 0 in epoch" 0L (Epoch.slot_in_epoch p 0L);
  Alcotest.(check int64) "slot 100 in epoch" 100L (Epoch.slot_in_epoch p 100L);
  Alcotest.(check int64) "slot 432000 in epoch" 0L (Epoch.slot_in_epoch p 432000L);
  Alcotest.(check int64) "slot 432042 in epoch" 42L (Epoch.slot_in_epoch p 432042L)

let test_epoch_boundary () =
  let p = Epoch.preview_epoch_params in
  Alcotest.(check bool) "same epoch" false
    (Epoch.is_epoch_boundary p ~prev_slot:100L ~slot:200L);
  Alcotest.(check bool) "cross boundary" true
    (Epoch.is_epoch_boundary p ~prev_slot:431999L ~slot:432000L);
  Alcotest.(check bool) "same epoch end" false
    (Epoch.is_epoch_boundary p ~prev_slot:432000L ~slot:432001L)

let test_epoch_first_slot () =
  let p = Epoch.preview_epoch_params in
  Alcotest.(check int64) "epoch 0" 0L (Epoch.epoch_to_first_slot p 0L);
  Alcotest.(check int64) "epoch 1" 432000L (Epoch.epoch_to_first_slot p 1L);
  Alcotest.(check int64) "epoch 2" 864000L (Epoch.epoch_to_first_slot p 2L)

(* ================================================================ *)
(* Chain validation with fake blocks                                 *)
(* ================================================================ *)

let test_valid_chain () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  (* Build 10 fake blocks with simple CBOR data *)
  for i = 0 to 9 do
    let slot = Int64.of_int (i * 20) in
    let hash = make_hash i in
    let data = Bytes.of_string (Printf.sprintf "block-%d" i) in
    ignore (Store.store_block store ~slot ~hash ~cbor_bytes:data)
  done;
  (* validate_chain will try to decode blocks as CBOR, which will fail
     for our fake data. That's expected — this tests the chain iteration logic. *)
  let errors = Chain_validation.validate_chain store ~from_slot:0L ~to_slot:180L in
  (* All blocks will fail to decode, which is fine *)
  Alcotest.(check bool) "errors expected for fake blocks" true (List.length errors > 0);
  rm_rf dir

let test_empty_chain () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let errors = Chain_validation.validate_chain store ~from_slot:0L ~to_slot:100L in
  Alcotest.(check int) "no errors for empty" 0 (List.length errors);
  rm_rf dir

(* ================================================================ *)
(* Max block size per era                                            *)
(* ================================================================ *)

let test_max_block_sizes () =
  Alcotest.(check int) "shelley" 65536
    (Header_validation.max_block_size_for_era Shelley);
  Alcotest.(check int) "alonzo" 73728
    (Header_validation.max_block_size_for_era Alonzo);
  Alcotest.(check int) "babbage" 90112
    (Header_validation.max_block_size_for_era Babbage);
  Alcotest.(check int) "conway" 90112
    (Header_validation.max_block_size_for_era Conway)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Chain-Validation"
    [ ( "Genesis block",
        [ Alcotest.test_case "zero hash" `Quick test_genesis_zero_hash;
          Alcotest.test_case "no hash" `Quick test_genesis_no_hash;
          Alcotest.test_case "nonzero fails" `Quick test_genesis_nonzero_fails ] );
      ( "Epoch arithmetic",
        [ Alcotest.test_case "slot to epoch" `Quick test_epoch_of_slot;
          Alcotest.test_case "slot in epoch" `Quick test_slot_in_epoch;
          Alcotest.test_case "boundary" `Quick test_epoch_boundary;
          Alcotest.test_case "first slot" `Quick test_epoch_first_slot ] );
      ( "Chain validation",
        [ Alcotest.test_case "fake blocks" `Quick test_valid_chain;
          Alcotest.test_case "empty chain" `Quick test_empty_chain ] );
      ( "Block sizes",
        [ Alcotest.test_case "max per era" `Quick test_max_block_sizes ] );
    ]
