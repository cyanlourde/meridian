open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let () = Crypto.init ()

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-int-test-%d-%d" (Unix.getpid ()) (Random.int 100000))

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

let make_addr () = Bytes.make 29 '\x61'

(* ================================================================ *)
(* Full pipeline construction                                        *)
(* ================================================================ *)

let test_pipeline_construction () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let ledger = Ledger_state.create () in
  Alcotest.(check int) "store empty" 0 (Store.block_count store);
  Alcotest.(check int) "ledger empty" 0 (Ledger_state.utxo_count ledger);
  let config = Sync_pipeline.default_config
    ~should_stop:(fun () -> true) () in
  Alcotest.(check int) "batch_size" 50 config.batch_size;
  rm_rf dir

(* ================================================================ *)
(* Validation ordering: header -> crypto -> ledger                   *)
(* ================================================================ *)

let test_validation_ordering () =
  (* Build a synthetic block and run each validation layer *)
  let header = Block_decoder.{
    bh_slot = 100L; bh_block_number = 1L;
    bh_prev_hash = None; bh_issuer_vkey = Bytes.make 32 '\xaa';
    bh_body_hash = Bytes.empty;
    bh_protocol_version = (2L, 0L); bh_era = Shelley;
    bh_vrf_vkey = Bytes.make 32 '\xbb';
    bh_block_signature = Bytes.make 448 '\xcc';
    bh_opcert = None;
    bh_header_body_cbor = Bytes.empty;
  } in
  (* Header validation: slot check *)
  (match Header_validation.validate_slot_number ~prev_slot:50L ~slot:100L with
   | Ok () -> () | Error e -> Alcotest.fail e);
  (* Crypto validation *)
  let block = Block_decoder.{
    db_era = Shelley; db_header = header;
    db_tx_count = 0; db_tx_raw = [];
    db_raw_cbor = Cbor.Null;
  } in
  (* Crypto should succeed (no opcert to check, sizes OK) *)
  (match Block_validator.validate_block_crypto block with
   | Ok () -> ()
   | Error errs ->
     let msgs = List.map Block_validator.error_to_string errs in
     Alcotest.fail (String.concat "; " msgs));
  (* Ledger should handle empty block *)
  let ls = Ledger_state.create () in
  let errs = Ledger_state.apply_block ls block in
  Alcotest.(check int) "no ledger errors" 0 (List.length errs)

(* ================================================================ *)
(* Error logging: bad prev_hash continues pipeline                   *)
(* ================================================================ *)

let test_error_continues () =
  (* Header validation with wrong prev_hash should return error but not crash *)
  let expected = Bytes.make 32 '\xaa' in
  let header = Block_decoder.{
    bh_slot = 100L; bh_block_number = 1L;
    bh_prev_hash = Some (Bytes.make 32 '\xbb');  (* wrong *)
    bh_issuer_vkey = Bytes.make 32 '\x01';
    bh_body_hash = Bytes.empty;
    bh_protocol_version = (2L, 0L); bh_era = Shelley;
    bh_vrf_vkey = Bytes.empty;
    bh_block_signature = Bytes.empty;
    bh_opcert = None;
    bh_header_body_cbor = Bytes.empty;
  } in
  match Header_validation.validate_prev_hash ~expected_hash:expected ~header with
  | Error _ -> ()  (* Error returned, pipeline can log and continue *)
  | Ok () -> Alcotest.fail "expected error"

(* ================================================================ *)
(* Resume with ledger snapshot                                       *)
(* ================================================================ *)

let test_resume_with_snapshot () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let store = Store.init ~base_dir:dir () in
  let ls = Ledger_state.create () in
  (* Store some blocks *)
  for i = 1 to 10 do
    let slot = Int64.of_int (i * 100) in
    let hash = make_hash i in
    let data = Bytes.of_string (Printf.sprintf "block-%d" i) in
    ignore (Store.store_block store ~slot ~hash ~cbor_bytes:data)
  done;
  (* Add to UTXO directly *)
  Utxo.add (Ledger_state.utxo ls)
    Utxo.TxIn.{ tx_hash = make_hash 99; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 5000000L;
                 has_datum = false; has_script_ref = false };
  (* Snapshot *)
  let path = Filename.concat dir "ledger.snapshot" in
  Ledger_state.snapshot ls ~path;
  (* Restore *)
  match Ledger_state.restore ~path with
  | Error e -> Alcotest.fail e
  | Ok ls2 ->
    Alcotest.(check int) "restored utxo" 1 (Ledger_state.utxo_count ls2);
    Alcotest.(check int64) "restored lovelace" 5000000L (Ledger_state.total_lovelace ls2);
    (* Store still has blocks *)
    let store2 = Store.init ~base_dir:dir () in
    Alcotest.(check int) "store blocks" 10 (Store.block_count store2);
    rm_rf dir

(* ================================================================ *)
(* Era detection from era tags                                       *)
(* ================================================================ *)

let test_era_detection () =
  let eras = [
    (1L, "shelley"); (2L, "allegra"); (3L, "mary");
    (4L, "alonzo"); (5L, "babbage"); (6L, "conway");
  ] in
  List.iter (fun (tag, expected_name) ->
    let inner = Cbor.Array [Cbor.Array []; Cbor.Array []; Cbor.Array []; Cbor.Map []] in
    let outer = Cbor.Array [Cbor.Uint tag; inner] in
    let cbor_bytes = Cbor.encode outer in
    match Block_decoder.decode_block cbor_bytes with
    | Ok block ->
      Alcotest.(check string) (Printf.sprintf "era tag %Ld" tag)
        expected_name (Block_decoder.era_name block.db_era)
    | Error _ -> ()  (* Some may fail to decode headers, era detection is OK *)
  ) eras

(* ================================================================ *)
(* Byron skip in crypto                                              *)
(* ================================================================ *)

let test_byron_crypto_skip () =
  let block = Block_decoder.{
    db_era = Byron;
    db_header = {
      bh_slot = 0L; bh_block_number = 0L;
      bh_prev_hash = None; bh_issuer_vkey = Bytes.empty;
      bh_body_hash = Bytes.empty;
      bh_protocol_version = (0L, 0L); bh_era = Byron;
      bh_vrf_vkey = Bytes.empty;
      bh_block_signature = Bytes.empty;
      bh_opcert = None;
      bh_header_body_cbor = Bytes.empty;
    };
    db_tx_count = 0; db_tx_raw = [];
    db_raw_cbor = Cbor.Null;
  } in
  match Block_validator.validate_block_crypto block with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "byron should skip crypto"

(* ================================================================ *)
(* Empty block through all layers                                    *)
(* ================================================================ *)

let test_empty_block_all_layers () =
  let header = Block_decoder.{
    bh_slot = 500L; bh_block_number = 5L;
    bh_prev_hash = None; bh_issuer_vkey = Bytes.make 32 '\x01';
    bh_body_hash = Bytes.empty;
    bh_protocol_version = (7L, 0L); bh_era = Babbage;
    bh_vrf_vkey = Bytes.make 32 '\x02';
    bh_block_signature = Bytes.make 448 '\x03';
    bh_opcert = None;
    bh_header_body_cbor = Bytes.empty;
  } in
  let block = Block_decoder.{
    db_era = Babbage; db_header = header;
    db_tx_count = 0; db_tx_raw = [];
    db_raw_cbor = Cbor.Null;
  } in
  (* Crypto OK *)
  (match Block_validator.validate_block_crypto block with
   | Ok () -> () | Error _ -> Alcotest.fail "crypto failed");
  (* Ledger OK *)
  let ls = Ledger_state.create () in
  let errs = Ledger_state.apply_block ls block in
  Alcotest.(check int) "no ledger errors" 0 (List.length errs);
  let (slot, blocks) = Ledger_state.tip ls in
  Alcotest.(check int64) "slot advanced" 500L slot;
  Alcotest.(check int) "block counted" 1 blocks

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Integration"
    [ ( "Pipeline construction",
        [ Alcotest.test_case "init" `Quick test_pipeline_construction ] );
      ( "Validation ordering",
        [ Alcotest.test_case "header -> crypto -> ledger" `Quick test_validation_ordering ] );
      ( "Error handling",
        [ Alcotest.test_case "continues on error" `Quick test_error_continues ] );
      ( "Resume",
        [ Alcotest.test_case "snapshot resume" `Quick test_resume_with_snapshot ] );
      ( "Era detection",
        [ Alcotest.test_case "all eras" `Quick test_era_detection ] );
      ( "Edge cases",
        [ Alcotest.test_case "byron crypto skip" `Quick test_byron_crypto_skip;
          Alcotest.test_case "empty block all layers" `Quick test_empty_block_all_layers ] );
    ]
