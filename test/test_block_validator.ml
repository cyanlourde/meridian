open Meridian

let () = Crypto.init ()

let _make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF); b

(* ================================================================ *)
(* Opcert signature tests                                            *)
(* ================================================================ *)

let test_opcert_valid () =
  (* Generate a cold keypair, create an opcert, sign it *)
  match Crypto.ed25519_keypair () with
  | Error e -> Alcotest.fail e
  | Ok (cold_pk, cold_sk) ->
    let hot_vkey = Bytes.make 32 '\xaa' in
    let oc = Block_decoder.{
      oc_hot_vkey = hot_vkey;
      oc_sequence_number = 1L;
      oc_kes_period = 200L;
      oc_cold_signature = Bytes.empty;  (* will be filled *)
    } in
    let message = Block_validator.opcert_signed_data oc in
    match Crypto.ed25519_sign ~secret_key:cold_sk ~message with
    | Error e -> Alcotest.fail e
    | Ok signature ->
      let oc = { oc with oc_cold_signature = signature } in
      match Block_validator.verify_opcert_signature ~issuer_vkey:cold_pk oc with
      | Ok () -> ()
      | Error e -> Alcotest.fail (Block_validator.error_to_string e)

let test_opcert_bad_sig () =
  match Crypto.ed25519_keypair () with
  | Error e -> Alcotest.fail e
  | Ok (cold_pk, cold_sk) ->
    let oc = Block_decoder.{
      oc_hot_vkey = Bytes.make 32 '\xaa';
      oc_sequence_number = 1L;
      oc_kes_period = 200L;
      oc_cold_signature = Bytes.empty;
    } in
    let message = Block_validator.opcert_signed_data oc in
    (match Crypto.ed25519_sign ~secret_key:cold_sk ~message with
     | Error e -> Alcotest.fail e
     | Ok signature ->
       (* Flip a bit *)
       let bad = Bytes.copy signature in
       Bytes.set_uint8 bad 0 (Bytes.get_uint8 bad 0 lxor 1);
       let oc = { oc with oc_cold_signature = bad } in
       match Block_validator.verify_opcert_signature ~issuer_vkey:cold_pk oc with
       | Error (Opcert_signature_invalid _) -> ()
       | _ -> Alcotest.fail "expected invalid sig")

let test_opcert_wrong_key () =
  match Crypto.ed25519_keypair (), Crypto.ed25519_keypair () with
  | Ok (_pk1, sk1), Ok (pk2, _sk2) ->
    let oc = Block_decoder.{
      oc_hot_vkey = Bytes.make 32 '\xaa';
      oc_sequence_number = 1L;
      oc_kes_period = 200L;
      oc_cold_signature = Bytes.empty;
    } in
    let message = Block_validator.opcert_signed_data oc in
    (match Crypto.ed25519_sign ~secret_key:sk1 ~message with
     | Error e -> Alcotest.fail e
     | Ok signature ->
       let oc = { oc with oc_cold_signature = signature } in
       (* Verify with wrong key *)
       match Block_validator.verify_opcert_signature ~issuer_vkey:pk2 oc with
       | Error (Opcert_signature_invalid _) -> ()
       | _ -> Alcotest.fail "expected wrong key failure")
  | _ -> Alcotest.fail "keypair failed"

(* ================================================================ *)
(* Size checks                                                       *)
(* ================================================================ *)

let test_kes_sig_size () =
  let good = Bytes.make 448 '\x00' in
  (match Block_validator.verify_kes_sig_size ~block_signature:good with
   | Ok () -> () | Error e -> Alcotest.fail (Block_validator.error_to_string e));
  let bad = Bytes.make 100 '\x00' in
  match Block_validator.verify_kes_sig_size ~block_signature:bad with
  | Error (Kes_signature_bad_size _) -> ()
  | _ -> Alcotest.fail "expected bad size"

let test_vrf_vkey_size () =
  (match Block_validator.verify_vrf_vkey_size ~vrf_vkey:(Bytes.make 32 '\x00') with
   | Ok () -> () | Error e -> Alcotest.fail (Block_validator.error_to_string e));
  (match Block_validator.verify_vrf_vkey_size ~vrf_vkey:(Bytes.make 31 '\x00') with
   | Error (Vrf_vkey_bad_size 31) -> ()
   | _ -> Alcotest.fail "expected 31");
  match Block_validator.verify_vrf_vkey_size ~vrf_vkey:(Bytes.make 33 '\x00') with
  | Error (Vrf_vkey_bad_size 33) -> ()
  | _ -> Alcotest.fail "expected 33"

let test_cold_key_size () =
  (match Block_validator.verify_cold_key_size ~issuer_vkey:(Bytes.make 32 '\x00') with
   | Ok () -> () | Error e -> Alcotest.fail (Block_validator.error_to_string e));
  match Block_validator.verify_cold_key_size ~issuer_vkey:(Bytes.make 16 '\x00') with
  | Error (Opcert_cold_key_bad_size 16) -> ()
  | _ -> Alcotest.fail "expected 16"

(* ================================================================ *)
(* Full validate_block_crypto                                        *)
(* ================================================================ *)

let test_validate_shelley_block_valid () =
  (* Build a synthetic Shelley block with valid opcert *)
  match Crypto.ed25519_keypair () with
  | Error e -> Alcotest.fail e
  | Ok (cold_pk, cold_sk) ->
    let hot_vkey = Bytes.make 32 '\xbb' in
    let oc_unsigned = Block_decoder.{
      oc_hot_vkey = hot_vkey;
      oc_sequence_number = 0L;
      oc_kes_period = 100L;
      oc_cold_signature = Bytes.empty;
    } in
    let msg = Block_validator.opcert_signed_data oc_unsigned in
    (match Crypto.ed25519_sign ~secret_key:cold_sk ~message:msg with
     | Error e -> Alcotest.fail e
     | Ok sig_ ->
       let oc = { oc_unsigned with oc_cold_signature = sig_ } in
       let header = Block_decoder.{
         bh_slot = 100L; bh_block_number = 1L;
         bh_prev_hash = None; bh_issuer_vkey = cold_pk;
         bh_body_hash = Bytes.empty;
         bh_protocol_version = (2L, 0L); bh_era = Shelley;
         bh_vrf_vkey = Bytes.make 32 '\xcc';
         bh_block_signature = Bytes.make 448 '\xdd';
         bh_opcert = Some oc;
         bh_header_body_cbor = Bytes.empty;
       } in
       let block = Block_decoder.{
         db_era = Shelley; db_header = header;
         db_tx_count = 0; db_tx_raw = [];
         db_raw_cbor = Cbor.Null;
         db_invalid_tx_indices = [];
       } in
       match Block_validator.validate_block_crypto block with
       | Ok () -> ()
       | Error errs ->
         Alcotest.fail (String.concat "; "
           (List.map Block_validator.error_to_string errs)))

let test_validate_block_bad_opcert () =
  let header = Block_decoder.{
    bh_slot = 100L; bh_block_number = 1L;
    bh_prev_hash = None; bh_issuer_vkey = Bytes.make 32 '\x01';
    bh_body_hash = Bytes.empty;
    bh_protocol_version = (2L, 0L); bh_era = Shelley;
    bh_vrf_vkey = Bytes.make 32 '\x02';
    bh_block_signature = Bytes.make 448 '\x03';
    bh_opcert = Some {
      oc_hot_vkey = Bytes.make 32 '\x04';
      oc_sequence_number = 0L;
      oc_kes_period = 0L;
      oc_cold_signature = Bytes.make 64 '\xff';  (* invalid sig *)
    };
    bh_header_body_cbor = Bytes.empty;
  } in
  let block = Block_decoder.{
    db_era = Shelley; db_header = header;
    db_tx_count = 0; db_tx_raw = [];
    db_raw_cbor = Cbor.Null;
    db_invalid_tx_indices = [];
  } in
  match Block_validator.validate_block_crypto block with
  | Error errs ->
    let has_opcert = List.exists (function
      | Block_validator.Opcert_signature_invalid _ -> true | _ -> false) errs in
    Alcotest.(check bool) "opcert error" true has_opcert
  | Ok () -> Alcotest.fail "expected error"

let test_validate_byron_skipped () =
  let header = Block_decoder.{
    bh_slot = 0L; bh_block_number = 0L;
    bh_prev_hash = None; bh_issuer_vkey = Bytes.empty;
    bh_body_hash = Bytes.empty;
    bh_protocol_version = (0L, 0L); bh_era = Byron;
    bh_vrf_vkey = Bytes.empty;
    bh_block_signature = Bytes.empty;
    bh_opcert = None;
    bh_header_body_cbor = Bytes.empty;
  } in
  let block = Block_decoder.{
    db_era = Byron; db_header = header;
    db_tx_count = 0; db_tx_raw = [];
    db_raw_cbor = Cbor.Null;
    db_invalid_tx_indices = [];
  } in
  match Block_validator.validate_block_crypto block with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "byron should be skipped"

(* ================================================================ *)
(* Error formatting                                                  *)
(* ================================================================ *)

let test_error_strings () =
  let errs = [
    Block_validator.Opcert_signature_invalid "test";
    Opcert_cold_key_bad_size 16;
    Opcert_hot_key_bad_size 31;
    Opcert_cold_sig_bad_size 32;
    Vrf_vkey_bad_size 33;
    Kes_signature_bad_size { expected = 448; actual = 100 };
    Byron_skip;
  ] in
  List.iter (fun e ->
    let s = Block_validator.error_to_string e in
    Alcotest.(check bool) "non-empty" true (String.length s > 0)
  ) errs

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Block-Validator"
    [ ( "Opcert signature",
        [ Alcotest.test_case "valid" `Quick test_opcert_valid;
          Alcotest.test_case "bad signature" `Quick test_opcert_bad_sig;
          Alcotest.test_case "wrong key" `Quick test_opcert_wrong_key ] );
      ( "Size checks",
        [ Alcotest.test_case "KES sig size" `Quick test_kes_sig_size;
          Alcotest.test_case "VRF vkey size" `Quick test_vrf_vkey_size;
          Alcotest.test_case "cold key size" `Quick test_cold_key_size ] );
      ( "Full validation",
        [ Alcotest.test_case "valid shelley" `Quick test_validate_shelley_block_valid;
          Alcotest.test_case "bad opcert" `Quick test_validate_block_bad_opcert;
          Alcotest.test_case "byron skipped" `Quick test_validate_byron_skipped ] );
      ( "Error strings",
        [ Alcotest.test_case "all formats" `Quick test_error_strings ] );
    ]
