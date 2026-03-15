open Meridian

let make_header ?(slot = 100L) ?(block_number = 10L) ?(era = Block_decoder.Shelley)
    ?(prev_hash = Some (Bytes.make 32 '\xaa'))
    ?(body_hash = Bytes.make 32 '\xbb')
    ?(proto = (2L, 0L)) () =
  Block_decoder.{
    bh_slot = slot; bh_block_number = block_number;
    bh_prev_hash = prev_hash; bh_issuer_vkey = Bytes.make 32 '\xcc';
    bh_body_hash = body_hash; bh_protocol_version = proto; bh_era = era;
    bh_vrf_vkey = Bytes.empty; bh_block_signature = Bytes.empty;
    bh_opcert = None; bh_header_body_cbor = Bytes.empty;
  }

(* ---- Slot number ---- *)

let test_valid_slot () =
  match Header_validation.validate_slot_number ~prev_slot:50L ~slot:100L with
  | Ok () -> () | Error e -> Alcotest.fail e

let test_invalid_slot_equal () =
  match Header_validation.validate_slot_number ~prev_slot:100L ~slot:100L with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

let test_invalid_slot_backward () =
  match Header_validation.validate_slot_number ~prev_slot:100L ~slot:50L with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

(* ---- Prev hash ---- *)

let test_valid_prev_hash () =
  let expected = Bytes.make 32 '\xaa' in
  let header = make_header ~prev_hash:(Some expected) () in
  match Header_validation.validate_prev_hash ~expected_hash:expected ~header with
  | Ok () -> () | Error e -> Alcotest.fail e

let test_invalid_prev_hash () =
  let expected = Bytes.make 32 '\xaa' in
  let header = make_header ~prev_hash:(Some (Bytes.make 32 '\xbb')) () in
  match Header_validation.validate_prev_hash ~expected_hash:expected ~header with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

(* ---- Block number ---- *)

let test_valid_block_number () =
  let header = make_header ~block_number:11L () in
  match Header_validation.validate_block_number ~prev_block_number:10L ~header with
  | Ok () -> () | Error e -> Alcotest.fail e

let test_invalid_block_number_skip () =
  let header = make_header ~block_number:12L () in
  match Header_validation.validate_block_number ~prev_block_number:10L ~header with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

let test_invalid_block_number_same () =
  let header = make_header ~block_number:10L () in
  match Header_validation.validate_block_number ~prev_block_number:10L ~header with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

(* ---- Block body hash ---- *)

let test_valid_body_hash () =
  let body = Bytes.of_string "test body data" in
  let hash = Crypto.blake2b_256 body in
  let header = make_header ~body_hash:hash () in
  match Header_validation.validate_block_body_hash ~header ~raw_body_cbor:body with
  | Ok () -> () | Error e -> Alcotest.fail e

let test_invalid_body_hash () =
  let body = Bytes.of_string "test body data" in
  let header = make_header ~body_hash:(Bytes.make 32 '\xff') () in
  match Header_validation.validate_block_body_hash ~header ~raw_body_cbor:body with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

(* ---- Header size ---- *)

let test_header_size_ok () =
  let hdr = Bytes.make 500 '\x00' in
  match Header_validation.validate_header_size ~raw_header_cbor:hdr ~max_size:1100 with
  | Ok () -> () | Error e -> Alcotest.fail e

let test_header_size_too_big () =
  let hdr = Bytes.make 1200 '\x00' in
  match Header_validation.validate_header_size ~raw_header_cbor:hdr ~max_size:1100 with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

(* ---- Block size per era ---- *)

let test_block_size_ok () =
  let block = Bytes.make 60000 '\x00' in
  match Header_validation.validate_block_size ~raw_block_cbor:block ~era:Shelley with
  | Ok () -> () | Error e -> Alcotest.fail e

let test_block_size_over () =
  let block = Bytes.make 70000 '\x00' in
  match Header_validation.validate_block_size ~raw_block_cbor:block ~era:Shelley with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

let test_block_size_babbage () =
  let block = Bytes.make 80000 '\x00' in
  match Header_validation.validate_block_size ~raw_block_cbor:block ~era:Babbage with
  | Ok () -> () | Error e -> Alcotest.fail e

(* ---- Protocol version ---- *)

let test_proto_shelley_ok () =
  let header = make_header ~proto:(2L, 0L) ~era:Shelley () in
  match Header_validation.validate_protocol_version ~header with
  | Ok () -> () | Error e -> Alcotest.fail e

let test_proto_shelley_bad () =
  let header = make_header ~proto:(9L, 0L) ~era:Shelley () in
  match Header_validation.validate_protocol_version ~header with
  | Error _ -> () | Ok () -> Alcotest.fail "expected error"

(* ---- Combined validate_header ---- *)

let test_validate_header_all_pass () =
  let body = Bytes.of_string "body" in
  let body_hash = Crypto.blake2b_256 body in
  let prev_hash = Bytes.make 32 '\xaa' in
  let header = make_header ~slot:200L ~block_number:11L
    ~prev_hash:(Some prev_hash) ~body_hash ~proto:(2L, 0L) () in
  let raw_hdr = Bytes.make 500 '\x00' in
  match Header_validation.validate_header
    ~prev_slot:100L ~prev_block_number:10L ~prev_hash
    ~header ~raw_header_cbor:raw_hdr ~raw_body_cbor:body with
  | Ok () -> ()
  | Error errs -> Alcotest.fail (String.concat "; " errs)

let test_validate_header_multiple_errors () =
  let header = make_header ~slot:50L ~block_number:15L
    ~prev_hash:(Some (Bytes.make 32 '\xff')) ~proto:(2L, 0L) () in
  let raw_hdr = Bytes.make 500 '\x00' in
  let body = Bytes.of_string "body" in
  match Header_validation.validate_header
    ~prev_slot:100L ~prev_block_number:10L
    ~prev_hash:(Bytes.make 32 '\xaa')
    ~header ~raw_header_cbor:raw_hdr ~raw_body_cbor:body with
  | Error errs ->
    Alcotest.(check bool) "multiple errors" true (List.length errs >= 2)
  | Ok () -> Alcotest.fail "expected errors"

let () =
  Alcotest.run "Header-Validation"
    [ ( "Slot number",
        [ Alcotest.test_case "valid" `Quick test_valid_slot;
          Alcotest.test_case "equal" `Quick test_invalid_slot_equal;
          Alcotest.test_case "backward" `Quick test_invalid_slot_backward ] );
      ( "Prev hash",
        [ Alcotest.test_case "match" `Quick test_valid_prev_hash;
          Alcotest.test_case "mismatch" `Quick test_invalid_prev_hash ] );
      ( "Block number",
        [ Alcotest.test_case "N+1" `Quick test_valid_block_number;
          Alcotest.test_case "N+2 skip" `Quick test_invalid_block_number_skip;
          Alcotest.test_case "same" `Quick test_invalid_block_number_same ] );
      ( "Body hash",
        [ Alcotest.test_case "valid" `Quick test_valid_body_hash;
          Alcotest.test_case "mismatch" `Quick test_invalid_body_hash ] );
      ( "Header size",
        [ Alcotest.test_case "within limit" `Quick test_header_size_ok;
          Alcotest.test_case "over limit" `Quick test_header_size_too_big ] );
      ( "Block size",
        [ Alcotest.test_case "shelley ok" `Quick test_block_size_ok;
          Alcotest.test_case "shelley over" `Quick test_block_size_over;
          Alcotest.test_case "babbage ok" `Quick test_block_size_babbage ] );
      ( "Protocol version",
        [ Alcotest.test_case "shelley ok" `Quick test_proto_shelley_ok;
          Alcotest.test_case "shelley bad" `Quick test_proto_shelley_bad ] );
      ( "Combined",
        [ Alcotest.test_case "all pass" `Quick test_validate_header_all_pass;
          Alcotest.test_case "multiple errors" `Quick test_validate_header_multiple_errors ] );
    ]
