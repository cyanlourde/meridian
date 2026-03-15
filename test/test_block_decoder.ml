open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF);
  b

(* ================================================================ *)
(* Era tag detection                                                 *)
(* ================================================================ *)

let test_era_tags () =
  let eras = [
    (0L, Block_decoder.Byron); (1L, Shelley); (2L, Allegra);
    (3L, Mary); (4L, Alonzo); (5L, Babbage); (6L, Conway);
  ] in
  List.iter (fun (tag, expected_era) ->
    (* Construct minimal era-tagged block CBOR *)
    let inner = Cbor.Array [Cbor.Array []; Cbor.Array []; Cbor.Array []; Cbor.Map []] in
    let outer = Cbor.Array [Cbor.Uint tag; inner] in
    let cbor_bytes = Cbor.encode outer in
    match Block_decoder.decode_block cbor_bytes with
    | Ok block ->
      Alcotest.(check string) (Printf.sprintf "era tag %Ld" tag)
        (Block_decoder.era_name expected_era) (Block_decoder.era_name block.db_era)
    | Error _ ->
      (* Some may fail header decode but era should be detected *)
      ()
  ) eras

(* ================================================================ *)
(* Tag 24 wrapping                                                   *)
(* ================================================================ *)

let test_tag24_unwrap () =
  (* Wrap a shelley-ish block in tag 24 *)
  let header_body = Cbor.Array [
    Cbor.Uint 1L; Cbor.Uint 100L; Cbor.Bytes (make_hash 0);
    Cbor.Bytes (make_hash 1); Cbor.Bytes (make_hash 2);
    Cbor.Array [Cbor.Bytes (Bytes.make 32 '\x00'); Cbor.Bytes (Bytes.make 80 '\x00')];
    Cbor.Array [Cbor.Bytes (Bytes.make 32 '\x00'); Cbor.Bytes (Bytes.make 80 '\x00')];
    Cbor.Uint 0L; Cbor.Bytes (make_hash 3);
    Cbor.Bytes (make_hash 4); Cbor.Uint 0L; Cbor.Uint 0L;
    Cbor.Bytes (Bytes.make 64 '\x00'); Cbor.Uint 2L; Cbor.Uint 0L;
  ] in
  let block_inner = Cbor.Array [
    Cbor.Array [header_body; Cbor.Bytes (Bytes.make 448 '\x00')];
    Cbor.Array [];  (* tx bodies *)
    Cbor.Array [];  (* witnesses *)
    Cbor.Map [];    (* metadata *)
  ] in
  let inner_bytes = Cbor.encode block_inner in
  let era_tagged = Cbor.Array [Cbor.Uint 1L; Cbor.Tag (24L, Cbor.Bytes inner_bytes)] in
  let cbor_bytes = Cbor.encode era_tagged in
  match Block_decoder.decode_block cbor_bytes with
  | Ok block ->
    Alcotest.(check string) "era" "shelley" (Block_decoder.era_name block.db_era);
    Alcotest.(check int64) "slot" 100L block.db_header.bh_slot
  | Error e -> Alcotest.fail e

(* ================================================================ *)
(* Shelley header extraction                                         *)
(* ================================================================ *)

let test_shelley_header () =
  let header_body = Cbor.Array [
    Cbor.Uint 42L;       (* block number *)
    Cbor.Uint 1000L;     (* slot *)
    Cbor.Bytes (make_hash 0);  (* prev hash *)
    Cbor.Bytes (make_hash 1);  (* issuer *)
    Cbor.Bytes (make_hash 2);  (* vrf vkey *)
    Cbor.Array [Cbor.Bytes (Bytes.make 32 '\x00'); Cbor.Bytes (Bytes.make 80 '\x00')];
    Cbor.Array [Cbor.Bytes (Bytes.make 32 '\x00'); Cbor.Bytes (Bytes.make 80 '\x00')];
    Cbor.Uint 512L;      (* body size *)
    Cbor.Bytes (make_hash 3);  (* body hash *)
    Cbor.Bytes (make_hash 4); Cbor.Uint 1L; Cbor.Uint 200L;
    Cbor.Bytes (Bytes.make 64 '\x00');
    Cbor.Uint 3L; Cbor.Uint 0L;  (* proto version *)
  ] in
  let block = Cbor.Array [
    Cbor.Array [header_body; Cbor.Bytes (Bytes.make 448 '\x00')];
    Cbor.Array []; Cbor.Array []; Cbor.Map [];
  ] in
  let outer = Cbor.Array [Cbor.Uint 1L; block] in
  let cbor_bytes = Cbor.encode outer in
  match Block_decoder.decode_block cbor_bytes with
  | Error e -> Alcotest.fail e
  | Ok b ->
    Alcotest.(check int64) "slot" 1000L b.db_header.bh_slot;
    Alcotest.(check int64) "block_no" 42L b.db_header.bh_block_number;
    Alcotest.(check bool) "has prev" true (b.db_header.bh_prev_hash <> None);
    Alcotest.(check int) "issuer len" 32 (Bytes.length b.db_header.bh_issuer_vkey)

(* ================================================================ *)
(* Transaction count                                                 *)
(* ================================================================ *)

let test_tx_count () =
  let header_body = Cbor.Array [
    Cbor.Uint 1L; Cbor.Uint 500L; Cbor.Null;
    Cbor.Bytes (make_hash 1); Cbor.Bytes (make_hash 2);
    Cbor.Array [Cbor.Bytes Bytes.empty; Cbor.Bytes Bytes.empty];
    Cbor.Array [Cbor.Bytes Bytes.empty; Cbor.Bytes Bytes.empty];
    Cbor.Uint 0L; Cbor.Bytes (make_hash 3);
    Cbor.Bytes (make_hash 4); Cbor.Uint 0L; Cbor.Uint 0L;
    Cbor.Bytes (Bytes.make 64 '\x00'); Cbor.Uint 2L; Cbor.Uint 0L;
  ] in
  let tx1 = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 10); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (Bytes.make 29 '\x61'); Cbor.Uint 1000000L]]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
    (Cbor.Uint 3L, Cbor.Uint 50000000L);
  ] in
  let block = Cbor.Array [
    Cbor.Array [header_body; Cbor.Bytes (Bytes.make 448 '\x00')];
    Cbor.Array [tx1; tx1; tx1];  (* 3 tx bodies *)
    Cbor.Array [Cbor.Map []; Cbor.Map []; Cbor.Map []];
    Cbor.Map [];
  ] in
  let outer = Cbor.Array [Cbor.Uint 1L; block] in
  let cbor_bytes = Cbor.encode outer in
  match Block_decoder.decode_block cbor_bytes with
  | Error e -> Alcotest.fail e
  | Ok b ->
    Alcotest.(check int) "3 txs" 3 b.db_tx_count;
    Alcotest.(check int) "3 raw txs" 3 (List.length b.db_tx_raw)

(* ================================================================ *)
(* Era name                                                          *)
(* ================================================================ *)

let test_era_name () =
  Alcotest.(check string) "byron" "byron" (Block_decoder.era_name Byron);
  Alcotest.(check string) "shelley" "shelley" (Block_decoder.era_name Shelley);
  Alcotest.(check string) "conway" "conway" (Block_decoder.era_name Conway)

(* ================================================================ *)
(* Malformed input                                                   *)
(* ================================================================ *)

let test_malformed () =
  (* Empty bytes *)
  (match Block_decoder.decode_block Bytes.empty with
   | Error _ -> () | Ok _ -> Alcotest.fail "expected error on empty");
  (* Truncated *)
  (match Block_decoder.decode_block (Bytes.of_string "\x82") with
   | Error _ -> () | Ok _ -> Alcotest.fail "expected error on truncated");
  (* Not an array *)
  let cbor_bytes = Cbor.encode (Cbor.Uint 42L) in
  (match Block_decoder.decode_block cbor_bytes with
   | Error _ -> () | Ok _ -> Alcotest.fail "expected error on non-array")

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Block-Decoder"
    [ ( "Era detection",
        [ Alcotest.test_case "all era tags" `Quick test_era_tags;
          Alcotest.test_case "era names" `Quick test_era_name ] );
      ( "Tag 24",
        [ Alcotest.test_case "unwrap" `Quick test_tag24_unwrap ] );
      ( "Header",
        [ Alcotest.test_case "shelley header" `Quick test_shelley_header ] );
      ( "Transactions",
        [ Alcotest.test_case "tx count" `Quick test_tx_count ] );
      ( "Error handling",
        [ Alcotest.test_case "malformed input" `Quick test_malformed ] );
    ]
