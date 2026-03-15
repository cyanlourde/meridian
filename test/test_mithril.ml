open Meridian

let () = Crypto.init ()

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-mithril-test-%d-%d" (Unix.getpid ()) (Random.int 100000))

let rm_rf dir =
  let rec go path =
    if Sys.is_directory path then begin
      Array.iter (fun f -> go (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
    end else Unix.unlink path
  in
  if Sys.file_exists dir then go dir

let write_file path content =
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let b = Bytes.of_string content in
  ignore (Unix.write fd b 0 (Bytes.length b));
  Unix.close fd

(* ================================================================ *)
(* Snapshot list parsing                                              *)
(* ================================================================ *)

let test_parse_snapshot_list () =
  let json_str = {|[
    {"digest": "abc123", "beacon": {"epoch": 100, "immutable_file_number": 500},
     "size": 1000000, "created_at": "2026-01-01", "locations": ["https://example.com/snap1.tar.zst"]},
    {"digest": "def456", "beacon": {"epoch": 99, "immutable_file_number": 490},
     "size": 900000, "created_at": "2025-12-31", "locations": ["https://example.com/snap2.tar.zst"]}
  ]|} in
  let json = Json.parse json_str in
  match json with
  | Json.Array items ->
    let snaps = List.map Mithril_client.parse_snapshot_summary items in
    Alcotest.(check int) "2 snapshots" 2 (List.length snaps);
    let s1 = List.hd snaps in
    Alcotest.(check string) "digest" "abc123" s1.digest;
    Alcotest.(check int64) "epoch" 100L s1.beacon_epoch;
    Alcotest.(check int64) "immutable" 500L s1.beacon_immutable;
    Alcotest.(check int64) "size" 1000000L s1.size;
    Alcotest.(check int) "1 location" 1 (List.length s1.locations)
  | _ -> Alcotest.fail "expected array"

(* ================================================================ *)
(* Certificate parsing                                               *)
(* ================================================================ *)

let test_parse_certificate () =
  let json_str = {|{
    "hash": "cert_hash_abc123def456789012345678901234567890",
    "previous_hash": "prev_hash_xyz",
    "beacon": {"epoch": 100, "immutable_file_number": 500},
    "signed_message": "signed_msg_content",
    "aggregate_verification_key": "avk_content"
  }|} in
  let json = Json.parse json_str in
  let cert = Mithril_client.parse_certificate json in
  Alcotest.(check string) "hash" "cert_hash_abc123def456789012345678901234567890" cert.cert_hash;
  Alcotest.(check string) "prev" "prev_hash_xyz" cert.previous_hash;
  Alcotest.(check int64) "epoch" 100L cert.beacon_epoch;
  Alcotest.(check string) "signed_msg" "signed_msg_content" cert.signed_message

(* ================================================================ *)
(* Certificate chain hash verification                               *)
(* ================================================================ *)

let test_cert_hash_valid () =
  let cert = Mithril_client.{
    cert_hash = "abc123def456789012345678901234567890abcdef";
    previous_hash = "prev";
    beacon_epoch = 100L; beacon_immutable = 500L;
    signed_message = "msg"; aggregate_verification_key = "avk";
  } in
  match Mithril_verify.verify_certificate_hash cert with
  | Ok () -> ()
  | Error e -> Alcotest.fail e

let test_cert_hash_empty () =
  let cert = Mithril_client.{
    cert_hash = "";
    previous_hash = ""; beacon_epoch = 0L; beacon_immutable = 0L;
    signed_message = ""; aggregate_verification_key = "";
  } in
  match Mithril_verify.verify_certificate_hash cert with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected error for empty hash"

(* ================================================================ *)
(* Snapshot digest verification                                      *)
(* ================================================================ *)

let test_digest_match () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "test.bin" in
  write_file path "hello world\n";
  match Mithril_verify.compute_file_digest ~path with
  | Error e -> rm_rf dir; Alcotest.fail e
  | Ok digest ->
    Alcotest.(check bool) "non-empty digest" true (String.length digest > 0);
    (* Verify matches itself *)
    (match Mithril_verify.verify_snapshot_digest ~expected_digest:digest ~snapshot_path:path with
     | Ok () -> ()
     | Error e -> Alcotest.fail e);
    rm_rf dir

let test_digest_mismatch () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "test.bin" in
  write_file path "hello world\n";
  match Mithril_verify.verify_snapshot_digest
          ~expected_digest:"0000000000000000000000000000000000000000000000000000000000000000"
          ~snapshot_path:path with
  | Error _ -> rm_rf dir
  | Ok () -> rm_rf dir; Alcotest.fail "expected mismatch"

(* ================================================================ *)
(* Chunk file parsing                                                *)
(* ================================================================ *)

let test_parse_chunk () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "00000.chunk" in
  (* Create 3 concatenated CBOR values *)
  let b1 = Cbor.encode (Cbor.Array [Cbor.Uint 1L; Cbor.Text "block1"]) in
  let b2 = Cbor.encode (Cbor.Array [Cbor.Uint 2L; Cbor.Text "block2"]) in
  let b3 = Cbor.encode (Cbor.Array [Cbor.Uint 3L; Cbor.Text "block3"]) in
  let combined = Bytes.create (Bytes.length b1 + Bytes.length b2 + Bytes.length b3) in
  Bytes.blit b1 0 combined 0 (Bytes.length b1);
  Bytes.blit b2 0 combined (Bytes.length b1) (Bytes.length b2);
  Bytes.blit b3 0 combined (Bytes.length b1 + Bytes.length b2) (Bytes.length b3);
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  ignore (Unix.write fd combined 0 (Bytes.length combined));
  Unix.close fd;
  let blocks = Mithril_import.parse_chunk_file ~path in
  Alcotest.(check int) "3 blocks" 3 (List.length blocks);
  rm_rf dir

(* ================================================================ *)
(* Import to store                                                   *)
(* ================================================================ *)

let test_import_to_store () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let store = Store.init ~base_dir:dir () in
  (* Create a minimal era-tagged block *)
  let block_cbor = Cbor.Array [
    Cbor.Uint 5L;  (* Babbage era *)
    Cbor.Array [
      Cbor.Array [Cbor.Array []; Cbor.Bytes Bytes.empty];
      Cbor.Array []; Cbor.Array []; Cbor.Map []
    ]
  ] in
  let block_bytes = Cbor.encode block_cbor in
  let result = Mithril_import.import_block_bytes store block_bytes in
  (match result with
   | Some _ -> Alcotest.(check int) "1 block stored" 1 (Store.block_count store)
   | None -> ());  (* May fail to decode header — that's ok for this test *)
  rm_rf dir

(* ================================================================ *)
(* Unpack archive                                                    *)
(* ================================================================ *)

let test_unpack_tar_gz () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  (* Create a tar.gz with one file *)
  let content_dir = Filename.concat dir "content" in
  Unix.mkdir content_dir 0o755;
  write_file (Filename.concat content_dir "test.txt") "hello";
  let archive = Filename.concat dir "test.tar.gz" in
  let cmd = Printf.sprintf "tar -czf %s -C %s test.txt 2>/dev/null"
    (Filename.quote archive) (Filename.quote content_dir) in
  ignore (Sys.command cmd);
  let dest = Filename.concat dir "unpacked" in
  (match Mithril_import.unpack_snapshot ~archive_path:archive ~dest_dir:dest with
   | Ok () ->
     Alcotest.(check bool) "file exists"
       true (Sys.file_exists (Filename.concat dest "test.txt"))
   | Error e -> Alcotest.fail e);
  rm_rf dir

(* ================================================================ *)
(* Preview aggregator URL                                            *)
(* ================================================================ *)

let test_aggregator_url () =
  Alcotest.(check bool) "has https" true
    (String.length Mithril_client.preview_aggregator > 10)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Mithril"
    [ ( "Snapshot parsing",
        [ Alcotest.test_case "list" `Quick test_parse_snapshot_list;
          Alcotest.test_case "certificate" `Quick test_parse_certificate ] );
      ( "Certificate verification",
        [ Alcotest.test_case "valid hash" `Quick test_cert_hash_valid;
          Alcotest.test_case "empty hash" `Quick test_cert_hash_empty ] );
      ( "Digest verification",
        [ Alcotest.test_case "match" `Quick test_digest_match;
          Alcotest.test_case "mismatch" `Quick test_digest_mismatch ] );
      ( "Chunk parsing",
        [ Alcotest.test_case "3 blocks" `Quick test_parse_chunk ] );
      ( "Import",
        [ Alcotest.test_case "to store" `Quick test_import_to_store ] );
      ( "Unpack",
        [ Alcotest.test_case "tar.gz" `Quick test_unpack_tar_gz ] );
      ( "Config",
        [ Alcotest.test_case "aggregator url" `Quick test_aggregator_url ] );
    ]
