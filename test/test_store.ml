open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let bytes_testable =
  Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

let temp_dir () =
  let dir = Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-test-%d-%d" (Unix.getpid ()) (Random.int 100000)) in
  dir

let rm_rf dir =
  let rec go path =
    if Sys.is_directory path then begin
      Array.iter (fun f -> go (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
    end else
      Unix.unlink path
  in
  if Sys.file_exists dir then go dir

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF);
  Bytes.set_uint8 b 1 ((n lsr 8) land 0xFF);
  b

let make_block n =
  Bytes.of_string (Printf.sprintf "block-data-%d" n)

(* ================================================================ *)
(* Store and retrieve by hash                                        *)
(* ================================================================ *)

let test_store_retrieve_by_hash () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let hash = make_hash 1 in
  let data = make_block 1 in
  (match Store.store_block store ~slot:100L ~hash ~cbor_bytes:data with
   | Ok () -> ()
   | Error e -> Alcotest.fail e);
  (match Store.get_block store ~hash with
   | Some retrieved -> Alcotest.check bytes_testable "data matches" data retrieved
   | None -> Alcotest.fail "block not found");
  rm_rf dir

(* ================================================================ *)
(* Store and retrieve by slot                                        *)
(* ================================================================ *)

let test_store_retrieve_by_slot () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let hash = make_hash 42 in
  let data = make_block 42 in
  ignore (Store.store_block store ~slot:500L ~hash ~cbor_bytes:data);
  (match Store.get_block_by_slot store ~slot:500L with
   | Some retrieved -> Alcotest.check bytes_testable "data matches" data retrieved
   | None -> Alcotest.fail "block not found by slot");
  (* Non-existent slot *)
  Alcotest.(check bool) "missing slot" true
    (Store.get_block_by_slot store ~slot:999L = None);
  rm_rf dir

(* ================================================================ *)
(* Chain index ordering and tip                                      *)
(* ================================================================ *)

let test_tip_ordering () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  (* Store blocks in non-sequential order *)
  ignore (Store.store_block store ~slot:300L ~hash:(make_hash 3) ~cbor_bytes:(make_block 3));
  ignore (Store.store_block store ~slot:100L ~hash:(make_hash 1) ~cbor_bytes:(make_block 1));
  ignore (Store.store_block store ~slot:500L ~hash:(make_hash 5) ~cbor_bytes:(make_block 5));
  ignore (Store.store_block store ~slot:200L ~hash:(make_hash 2) ~cbor_bytes:(make_block 2));
  (* Tip should be the last stored block (append-only) *)
  (match Store.tip store with
   | Some (slot, _hash) ->
     Alcotest.(check int64) "tip slot" 200L slot
   | None -> Alcotest.fail "expected tip");
  Alcotest.(check int) "block count" 4 (Store.block_count store);
  rm_rf dir

(* ================================================================ *)
(* Crash safety: no temp files left behind                           *)
(* ================================================================ *)

let test_crash_safety () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let hash = make_hash 99 in
  ignore (Store.store_block store ~slot:100L ~hash ~cbor_bytes:(make_block 99));
  (* Check that no .tmp files exist *)
  let hex = hex_of_bytes hash in
  let prefix = String.sub hex 0 2 in
  let block_dir = Filename.concat (Filename.concat dir "blocks") prefix in
  let files = Sys.readdir block_dir in
  let tmp_files = Array.to_list files |> List.filter (fun f ->
    Filename.check_suffix f ".tmp") in
  Alcotest.(check int) "no tmp files" 0 (List.length tmp_files);
  (* Final file exists *)
  let block_file = Filename.concat block_dir (hex ^ ".block") in
  Alcotest.(check bool) "block file exists" true (Sys.file_exists block_file);
  (* Index .tmp should not exist *)
  Alcotest.(check bool) "no index tmp"
    false (Sys.file_exists (Filename.concat dir "chain.index.tmp"));
  rm_rf dir

(* ================================================================ *)
(* has_block                                                         *)
(* ================================================================ *)

let test_has_block () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let hash = make_hash 7 in
  Alcotest.(check bool) "not stored yet" false (Store.has_block store ~hash);
  ignore (Store.store_block store ~slot:100L ~hash ~cbor_bytes:(make_block 7));
  Alcotest.(check bool) "now stored" true (Store.has_block store ~hash);
  rm_rf dir

(* ================================================================ *)
(* Idempotent store                                                  *)
(* ================================================================ *)

let test_idempotent () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let hash = make_hash 1 in
  let data = make_block 1 in
  ignore (Store.store_block store ~slot:100L ~hash ~cbor_bytes:data);
  ignore (Store.store_block store ~slot:100L ~hash ~cbor_bytes:data);
  Alcotest.(check int) "stored once" 1 (Store.block_count store);
  rm_rf dir

(* ================================================================ *)
(* LRU cache                                                         *)
(* ================================================================ *)

let test_recent_blocks () =
  let dir = temp_dir () in
  let store = Store.init ~cache_max:50 ~base_dir:dir () in
  for i = 1 to 100 do
    let hash = make_hash i in
    ignore (Store.store_block store ~slot:(Int64.of_int (i * 10)) ~hash
              ~cbor_bytes:(make_block i))
  done;
  let recent = Store.get_recent_blocks store ~count:5 in
  Alcotest.(check int) "5 recent" 5 (List.length recent);
  (* Last stored should be slot 1000 *)
  let (last_slot, _) = List.nth recent 4 in
  Alcotest.(check int64) "last slot" 1000L last_slot;
  (* First of the 5 should be slot 960 *)
  let (first_slot, _) = List.hd recent in
  Alcotest.(check int64) "first slot" 960L first_slot;
  rm_rf dir

(* ================================================================ *)
(* get_chain_points                                                  *)
(* ================================================================ *)

let test_chain_points () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  for i = 1 to 3000 do
    let hash = make_hash i in
    ignore (Store.store_block store ~slot:(Int64.of_int (i * 20)) ~hash
              ~cbor_bytes:(make_block i))
  done;
  let points = Store.get_chain_points store in
  (* Should include: tip, tip-10, tip-100, tip-1000, tip-2160, Origin *)
  let last_is_origin = match List.rev points with
    | Chain_sync.Origin :: _ -> true | _ -> false
  in
  Alcotest.(check bool) "ends with origin" true last_is_origin;
  Alcotest.(check bool) "at least 5 points" true (List.length points >= 5);
  (* First point should be the tip *)
  (match List.hd points with
   | Chain_sync.Point (slot, _) ->
     Alcotest.(check int64) "tip slot" 60000L slot
   | _ -> Alcotest.fail "expected Point");
  rm_rf dir

let test_chain_points_empty () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let points = Store.get_chain_points store in
  Alcotest.(check int) "just origin" 1 (List.length points);
  (match points with
   | [Chain_sync.Origin] -> ()
   | _ -> Alcotest.fail "expected [Origin]");
  rm_rf dir

(* ================================================================ *)
(* Resumable index: close and re-open                                *)
(* ================================================================ *)

let test_resumable () =
  let dir = temp_dir () in
  (* First session: store some blocks *)
  let store1 = Store.init ~base_dir:dir () in
  for i = 1 to 10 do
    ignore (Store.store_block store1 ~slot:(Int64.of_int (i * 100))
              ~hash:(make_hash i) ~cbor_bytes:(make_block i))
  done;
  Alcotest.(check int) "session 1 count" 10 (Store.block_count store1);
  let tip1 = Store.tip store1 in
  (* "Close" by just dropping the reference — the files are on disk *)
  (* Second session: re-init from same directory *)
  let store2 = Store.init ~base_dir:dir () in
  Alcotest.(check int) "session 2 count" 10 (Store.block_count store2);
  let tip2 = Store.tip store2 in
  (match tip1, tip2 with
   | Some (s1, h1), Some (s2, h2) ->
     Alcotest.(check int64) "same tip slot" s1 s2;
     Alcotest.check bytes_testable "same tip hash" h1 h2
   | _ -> Alcotest.fail "expected tips");
  (* Verify blocks are accessible *)
  for i = 1 to 10 do
    let hash = make_hash i in
    match Store.get_block store2 ~hash with
    | Some data -> Alcotest.check bytes_testable
        (Printf.sprintf "block %d" i) (make_block i) data
    | None -> Alcotest.fail (Printf.sprintf "block %d not found" i)
  done;
  (* Store more blocks in session 2 *)
  for i = 11 to 15 do
    ignore (Store.store_block store2 ~slot:(Int64.of_int (i * 100))
              ~hash:(make_hash i) ~cbor_bytes:(make_block i))
  done;
  Alcotest.(check int) "session 2 updated count" 15 (Store.block_count store2);
  rm_rf dir

(* ================================================================ *)
(* Empty store                                                       *)
(* ================================================================ *)

let test_empty_store () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  Alcotest.(check bool) "tip is none" true (Store.tip store = None);
  Alcotest.(check bool) "get_block none" true
    (Store.get_block store ~hash:(make_hash 1) = None);
  Alcotest.(check int) "block count 0" 0 (Store.block_count store);
  Alcotest.(check int) "recent empty" 0
    (List.length (Store.get_recent_blocks store ~count:10));
  rm_rf dir

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Store"
    [ ( "Store/retrieve",
        [ Alcotest.test_case "by hash" `Quick test_store_retrieve_by_hash;
          Alcotest.test_case "by slot" `Quick test_store_retrieve_by_slot;
          Alcotest.test_case "has_block" `Quick test_has_block;
          Alcotest.test_case "idempotent" `Quick test_idempotent ] );
      ( "Chain index",
        [ Alcotest.test_case "tip ordering" `Quick test_tip_ordering;
          Alcotest.test_case "chain points" `Quick test_chain_points;
          Alcotest.test_case "chain points empty" `Quick test_chain_points_empty ] );
      ( "Crash safety",
        [ Alcotest.test_case "no tmp files" `Quick test_crash_safety ] );
      ( "LRU cache",
        [ Alcotest.test_case "recent blocks" `Quick test_recent_blocks ] );
      ( "Resumable",
        [ Alcotest.test_case "close and reopen" `Quick test_resumable ] );
      ( "Empty store",
        [ Alcotest.test_case "all empty" `Quick test_empty_store ] );
    ]
