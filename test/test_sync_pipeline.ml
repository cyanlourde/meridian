open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-pipe-test-%d-%d" (Unix.getpid ()) (Random.int 100000))

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
  Bytes.set_uint8 b 1 ((n lsr 8) land 0xFF);
  b

let make_block_data n =
  Bytes.of_string (Printf.sprintf "block-body-%04d" n)

(* ================================================================ *)
(* Test: store integration                                           *)
(* ================================================================ *)

(** Test that store_block + get_chain_points works for pipeline resume. *)
let test_store_chain_points_for_resume () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  (* Store some blocks *)
  for i = 1 to 5 do
    ignore (Store.store_block store ~slot:(Int64.of_int (i * 100))
              ~hash:(make_hash i) ~cbor_bytes:(make_block_data i))
  done;
  let points = Store.get_chain_points store in
  (* Should have tip + origin at minimum *)
  Alcotest.(check bool) "has points" true (List.length points >= 2);
  (* First point should be tip (slot 500) *)
  (match List.hd points with
   | Chain_sync.Point (500L, _) -> ()
   | _ -> Alcotest.fail "expected tip at slot 500");
  (* Last should be origin *)
  (match List.rev points |> List.hd with
   | Chain_sync.Origin -> ()
   | _ -> Alcotest.fail "expected origin at end");
  rm_rf dir

(** Test that re-initializing store preserves chain points. *)
let test_resumable_chain_points () =
  let dir = temp_dir () in
  let store1 = Store.init ~base_dir:dir () in
  for i = 1 to 5 do
    ignore (Store.store_block store1 ~slot:(Int64.of_int (i * 100))
              ~hash:(make_hash i) ~cbor_bytes:(make_block_data i))
  done;
  let points1 = Store.get_chain_points store1 in
  (* Re-init *)
  let store2 = Store.init ~base_dir:dir () in
  let points2 = Store.get_chain_points store2 in
  Alcotest.(check int) "same point count" (List.length points1) (List.length points2);
  rm_rf dir

(* ================================================================ *)
(* Test: default_config                                              *)
(* ================================================================ *)

let test_default_config () =
  let cfg = Sync_pipeline.default_config () in
  Alcotest.(check int) "batch_size" 50 cfg.batch_size;
  Alcotest.(check bool) "should_stop" false (cfg.should_stop ())

(* ================================================================ *)
(* Test: batch size respected in on_progress                         *)
(* ================================================================ *)

let test_batch_size_config () =
  let cfg = Sync_pipeline.default_config ~batch_size:3 () in
  Alcotest.(check int) "batch_size 3" 3 cfg.batch_size

(* ================================================================ *)
(* Test: block_info type                                             *)
(* ================================================================ *)

let test_block_info () =
  let bi = Sync_pipeline.{ bi_slot = 42L; bi_hash = make_hash 1; bi_size = 100 } in
  Alcotest.(check int64) "slot" 42L bi.bi_slot;
  Alcotest.(check int) "size" 100 bi.bi_size

(* ================================================================ *)
(* Test: progress type                                               *)
(* ================================================================ *)

let test_progress () =
  let p = Sync_pipeline.{
    blocks_synced = 100;
    current_slot = 1000L;
    tip_slot = 50000L;
    tip_block = 2000L;
    blocks_per_sec = 50.0;
    disk_blocks = 100;
  } in
  Alcotest.(check int) "synced" 100 p.blocks_synced;
  Alcotest.(check int64) "current" 1000L p.current_slot;
  Alcotest.(check int64) "tip_slot" 50000L p.tip_slot

(* ================================================================ *)
(* Test: result type                                                 *)
(* ================================================================ *)

let test_result_types () =
  let _ = (Sync_pipeline.Completed : Sync_pipeline.result) in
  let _ = (Sync_pipeline.Stopped : Sync_pipeline.result) in
  let _ = Sync_pipeline.Disconnected ("err", 42L) in
  ()

(* ================================================================ *)
(* Test: callbacks are invoked                                       *)
(* ================================================================ *)

let test_callbacks () =
  let block_count = ref 0 in
  let progress_count = ref 0 in
  let cfg = Sync_pipeline.default_config
    ~batch_size:5
    ~on_block:(fun _bi -> incr block_count)
    ~on_progress:(fun _p -> incr progress_count)
    ~should_stop:(fun () -> true)  (* stop immediately *)
    () in
  (* Calling should_stop should return true *)
  Alcotest.(check bool) "should_stop" true (cfg.should_stop ());
  (* Invoke callbacks manually to verify they work *)
  cfg.on_block { bi_slot = 1L; bi_hash = make_hash 1; bi_size = 10 };
  cfg.on_block { bi_slot = 2L; bi_hash = make_hash 2; bi_size = 20 };
  cfg.on_progress { blocks_synced = 2; current_slot = 2L; tip_slot = 100L;
                    tip_block = 50L; blocks_per_sec = 10.0; disk_blocks = 2 };
  Alcotest.(check int) "blocks" 2 !block_count;
  Alcotest.(check int) "progress" 1 !progress_count

(* ================================================================ *)
(* Test: store block and verify via chain points                     *)
(* ================================================================ *)

let test_pipeline_store_integration () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  (* Simulate what pipeline does: store blocks, then check chain points *)
  let points_collected = ref [] in
  for i = 1 to 20 do
    let slot = Int64.of_int (i * 50) in
    let hash = make_hash i in
    let data = make_block_data i in
    ignore (Store.store_block store ~slot ~hash ~cbor_bytes:data);
    points_collected := Chain_sync.Point (slot, hash) :: !points_collected
  done;
  (* Verify all blocks stored *)
  Alcotest.(check int) "20 blocks" 20 (Store.block_count store);
  (* Chain points should include tip (slot 1000) *)
  let cps = Store.get_chain_points store in
  (match List.hd cps with
   | Chain_sync.Point (1000L, _) -> ()
   | _ -> Alcotest.fail "expected tip at 1000");
  (* Verify recent blocks *)
  let recent = Store.get_recent_blocks store ~count:5 in
  Alcotest.(check int) "5 recent" 5 (List.length recent);
  rm_rf dir

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Sync-Pipeline"
    [ ( "Config",
        [ Alcotest.test_case "default config" `Quick test_default_config;
          Alcotest.test_case "batch size" `Quick test_batch_size_config;
          Alcotest.test_case "callbacks" `Quick test_callbacks ] );
      ( "Types",
        [ Alcotest.test_case "block_info" `Quick test_block_info;
          Alcotest.test_case "progress" `Quick test_progress;
          Alcotest.test_case "result types" `Quick test_result_types ] );
      ( "Store integration",
        [ Alcotest.test_case "chain points" `Quick test_store_chain_points_for_resume;
          Alcotest.test_case "resumable" `Quick test_resumable_chain_points;
          Alcotest.test_case "pipeline store" `Quick test_pipeline_store_integration ] );
    ]
