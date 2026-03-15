open Meridian

let () = Crypto.init ()

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF); b

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-peer-test-%d-%d" (Unix.getpid ()) (Random.int 100000))

let rm_rf dir =
  let rec go path =
    if Sys.is_directory path then begin
      Array.iter (fun f -> go (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
    end else Unix.unlink path
  in
  if Sys.file_exists dir then go dir

(* ================================================================ *)
(* Peer manager basics                                               *)
(* ================================================================ *)

let test_peer_manager_empty () =
  let pm = Peer_manager.create () in
  Alcotest.(check int) "0 peers" 0 (Peer_manager.peer_count pm);
  Alcotest.(check int) "0 inbound" 0 (Peer_manager.inbound_count pm);
  Alcotest.(check int) "0 outbound" 0 (Peer_manager.outbound_count pm)

let test_peer_register () =
  let pm = Peer_manager.create () in
  let (rd, wr) = Unix.pipe () in
  (match Peer_manager.register_peer pm ~host:"localhost" ~port:3001
           ~direction:Outbound ~fd:wr with
   | Ok peer ->
     Alcotest.(check int) "id 1" 1 peer.peer_id;
     Alcotest.(check int) "1 peer" 1 (Peer_manager.peer_count pm);
     Alcotest.(check int) "1 outbound" 1 (Peer_manager.outbound_count pm)
   | Error e -> Alcotest.fail e);
  Unix.close rd; Unix.close wr

let test_peer_remove () =
  let pm = Peer_manager.create () in
  let (rd, wr) = Unix.pipe () in
  (match Peer_manager.register_peer pm ~host:"localhost" ~port:3001
           ~direction:Outbound ~fd:wr with
   | Ok peer ->
     Peer_manager.remove_peer pm ~peer_id:peer.peer_id;
     Alcotest.(check int) "0 peers" 0 (Peer_manager.peer_count pm)
   | Error e -> Alcotest.fail e);
  Unix.close rd

let test_peer_max_limit () =
  let pm = Peer_manager.create ~max_peers:2 () in
  let fds = ref [] in
  for _ = 1 to 2 do
    let (rd, wr) = Unix.pipe () in
    fds := (rd, wr) :: !fds;
    ignore (Peer_manager.register_peer pm ~host:"h" ~port:0
              ~direction:Outbound ~fd:wr)
  done;
  let (rd, wr) = Unix.pipe () in
  (match Peer_manager.register_peer pm ~host:"h" ~port:0
           ~direction:Outbound ~fd:wr with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "expected max peers");
  Unix.close rd; Unix.close wr;
  List.iter (fun (r, _w) -> Unix.close r) !fds

let test_peer_evict () =
  let pm = Peer_manager.create () in
  let (rd, wr) = Unix.pipe () in
  ignore (Peer_manager.register_peer pm ~host:"h" ~port:0
            ~direction:Outbound ~fd:wr);
  Unix.sleepf 0.05;
  let evicted = Peer_manager.evict_inactive pm ~timeout_s:0.01 in
  Alcotest.(check int) "1 evicted" 1 evicted;
  Alcotest.(check int) "0 peers" 0 (Peer_manager.peer_count pm);
  Unix.close rd

(* ================================================================ *)
(* TCP server lifecycle                                              *)
(* ================================================================ *)

let test_tcp_server () =
  (* Use a high random port to avoid conflicts *)
  let port = 30000 + Random.int 10000 in
  let srv = Tcp_server.create ~port in
  Alcotest.(check bool) "running" true (Tcp_server.is_running srv);
  Alcotest.(check int) "port" port (Tcp_server.port srv);
  (* No connections pending *)
  Alcotest.(check bool) "no accept" true (Tcp_server.accept srv = None);
  Tcp_server.close srv

(* ================================================================ *)
(* Chain-sync server: find intersection                              *)
(* ================================================================ *)

let test_chain_sync_intersection () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  (* Store 3 blocks *)
  for i = 1 to 3 do
    ignore (Store.store_block store ~slot:(Int64.of_int (i * 100))
              ~hash:(make_hash i) ~cbor_bytes:(Bytes.make 10 '\xaa'))
  done;
  (* Find intersection with known point *)
  let pt = Chain_sync.Point (200L, make_hash 2) in
  let result = Chain_sync_server.find_intersection store [pt; Chain_sync.Origin] in
  (match result with
   | Some (Chain_sync.Point (200L, _)) -> ()
   | Some Origin -> Alcotest.fail "expected Point, got Origin"
   | None -> Alcotest.fail "expected intersection"
   | _ -> Alcotest.fail "unexpected result");
  (* Find intersection with unknown point falls back to origin *)
  let unknown = Chain_sync.Point (999L, make_hash 99) in
  let result = Chain_sync_server.find_intersection store [unknown; Chain_sync.Origin] in
  (match result with
   | Some Origin -> ()
   | _ -> Alcotest.fail "expected Origin fallback");
  rm_rf dir

let test_chain_sync_tip () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  ignore (Store.store_block store ~slot:500L ~hash:(make_hash 5)
            ~cbor_bytes:(Bytes.make 10 '\xbb'));
  let tip = Chain_sync_server.get_tip store in
  Alcotest.(check int64) "tip slot" 500L
    (match tip.tip_point with Point (s, _) -> s | Origin -> -1L);
  rm_rf dir

let test_chain_sync_empty_store () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let tip = Chain_sync_server.get_tip store in
  Alcotest.(check int64) "tip block 0" 0L tip.tip_block_number;
  rm_rf dir

(* ================================================================ *)
(* Block-fetch server: pipe simulation                               *)
(* ================================================================ *)

let test_block_fetch_serve () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  for i = 1 to 3 do
    ignore (Store.store_block store ~slot:(Int64.of_int (i * 100))
              ~hash:(make_hash i) ~cbor_bytes:(Bytes.of_string (Printf.sprintf "block%d" i)))
  done;
  let (c2s_rd, c2s_wr) = Unix.pipe () in
  let (s2c_rd, s2c_wr) = Unix.pipe () in
  let server_out = Mux.create ~fd:s2c_wr ~mode:Responder in
  let client_in = Mux.create ~fd:s2c_rd ~mode:Initiator in
  let cleanup () = List.iter (fun fd -> try Unix.close fd with _ -> ())
    [c2s_rd; c2s_wr; s2c_rd; s2c_wr] in
  (* Server handles a request *)
  let from_pt = Chain_sync.Point (100L, make_hash 1) in
  let to_pt = Chain_sync.Point (300L, make_hash 3) in
  let req_payload = Block_fetch.to_bytes (MsgRequestRange (from_pt, to_pt)) in
  let recv_called = ref false in
  let recv_fn () = recv_called := true; Ok req_payload in
  (match Block_fetch_server.handle ~store ~mux:server_out ~recv_payload:recv_fn with
   | Ok n -> Alcotest.(check bool) "served blocks" true (n > 0)
   | Error e -> cleanup (); Alcotest.fail e);
  (* Client reads: StartBatch, blocks, BatchDone *)
  (match Mux.recv_segment client_in with
   | Ok (_, p) ->
     (match Block_fetch.of_bytes p with
      | Ok MsgStartBatch -> ()
      | _ -> Alcotest.fail "expected StartBatch")
   | Error e -> cleanup (); Alcotest.fail e);
  cleanup ()

(* ================================================================ *)
(* Block propagation stats                                           *)
(* ================================================================ *)

let test_propagation_stats () =
  let pm = Peer_manager.create () in
  let stats = Block_propagation.create_stats () in
  let (rd, wr) = Unix.pipe () in
  ignore (Peer_manager.register_peer pm ~host:"h" ~port:0
            ~direction:Outbound ~fd:wr);
  let n = Block_propagation.announce_block ~peer_manager:pm
    ~block_slot:100L ~block_hash:(make_hash 1) ~stats in
  Alcotest.(check int) "1 notified" 1 n;
  Alcotest.(check int) "1 announced" 1 stats.blocks_announced;
  Unix.close rd; Unix.close wr

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "P2P"
    [ ( "Peer manager",
        [ Alcotest.test_case "empty" `Quick test_peer_manager_empty;
          Alcotest.test_case "register" `Quick test_peer_register;
          Alcotest.test_case "remove" `Quick test_peer_remove;
          Alcotest.test_case "max limit" `Quick test_peer_max_limit;
          Alcotest.test_case "evict inactive" `Quick test_peer_evict ] );
      ( "TCP server",
        [ Alcotest.test_case "lifecycle" `Quick test_tcp_server ] );
      ( "Chain-sync server",
        [ Alcotest.test_case "intersection" `Quick test_chain_sync_intersection;
          Alcotest.test_case "tip" `Quick test_chain_sync_tip;
          Alcotest.test_case "empty store" `Quick test_chain_sync_empty_store ] );
      ( "Block-fetch server",
        [ Alcotest.test_case "serve range" `Quick test_block_fetch_serve ] );
      ( "Block propagation",
        [ Alcotest.test_case "announce stats" `Quick test_propagation_stats ] );
    ]
