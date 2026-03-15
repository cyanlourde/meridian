open Meridian

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-topo-%d-%d" (Unix.getpid ()) (Random.int 100000))

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
(* Topology parsing                                                  *)
(* ================================================================ *)

let test_parse_legacy () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "topology.json" in
  write_file path {|{
    "Producers": [
      {"addr": "host1.example.com", "port": 3001, "valency": 1},
      {"addr": "host2.example.com", "port": 3002, "valency": 2},
      {"addr": "host3.example.com", "port": 3001, "valency": 1}
    ]
  }|};
  match Topology.parse_topology ~path with
  | Error e -> Alcotest.fail e
  | Ok topo ->
    Alcotest.(check int) "3 local roots" 3 (List.length topo.local_roots);
    let e1 = List.hd topo.local_roots in
    Alcotest.(check string) "host1" "host1.example.com" e1.te_host;
    Alcotest.(check int) "port1" 3001 e1.te_port;
    Alcotest.(check bool) "is root" true e1.te_is_root;
    rm_rf dir

let test_parse_p2p () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "topology.json" in
  write_file path {|{
    "localRoots": [{
      "accessPoints": [{"address": "local1.example.com", "port": 3001}],
      "advertise": false, "valency": 1
    }],
    "publicRoots": [{
      "accessPoints": [{"address": "public1.example.com", "port": 3001}],
      "advertise": true
    }]
  }|};
  match Topology.parse_topology ~path with
  | Error e -> Alcotest.fail e
  | Ok topo ->
    Alcotest.(check int) "1 local" 1 (List.length topo.local_roots);
    Alcotest.(check int) "1 public" 1 (List.length topo.public_roots);
    Alcotest.(check bool) "local is root" true
      (List.hd topo.local_roots).te_is_root;
    Alcotest.(check bool) "public not root" false
      (List.hd topo.public_roots).te_is_root;
    rm_rf dir

let test_default_topology () =
  let topo = Topology.default_topology "preview" in
  Alcotest.(check int) "1 preview root" 1 (List.length topo.local_roots);
  let topo = Topology.default_topology "mainnet" in
  Alcotest.(check int) "3 mainnet roots" 3 (List.length topo.local_roots)

(* ================================================================ *)
(* Peer scoring                                                      *)
(* ================================================================ *)

let test_scoring_reliability () =
  let good = Peer_scoring.create_score ~is_root:false in
  Peer_scoring.on_connect_success good ~latency_ms:50.0;
  Peer_scoring.on_connect_success good ~latency_ms:50.0;
  let bad = Peer_scoring.create_score ~is_root:false in
  Peer_scoring.on_connect_failure bad;
  Peer_scoring.on_connect_failure bad;
  Alcotest.(check bool) "good > bad" true
    (Peer_scoring.compute_score good > Peer_scoring.compute_score bad)

let test_root_bonus () =
  let root = Peer_scoring.create_score ~is_root:true in
  let non_root = Peer_scoring.create_score ~is_root:false in
  Alcotest.(check bool) "root > non-root" true
    (Peer_scoring.compute_score root > Peer_scoring.compute_score non_root)

let test_backoff () =
  Alcotest.(check bool) "1st ~1s" true (Peer_scoring.backoff_seconds ~failures:0 <= 2.0);
  Alcotest.(check bool) "2nd ~2s" true (Peer_scoring.backoff_seconds ~failures:1 <= 3.0);
  Alcotest.(check bool) "3rd ~4s" true (Peer_scoring.backoff_seconds ~failures:2 <= 5.0);
  Alcotest.(check bool) "cap 60s" true (Peer_scoring.backoff_seconds ~failures:100 <= 60.0)

(* ================================================================ *)
(* Peer discovery                                                    *)
(* ================================================================ *)

let test_known_peers () =
  let pd = Peer_discovery.create () in
  Peer_discovery.add_known pd ~host:"h1" ~port:3001 ~is_root:true;
  Peer_discovery.add_known pd ~host:"h2" ~port:3002 ~is_root:false;
  Alcotest.(check int) "2 known" 2 (Peer_discovery.known_count pd);
  (match Peer_discovery.get_known pd ~host:"h1" ~port:3001 with
   | Some kp -> Alcotest.(check bool) "is root" true kp.kp_score.is_root
   | None -> Alcotest.fail "h1 not found");
  Alcotest.(check bool) "h3 not found" true
    (Peer_discovery.get_known pd ~host:"h3" ~port:3001 = None)

let test_load_topology () =
  let pd = Peer_discovery.create () in
  let topo = Topology.default_topology "mainnet" in
  Peer_discovery.load_topology pd topo;
  Alcotest.(check int) "3 known" 3 (Peer_discovery.known_count pd)

let test_candidates () =
  let pd = Peer_discovery.create ~target_outbound:2 () in
  Peer_discovery.add_known pd ~host:"h1" ~port:3001 ~is_root:true;
  Peer_discovery.add_known pd ~host:"h2" ~port:3002 ~is_root:false;
  Peer_discovery.add_known pd ~host:"h3" ~port:3003 ~is_root:false;
  let candidates = Peer_discovery.get_candidates pd ~currently_connected:["h1:3001"] in
  Alcotest.(check int) "2 candidates" 2 (List.length candidates);
  (* h1 excluded because already connected *)
  let hosts = List.map (fun (kp : Peer_discovery.known_peer) -> kp.kp_host) candidates in
  Alcotest.(check bool) "h1 excluded" true (not (List.mem "h1" hosts))

let test_on_connected () =
  let pd = Peer_discovery.create () in
  Peer_discovery.add_known pd ~host:"h1" ~port:3001 ~is_root:false;
  Peer_discovery.on_connected pd ~host:"h1" ~port:3001 ~latency_ms:50.0;
  match Peer_discovery.get_known pd ~host:"h1" ~port:3001 with
  | Some kp ->
    Alcotest.(check int) "1 success" 1 kp.kp_score.successful_connections;
    Alcotest.(check int) "0 failures" 0 kp.kp_failures
  | None -> Alcotest.fail "not found"

let test_on_failed () =
  let pd = Peer_discovery.create () in
  Peer_discovery.add_known pd ~host:"h1" ~port:3001 ~is_root:false;
  Peer_discovery.on_connection_failed pd ~host:"h1" ~port:3001;
  Peer_discovery.on_connection_failed pd ~host:"h1" ~port:3001;
  match Peer_discovery.get_known pd ~host:"h1" ~port:3001 with
  | Some kp ->
    Alcotest.(check int) "2 failures" 2 kp.kp_failures
  | None -> Alcotest.fail "not found"

let test_connection_stats () =
  let pd = Peer_discovery.create ~target_outbound:5 () in
  Peer_discovery.add_known pd ~host:"h1" ~port:3001 ~is_root:true;
  let stats = Peer_discovery.connection_stats pd ~outbound:2 ~inbound:3 in
  Alcotest.(check int) "outbound" 2 stats.cs_outbound;
  Alcotest.(check int) "inbound" 3 stats.cs_inbound;
  Alcotest.(check int) "known" 1 stats.cs_known;
  Alcotest.(check int) "target" 5 stats.cs_target

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Topology"
    [ ( "Topology parsing",
        [ Alcotest.test_case "legacy" `Quick test_parse_legacy;
          Alcotest.test_case "p2p" `Quick test_parse_p2p;
          Alcotest.test_case "defaults" `Quick test_default_topology ] );
      ( "Peer scoring",
        [ Alcotest.test_case "reliability" `Quick test_scoring_reliability;
          Alcotest.test_case "root bonus" `Quick test_root_bonus;
          Alcotest.test_case "backoff" `Quick test_backoff ] );
      ( "Peer discovery",
        [ Alcotest.test_case "known peers" `Quick test_known_peers;
          Alcotest.test_case "load topology" `Quick test_load_topology;
          Alcotest.test_case "candidates" `Quick test_candidates;
          Alcotest.test_case "connected" `Quick test_on_connected;
          Alcotest.test_case "failed" `Quick test_on_failed;
          Alcotest.test_case "stats" `Quick test_connection_stats ] );
    ]
