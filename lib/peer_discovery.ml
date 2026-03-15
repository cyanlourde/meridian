(* Dynamic peer discovery via gossip and topology. *)

type known_peer = {
  kp_host : string;
  kp_port : int;
  kp_score : Peer_scoring.peer_score;
  mutable kp_last_attempt : float;
  mutable kp_failures : int;
}

type t = {
  known : (string, known_peer) Hashtbl.t;
  target_outbound : int;
  max_inbound : int;
  min_root_peers : int;
}

let peer_key ~host ~port = Printf.sprintf "%s:%d" host port

let create ?(target_outbound = 5) ?(max_inbound = 20) ?(min_root_peers = 1) () =
  { known = Hashtbl.create 64; target_outbound; max_inbound; min_root_peers }

let add_known t ~host ~port ~is_root =
  let key = peer_key ~host ~port in
  if not (Hashtbl.mem t.known key) then
    Hashtbl.replace t.known key {
      kp_host = host; kp_port = port;
      kp_score = Peer_scoring.create_score ~is_root;
      kp_last_attempt = 0.0; kp_failures = 0;
    }

let get_known t ~host ~port =
  Hashtbl.find_opt t.known (peer_key ~host ~port)

let known_count t = Hashtbl.length t.known

(** Load peers from topology. *)
let load_topology t (topo : Topology.topology) =
  List.iter (fun (e : Topology.topology_entry) ->
    add_known t ~host:e.te_host ~port:e.te_port ~is_root:e.te_is_root
  ) (Topology.all_entries topo)

(** Record a successful connection. *)
let on_connected t ~host ~port ~latency_ms =
  match get_known t ~host ~port with
  | Some kp ->
    Peer_scoring.on_connect_success kp.kp_score ~latency_ms;
    kp.kp_failures <- 0
  | None ->
    add_known t ~host ~port ~is_root:false;
    (match get_known t ~host ~port with
     | Some kp -> Peer_scoring.on_connect_success kp.kp_score ~latency_ms
     | None -> ())

(** Record a failed connection. *)
let on_connection_failed t ~host ~port =
  match get_known t ~host ~port with
  | Some kp ->
    Peer_scoring.on_connect_failure kp.kp_score;
    kp.kp_failures <- kp.kp_failures + 1;
    kp.kp_last_attempt <- Unix.gettimeofday ()
  | None -> ()

(** Get candidates for new outbound connections, scored and filtered. *)
let get_candidates t ~currently_connected =
  let now = Unix.gettimeofday () in
  let candidates = Hashtbl.fold (fun key kp acc ->
    if List.mem key currently_connected then acc
    else
      let backoff = Peer_scoring.backoff_seconds ~failures:kp.kp_failures in
      if now -. kp.kp_last_attempt < backoff then acc
      else (kp, Peer_scoring.compute_score kp.kp_score) :: acc
  ) t.known [] in
  (* Sort by score descending *)
  let sorted = List.sort (fun (_, a) (_, b) -> compare b a) candidates in
  List.map fst sorted

(** Get connection stats. *)
type connection_stats = {
  cs_outbound : int;
  cs_inbound : int;
  cs_known : int;
  cs_target : int;
}

let connection_stats t ~outbound ~inbound = {
  cs_outbound = outbound;
  cs_inbound = inbound;
  cs_known = known_count t;
  cs_target = t.target_outbound;
}
