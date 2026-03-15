(* Peer connection management.

   Tracks connected peers (inbound and outbound), handles lifecycle. *)

type direction = Inbound | Outbound

type peer = {
  peer_id : int;
  host : string;
  port : int;
  direction : direction;
  fd : Unix.file_descr;
  mux : Mux.mux;
  connected_at : float;
  mutable last_activity : float;
  mutable bytes_sent : int;
  mutable bytes_received : int;
}

type t = {
  mutable peers : peer list;
  mutable next_id : int;
  max_peers : int;
  target_outbound : int;
}

let create ?(max_peers = 20) ?(target_outbound = 5) () =
  { peers = []; next_id = 1; max_peers; target_outbound }

let peer_count t = List.length t.peers
let inbound_count t = List.length (List.filter (fun p -> p.direction = Inbound) t.peers)
let outbound_count t = List.length (List.filter (fun p -> p.direction = Outbound) t.peers)
let get_peers t = t.peers
let get_peer t ~peer_id = List.find_opt (fun p -> p.peer_id = peer_id) t.peers

let register_peer t ~host ~port ~direction ~fd =
  if List.length t.peers >= t.max_peers then
    Error "max peers reached"
  else begin
    let mux = Mux.create ~fd ~mode:(match direction with
      | Outbound -> Initiator | Inbound -> Responder) in
    let now = Unix.gettimeofday () in
    let peer = { peer_id = t.next_id; host; port; direction; fd; mux;
                 connected_at = now; last_activity = now;
                 bytes_sent = 0; bytes_received = 0 } in
    t.next_id <- t.next_id + 1;
    t.peers <- peer :: t.peers;
    Ok peer
  end

let remove_peer t ~peer_id =
  let to_remove = List.find_opt (fun p -> p.peer_id = peer_id) t.peers in
  (match to_remove with
   | Some p -> (try Unix.close p.fd with _ -> ())
   | None -> ());
  t.peers <- List.filter (fun p -> p.peer_id <> peer_id) t.peers

let touch_peer t ~peer_id =
  List.iter (fun p ->
    if p.peer_id = peer_id then
      p.last_activity <- Unix.gettimeofday ()
  ) t.peers

let evict_inactive t ~timeout_s =
  let cutoff = Unix.gettimeofday () -. timeout_s in
  let (keep, evict) = List.partition (fun p ->
    p.last_activity >= cutoff
  ) t.peers in
  List.iter (fun p -> try Unix.close p.fd with _ -> ()) evict;
  t.peers <- keep;
  List.length evict
