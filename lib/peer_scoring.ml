(* Peer quality scoring for connection prioritization. *)

type peer_score = {
  mutable successful_connections : int;
  mutable failed_connections : int;
  mutable latency_ms : float;
  mutable bytes_transferred : int64;
  mutable last_active : float;
  mutable uptime_seconds : float;
  is_root : bool;
}

let create_score ~is_root = {
  successful_connections = 0; failed_connections = 0;
  latency_ms = 1000.0; bytes_transferred = 0L;
  last_active = Unix.gettimeofday (); uptime_seconds = 0.0;
  is_root;
}

(** Compute a weighted score in [0, 1]. Higher is better. *)
let compute_score s =
  (* Root peers get a bonus *)
  let root_bonus = if s.is_root then 0.3 else 0.0 in
  (* Reliability: success rate, weight 0.4 *)
  let total = s.successful_connections + s.failed_connections in
  let reliability = if total > 0 then
    float_of_int s.successful_connections /. float_of_int total
  else 0.5 in
  (* Latency: inverse, capped, weight 0.3 *)
  let latency_score = max 0.0 (min 1.0 (1.0 -. s.latency_ms /. 5000.0)) in
  (* Activity: recency, weight 0.2 *)
  let age = Unix.gettimeofday () -. s.last_active in
  let activity_score = max 0.0 (min 1.0 (1.0 -. age /. 3600.0)) in
  min 1.0 (root_bonus +. 0.4 *. reliability +. 0.3 *. latency_score +. 0.2 *. activity_score)

let on_connect_success s ~latency_ms =
  s.successful_connections <- s.successful_connections + 1;
  s.latency_ms <- latency_ms;
  s.last_active <- Unix.gettimeofday ()

let on_connect_failure s =
  s.failed_connections <- s.failed_connections + 1

let on_data_transfer s ~bytes =
  s.bytes_transferred <- Int64.add s.bytes_transferred (Int64.of_int bytes);
  s.last_active <- Unix.gettimeofday ()

(** Exponential backoff: 1, 2, 4, 8, 16, 32, 60 max. *)
let backoff_seconds ~failures =
  let base = min 60.0 (2.0 ** float_of_int (min failures 6)) in
  min 60.0 (max 1.0 base)
