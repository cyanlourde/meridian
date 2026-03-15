(** Peer quality scoring for connection prioritization. *)

type peer_score = {
  mutable successful_connections : int;
  mutable failed_connections : int;
  mutable latency_ms : float;
  mutable bytes_transferred : int64;
  mutable last_active : float;
  mutable uptime_seconds : float;
  is_root : bool;
}

val create_score : is_root:bool -> peer_score
val compute_score : peer_score -> float
val on_connect_success : peer_score -> latency_ms:float -> unit
val on_connect_failure : peer_score -> unit
val on_data_transfer : peer_score -> bytes:int -> unit
val backoff_seconds : failures:int -> float
