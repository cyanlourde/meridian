(** Dynamic peer discovery via gossip and topology. *)

type known_peer = {
  kp_host : string;
  kp_port : int;
  kp_score : Peer_scoring.peer_score;
  mutable kp_last_attempt : float;
  mutable kp_failures : int;
}

type t

val create : ?target_outbound:int -> ?max_inbound:int -> ?min_root_peers:int -> unit -> t
val add_known : t -> host:string -> port:int -> is_root:bool -> unit
val get_known : t -> host:string -> port:int -> known_peer option
val known_count : t -> int
val load_topology : t -> Topology.topology -> unit
val on_connected : t -> host:string -> port:int -> latency_ms:float -> unit
val on_connection_failed : t -> host:string -> port:int -> unit
val get_candidates : t -> currently_connected:string list -> known_peer list

type connection_stats = {
  cs_outbound : int; cs_inbound : int; cs_known : int; cs_target : int;
}
val connection_stats : t -> outbound:int -> inbound:int -> connection_stats
