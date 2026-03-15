(** Peer connection management. *)

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

type t

val create : ?max_peers:int -> ?target_outbound:int -> unit -> t
val peer_count : t -> int
val inbound_count : t -> int
val outbound_count : t -> int
val get_peers : t -> peer list
val get_peer : t -> peer_id:int -> peer option

val register_peer :
  t -> host:string -> port:int -> direction:direction ->
  fd:Unix.file_descr -> (peer, string) result

val remove_peer : t -> peer_id:int -> unit
val touch_peer : t -> peer_id:int -> unit
val evict_inactive : t -> timeout_s:float -> int
