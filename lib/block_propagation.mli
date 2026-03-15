(** Block propagation — announce new blocks to peers. *)

type stats = {
  mutable blocks_announced : int;
  mutable peers_notified : int;
}

val create_stats : unit -> stats

val announce_block :
  peer_manager:Peer_manager.t ->
  block_slot:int64 -> block_hash:bytes ->
  stats:stats -> int
