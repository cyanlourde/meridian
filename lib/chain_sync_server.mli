(** Server side of node-to-node chain-sync. *)

val find_intersection : Store.store -> Chain_sync.point list -> Chain_sync.point option
val get_tip : Store.store -> Chain_sync.tip
val handle : store:Store.store -> mux:Mux.mux ->
  recv_payload:(unit -> (bytes, string) result) -> (int, string) result
