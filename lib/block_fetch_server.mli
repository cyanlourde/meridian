(** Server side of node-to-node block-fetch. *)

val handle : store:Store.store -> mux:Mux.mux ->
  recv_payload:(unit -> (bytes, string) result) -> (int, string) result
