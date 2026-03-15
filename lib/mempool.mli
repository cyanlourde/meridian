(** Transaction mempool. *)

type mempool_entry = {
  raw_cbor : bytes;
  decoded : Tx_decoder.decoded_tx;
  added_at : float;
  size_bytes : int;
  fee : int64;
}

type t

val create : ?max_size_bytes:int -> ?max_tx_count:int -> unit -> t
val size : t -> int * int
val has_tx : t -> tx_hash:bytes -> bool
val get_tx : t -> tx_hash:bytes -> mempool_entry option

val add_tx :
  t -> tx_hash:bytes -> raw_cbor:bytes ->
  decoded:Tx_decoder.decoded_tx -> fee:int64 ->
  (unit, string) result

val remove_tx : t -> tx_hash:bytes -> unit
val remove_confirmed : t -> block_tx_hashes:bytes list -> int
val revalidate_all : t -> utxo:Utxo.utxo_set -> current_slot:int64 -> string list
val expire : t -> current_time:float -> max_age_seconds:float -> int
val get_all : t -> mempool_entry list

type snapshot = {
  snap_tx_count : int;
  snap_total_size : int;
  snap_tx_hashes : bytes list;
}

val get_snapshot : t -> snapshot
