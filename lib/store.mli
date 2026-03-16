(** On-disk block store.

    Append-only, content-addressed block storage with crash-safe
    writes (temp file + atomic rename). Maintains a chain index
    mapping slots to block hashes. *)

type store

type block_entry = {
  slot : int64;
  hash : bytes;
}

val init : ?cache_max:int -> base_dir:string -> unit -> store

val store_block :
  store -> slot:int64 -> hash:bytes -> cbor_bytes:bytes ->
  (unit, string) result

val get_block : store -> hash:bytes -> bytes option
val get_block_by_slot : store -> slot:int64 -> bytes option
val has_block : store -> hash:bytes -> bool
val tip : store -> (int64 * bytes) option
val block_count : store -> int

val get_recent_blocks : store -> count:int -> (int64 * bytes) list
val get_chain_points : store -> Chain_sync.point list
val flush_meta : store -> unit
