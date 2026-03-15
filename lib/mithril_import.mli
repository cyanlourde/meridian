(** Mithril snapshot unpacking and import into Meridian store. *)

val unpack_snapshot :
  archive_path:string -> dest_dir:string -> (unit, string) result

val parse_chunk_file : path:string -> bytes list

type import_stats = {
  mutable chunks_processed : int;
  mutable blocks_imported : int;
  mutable bytes_total : int;
  mutable errors : int;
}

val import_block_bytes : Store.store -> bytes -> (int64 * bytes) option

val import_immutable_db :
  immutable_dir:string -> store:Store.store ->
  on_progress:(import_stats -> unit) ->
  (import_stats, string) result

val rebuild_ledger :
  store:Store.store -> ledger:Ledger_state.t ->
  from_slot:int64 -> to_slot:int64 ->
  on_progress:(int -> unit) -> int
