(** Unified sync pipeline: chain-sync + block-fetch + storage.

    Orchestrates header collection, block body download, and on-disk
    storage in a single loop with automatic keep-alive handling. *)

type progress = {
  blocks_synced : int;
  current_slot : int64;
  tip_slot : int64;
  tip_block : int64;
  blocks_per_sec : float;
  disk_blocks : int;
}

type block_info = {
  bi_slot : int64;
  bi_hash : bytes;
  bi_size : int;
}

type config = {
  batch_size : int;
  on_block : block_info -> unit;
  on_progress : progress -> unit;
  should_stop : unit -> bool;
}

type result =
  | Completed
  | Stopped
  | Disconnected of string * int64

val start :
  net:Network.t -> store:Store.store -> config:config ->
  (result, string) Result.t

val default_config :
  ?batch_size:int ->
  ?on_block:(block_info -> unit) ->
  ?on_progress:(progress -> unit) ->
  ?should_stop:(unit -> bool) ->
  unit -> config
