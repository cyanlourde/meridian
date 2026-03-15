(** Persistent ledger state: UTXO set + metadata + snapshots.

    Reference: Shelley formal ledger spec (SL-D5), Section 9 *)

type protocol_params = {
  min_fee_a : int64;
  min_fee_b : int64;
  min_utxo_value : int64;
  max_tx_size : int;
  max_block_size : int;
}

val shelley_params : protocol_params

type block_error = {
  be_slot : int64;
  be_tx_index : int;
  be_errors : Utxo.validation_error list;
}

type t

val create : ?params:protocol_params -> unit -> t
val utxo_count : t -> int
val total_lovelace : t -> int64
val tip : t -> int64 * int
val utxo : t -> Utxo.utxo_set

type utxo_stats = { us_count : int; us_total_lovelace : int64 }
val utxo_stats : t -> utxo_stats

val apply_block : t -> Block_decoder.decoded_block -> block_error list

val snapshot : t -> path:string -> unit
val restore : path:string -> (t, string) result
