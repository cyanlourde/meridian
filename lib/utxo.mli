(** UTXO set management and transaction validation.

    Full multi-asset conservation:
    consumed + mint = produced
    where consumed = sum(inputs) + withdrawals + refunds
    and produced = sum(outputs) + fee + deposits *)

module TxIn : sig
  type t = { tx_hash : bytes; tx_index : int }
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val of_decoder : Tx_decoder.tx_input -> t
  val to_string : t -> string
end

module TxOut : sig
  type t = {
    address : bytes;
    value : Multi_asset.value;
    has_datum : bool;
    has_script_ref : bool;
  }
  val of_decoder : Tx_decoder.tx_output -> t
end

type utxo_set

val create : unit -> utxo_set
val size : utxo_set -> int
val mem : utxo_set -> TxIn.t -> bool
val find : utxo_set -> TxIn.t -> TxOut.t option
val add : utxo_set -> TxIn.t -> TxOut.t -> unit
val remove : utxo_set -> TxIn.t -> unit
val iter : (TxIn.t -> TxOut.t -> unit) -> utxo_set -> unit
val find_by_address : utxo_set -> address:bytes -> (TxIn.t * TxOut.t) list
val find_by_txins : utxo_set -> TxIn.t list -> (TxIn.t * TxOut.t option) list
val total_lovelace : utxo_set -> int64
val total_assets : utxo_set -> int

type validation_error =
  | Input_not_in_utxo of TxIn.t
  | Duplicate_input of TxIn.t
  | Insufficient_fee of { required : int64; actual : int64 }
  | Value_not_conserved of { consumed : int64; produced : int64 }
  | Output_too_small of { index : int; lovelace : int64; minimum : int64 }
  | Expired_ttl of { slot : int64; ttl : int64 }
  | Empty_inputs
  | Empty_outputs

val error_to_string : validation_error -> string

val validate_tx :
  ?min_fee_a:int64 -> ?min_fee_b:int64 ->
  ?min_utxo_value:int64 -> ?tx_size_estimate:int ->
  ?key_deposit:int64 -> ?pool_deposit:int64 ->
  utxo:utxo_set -> current_slot:int64 ->
  Tx_decoder.decoded_tx -> validation_error list

val apply_tx : utxo_set -> tx_hash:bytes -> Tx_decoder.decoded_tx -> unit

val apply_block :
  utxo_set -> current_slot:int64 ->
  tx_hash_fn:(Cbor.cbor_value -> bytes) ->
  Block_decoder.decoded_block ->
  int * validation_error list
