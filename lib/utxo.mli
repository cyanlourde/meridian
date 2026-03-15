(** UTXO set management and transaction validation.

    Reference: Shelley formal ledger spec (SL-D5), Section 9 "UTxO"

    Tracks unspent transaction outputs, validates that transactions
    only spend existing outputs, and applies the UTxO transition rule:
    utxo' = (utxo \ inputs) ∪ outputs *)

(** {1 UTxO key and value types} *)

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
    lovelace : int64;
    has_multi_asset : bool;
    has_datum : bool;
    has_script_ref : bool;
  }
  val of_decoder : Tx_decoder.tx_output -> t
end

(** {1 UTxO set} *)

type utxo_set

val create : unit -> utxo_set
val size : utxo_set -> int
val mem : utxo_set -> TxIn.t -> bool
val find : utxo_set -> TxIn.t -> TxOut.t option
val add : utxo_set -> TxIn.t -> TxOut.t -> unit
val remove : utxo_set -> TxIn.t -> unit
val total_lovelace : utxo_set -> int64

(** {1 Transaction validation} *)

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
  utxo:utxo_set -> current_slot:int64 ->
  Tx_decoder.decoded_tx -> validation_error list

(** {1 UTxO transition} *)

val apply_tx :
  utxo_set -> tx_hash:bytes -> Tx_decoder.decoded_tx -> unit

val apply_block :
  utxo_set -> current_slot:int64 ->
  tx_hash_fn:(Cbor.cbor_value -> bytes) ->
  Block_decoder.decoded_block ->
  int * validation_error list
(** Returns [(applied_count, errors)]. *)
