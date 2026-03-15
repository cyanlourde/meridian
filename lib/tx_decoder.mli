(** Transaction deserialization per era. *)

type tx_input = { ti_tx_hash : bytes; ti_index : int64 }

type tx_output = {
  to_address : bytes;
  to_lovelace : int64;
  to_has_multi_asset : bool;
  to_has_datum : bool;
  to_has_script_ref : bool;
}

type decoded_tx = {
  dt_inputs : tx_input list;
  dt_outputs : tx_output list;
  dt_fee : int64;
  dt_ttl : int64 option;
  dt_validity_start : int64 option;
  dt_cert_count : int;
  dt_withdrawal_count : int;
  dt_mint : bool;
  dt_collateral_count : int;
  dt_era : Block_decoder.era;
}

val decode_transaction :
  era:Block_decoder.era -> Cbor.cbor_value ->
  (decoded_tx, string) result
