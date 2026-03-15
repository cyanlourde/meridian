(** Transaction deserialization per era. *)

type tx_input = { ti_tx_hash : bytes; ti_index : int64 }

type tx_output = {
  to_address : bytes;
  to_lovelace : int64;
  to_has_multi_asset : bool;
  to_has_datum : bool;
  to_has_script_ref : bool;
}

type cert_action =
  | Cert_stake_registration
  | Cert_stake_deregistration
  | Cert_pool_registration
  | Cert_pool_retirement
  | Cert_other

type decoded_tx = {
  dt_inputs : tx_input list;
  dt_outputs : tx_output list;
  dt_fee : int64;
  dt_ttl : int64 option;
  dt_validity_start : int64 option;
  dt_certs : cert_action list;
  dt_withdrawal_total : int64;
  dt_mint : bool;
  dt_collateral_inputs : tx_input list;
  dt_collateral_return : tx_output option;
  dt_total_collateral : int64 option;
  dt_is_valid : bool;
  dt_era : Block_decoder.era;
}

val decode_transaction :
  era:Block_decoder.era -> Cbor.cbor_value ->
  (decoded_tx, string) result
