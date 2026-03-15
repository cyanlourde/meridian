(** Genesis configuration parser for Cardano. *)

type genesis_config = {
  network_magic : int64;
  epoch_length : int64;
  slot_length : int;
  active_slots_coeff : float;
  max_kes_evolutions : int;
  max_lovelace_supply : int64;
  protocol_params : Ledger_state.protocol_params;
  initial_funds : (bytes * int64) list;
}

val parse_shelley_genesis : path:string -> (genesis_config, string) result

val genesis_utxos : genesis_config -> (Utxo.TxIn.t * Utxo.TxOut.t) list

val preview_genesis : genesis_config
val preprod_genesis : genesis_config
val mainnet_genesis : genesis_config

val genesis_for_network : string -> genesis_config
val epoch_params_for_network : string -> Epoch.epoch_params
val default_node_for_network : string -> string * int

val init_ledger : genesis_config -> Ledger_state.t
