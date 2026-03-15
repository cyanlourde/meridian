(** Cardano block and transaction types for Byron through Conway eras.

    References:
    - Byron CDDL:   cardano-ledger/eras/byron/cddl-spec/byron.cddl
    - Shelley CDDL: cardano-ledger/eras/shelley/impl/cddl-files/shelley.cddl
    - Allegra CDDL: cardano-ledger/eras/allegra/impl/cddl-files/allegra.cddl
    - Mary CDDL:    cardano-ledger/eras/mary/impl/cddl-files/mary.cddl
    - Alonzo CDDL:  cardano-ledger/eras/alonzo/impl/cddl-files/alonzo.cddl
    - Babbage CDDL:  cardano-ledger/eras/babbage/impl/cddl-files/babbage.cddl
    - Conway CDDL:  cardano-ledger/eras/conway/impl/cddl-files/conway.cddl
    - Shelley formal ledger spec (SL-D5) *)

(** {1 Common types} *)

(** 28-byte Blake2b-224 hash *)
type hash28 = bytes

(** 32-byte Blake2b-256 hash *)
type hash32 = bytes

type block_hash = hash32
type tx_hash = hash32
type script_hash = hash28
type vkey_hash = hash28
type pool_keyhash = hash28
type genesis_hash = hash28
type genesis_delegate_hash = hash28
type vrf_keyhash = hash32
type auxiliary_data_hash = hash32

type vkey = bytes
type signature = bytes
type vrf_vkey = bytes
type kes_vkey = bytes
type kes_signature = bytes

type vrf_cert = {
  vrf_output : bytes;
  vrf_proof : bytes;
}

type slot_number = int64
type epoch_number = int64
type block_number = int64
type coin = int64

type rational = {
  numerator : int64;
  denominator : int64;
}

type unit_interval = rational

(** {1 Byron era types} *)

type byron_address = {
  address_root : hash28;
  address_attributes : Cbor.cbor_value;
  address_type : int;
}

type byron_tx_in = {
  byron_txin_id : tx_hash;
  byron_txin_index : int64;
}

type byron_tx_out = {
  byron_txout_address : byron_address;
  byron_txout_amount : coin;
}

type byron_tx = {
  byron_tx_inputs : byron_tx_in list;
  byron_tx_outputs : byron_tx_out list;
  byron_tx_attributes : Cbor.cbor_value;
}

type byron_consensus_data = {
  byron_epoch : epoch_number;
  byron_slot_in_epoch : slot_number;
  byron_issuer : bytes;
  byron_difficulty : int64;
  byron_signature : Cbor.cbor_value;
}

type byron_block_header = {
  byron_protocol_magic : int64;
  byron_prev_block : block_hash;
  byron_body_proof : Cbor.cbor_value;
  byron_consensus : byron_consensus_data;
  byron_extra_data : Cbor.cbor_value;
}

type byron_block_body = {
  byron_tx_payload : Cbor.cbor_value;
  byron_ssc_payload : Cbor.cbor_value;
  byron_dlg_payload : Cbor.cbor_value;
  byron_upd_payload : Cbor.cbor_value;
}

type byron_block = {
  byron_header : byron_block_header;
  byron_body : byron_block_body;
  byron_extra : Cbor.cbor_value list;
}

(** {1 Shelley era types} *)

type credential =
  | Key_hash of vkey_hash
  | Script_hash of script_hash

type stake_pointer = {
  slot : slot_number;
  tx_index : int64;
  cert_index : int64;
}

type shelley_address =
  | Base_address of { network : int; payment : credential; stake : credential }
  | Pointer_address of { network : int; payment : credential; pointer : stake_pointer }
  | Enterprise_address of { network : int; payment : credential }
  | Reward_address of { network : int; stake : credential }
  | Byron_compat_address of bytes

type tx_in = {
  tx_id : tx_hash;
  tx_index : int64;
}

type shelley_tx_out = {
  address : bytes;
  amount : coin;
}

type withdrawals = (bytes * coin) list

type relay =
  | Single_host_addr of { port : int option; ipv4 : bytes option; ipv6 : bytes option }
  | Single_host_name of { port : int option; dns_name : string }
  | Multi_host_name of { dns_name : string }

type pool_metadata = {
  pool_url : string;
  pool_metadata_hash : hash32;
}

type pool_params = {
  pool_operator : pool_keyhash;
  pool_vrf_keyhash : vrf_keyhash;
  pool_pledge : coin;
  pool_cost : coin;
  pool_margin : unit_interval;
  pool_reward_account : bytes;
  pool_owners : vkey_hash list;
  pool_relays : relay list;
  pool_metadata : pool_metadata option;
}

type certificate =
  | Stake_registration of credential
  | Stake_deregistration of credential
  | Stake_delegation of { delegator : credential; pool : pool_keyhash }
  | Pool_registration of pool_params
  | Pool_retirement of { pool : pool_keyhash; epoch : epoch_number }
  | Genesis_key_delegation of {
      genesis_hash : genesis_hash;
      delegate_hash : genesis_delegate_hash;
      vrf_keyhash : vrf_keyhash;
    }
  | Move_instantaneous_rewards of Cbor.cbor_value

type protocol_param_update = {
  min_fee_a : int64 option;
  min_fee_b : int64 option;
  max_block_body_size : int64 option;
  max_tx_size : int64 option;
  max_block_header_size : int64 option;
  key_deposit : coin option;
  pool_deposit : coin option;
  max_epoch : epoch_number option;
  n_opt : int64 option;
  pool_pledge_influence : rational option;
  expansion_rate : unit_interval option;
  treasury_growth_rate : unit_interval option;
  decentralization_param : unit_interval option;
  extra_entropy : Cbor.cbor_value option;
  protocol_version : (int64 * int64) option;
  min_utxo_value : coin option;
}

type proposed_protocol_updates = (genesis_hash * protocol_param_update) list

type update = {
  proposed_updates : proposed_protocol_updates;
  update_epoch : epoch_number;
}

type shelley_tx_body = {
  inputs : tx_in list;
  outputs : shelley_tx_out list;
  fee : coin;
  ttl : slot_number;
  certs : certificate list option;
  withdrawals : withdrawals option;
  update : update option;
  auxiliary_data_hash : auxiliary_data_hash option;
}

type vkey_witness = {
  witness_vkey : vkey;
  witness_sig : signature;
}

type multisig_script =
  | Sig of vkey_hash
  | All_of of multisig_script list
  | Any_of of multisig_script list
  | N_of of int * multisig_script list

type shelley_tx_witness_set = {
  vkey_witnesses : vkey_witness list option;
  multisig_scripts : multisig_script list option;
  bootstrap_witnesses : Cbor.cbor_value option;
}

type shelley_tx = {
  shelley_tx_body : shelley_tx_body;
  shelley_tx_witness_set : shelley_tx_witness_set;
  shelley_tx_metadata : Cbor.cbor_value option;
}

type operational_cert = {
  hot_vkey : kes_vkey;
  sequence_number : int64;
  kes_period : int64;
  sigma : signature;
}

type protocol_version = {
  proto_major : int64;
  proto_minor : int64;
}

type shelley_header_body = {
  shb_block_number : block_number;
  shb_slot : slot_number;
  shb_prev_hash : block_hash option;
  shb_issuer_vkey : vkey;
  shb_vrf_vkey : vrf_vkey;
  shb_nonce_vrf : vrf_cert;
  shb_leader_vrf : vrf_cert;
  shb_body_size : int64;
  shb_body_hash : hash32;
  shb_operational_cert : operational_cert;
  shb_protocol_version : protocol_version;
}

type shelley_header = {
  sh_header_body : shelley_header_body;
  sh_body_signature : kes_signature;
}

type shelley_block = {
  sb_header : shelley_header;
  sb_tx_bodies : shelley_tx_body list;
  sb_tx_witness_sets : shelley_tx_witness_set list;
  sb_tx_metadata : (int * Cbor.cbor_value) list;
}

(** {1 Allegra era types} *)

type timelock =
  | TL_Signature of vkey_hash
  | TL_AllOf of timelock list
  | TL_AnyOf of timelock list
  | TL_MOfN of int * timelock list
  | TL_InvalidBefore of slot_number
  | TL_InvalidHereafter of slot_number

type allegra_tx_body = {
  al_inputs : tx_in list;
  al_outputs : shelley_tx_out list;
  al_fee : coin;
  al_ttl : slot_number option;
  al_certs : certificate list option;
  al_withdrawals : withdrawals option;
  al_update : update option;
  al_auxiliary_data_hash : auxiliary_data_hash option;
  al_validity_interval_start : slot_number option;
}

type allegra_tx_witness_set = {
  al_vkey_witnesses : vkey_witness list option;
  al_native_scripts : timelock list option;
  al_bootstrap_witnesses : Cbor.cbor_value option;
}

type allegra_tx = {
  allegra_tx_body : allegra_tx_body;
  allegra_tx_witness_set : allegra_tx_witness_set;
  allegra_tx_metadata : Cbor.cbor_value option;
}

type allegra_block = {
  alb_header : shelley_header;
  alb_tx_bodies : allegra_tx_body list;
  alb_tx_witness_sets : allegra_tx_witness_set list;
  alb_tx_metadata : (int * Cbor.cbor_value) list;
}

(** {1 Mary era types} *)

type multi_asset = (script_hash * (bytes * int64) list) list

type value =
  | Lovelace of coin
  | Lovelace_and_assets of coin * multi_asset

type mary_tx_out = {
  ma_address : bytes;
  ma_value : value;
}

type mary_tx_body = {
  ma_inputs : tx_in list;
  ma_outputs : mary_tx_out list;
  ma_fee : coin;
  ma_ttl : slot_number option;
  ma_certs : certificate list option;
  ma_withdrawals : withdrawals option;
  ma_update : update option;
  ma_auxiliary_data_hash : auxiliary_data_hash option;
  ma_validity_interval_start : slot_number option;
  ma_mint : multi_asset option;
}

type mary_tx_witness_set = allegra_tx_witness_set

type mary_tx = {
  mary_tx_body : mary_tx_body;
  mary_tx_witness_set : mary_tx_witness_set;
  mary_tx_metadata : Cbor.cbor_value option;
}

type mary_block = {
  mab_header : shelley_header;
  mab_tx_bodies : mary_tx_body list;
  mab_tx_witness_sets : mary_tx_witness_set list;
  mab_tx_metadata : (int * Cbor.cbor_value) list;
}

(** {1 Alonzo era types} *)

type plutus_data = Cbor.cbor_value

type ex_units = { mem : int64; steps : int64 }

type redeemer_tag = Spend | Mint | Cert | Reward

type redeemer = {
  rd_tag : redeemer_tag;
  rd_index : int64;
  rd_data : plutus_data;
  rd_ex_units : ex_units;
}

type alonzo_tx_out = {
  az_address : bytes;
  az_value : value;
  az_datum_hash : hash32 option;
}

type alonzo_tx_body = {
  az_inputs : tx_in list;
  az_outputs : alonzo_tx_out list;
  az_fee : coin;
  az_ttl : slot_number option;
  az_certs : certificate list option;
  az_withdrawals : withdrawals option;
  az_update : update option;
  az_auxiliary_data_hash : auxiliary_data_hash option;
  az_validity_interval_start : slot_number option;
  az_mint : multi_asset option;
  az_script_data_hash : hash32 option;
  az_collateral : tx_in list option;
  az_required_signers : vkey_hash list option;
  az_network_id : int64 option;
}

type alonzo_tx_witness_set = {
  az_vkey_witnesses : vkey_witness list option;
  az_native_scripts : timelock list option;
  az_bootstrap_witnesses : Cbor.cbor_value option;
  az_plutus_v1_scripts : bytes list option;
  az_plutus_data : plutus_data list option;
  az_redeemers : redeemer list option;
}

type alonzo_tx = {
  alonzo_tx_body : alonzo_tx_body;
  alonzo_tx_witness_set : alonzo_tx_witness_set;
  alonzo_tx_is_valid : bool;
  alonzo_tx_auxiliary_data : Cbor.cbor_value option;
}

type alonzo_block = {
  azb_header : shelley_header;
  azb_tx_bodies : alonzo_tx_body list;
  azb_tx_witness_sets : alonzo_tx_witness_set list;
  azb_tx_metadata : (int * Cbor.cbor_value) list;
  azb_invalid_txs : int list;
}

(** {1 Babbage era types} *)

type datum_option =
  | Datum_hash of hash32
  | Inline_datum of plutus_data

type script_ref =
  | Native_script_ref of timelock
  | Plutus_v1_script_ref of bytes
  | Plutus_v2_script_ref of bytes
  | Plutus_v3_script_ref of bytes

type babbage_tx_out = {
  bb_address : bytes;
  bb_value : value;
  bb_datum_option : datum_option option;
  bb_script_ref : script_ref option;
}

type babbage_header_body = {
  bhb_block_number : block_number;
  bhb_slot : slot_number;
  bhb_prev_hash : block_hash option;
  bhb_issuer_vkey : vkey;
  bhb_vrf_vkey : vrf_vkey;
  bhb_vrf_result : vrf_cert;
  bhb_body_size : int64;
  bhb_body_hash : hash32;
  bhb_operational_cert : operational_cert;
  bhb_protocol_version : protocol_version;
}

type babbage_header = {
  bh_header_body : babbage_header_body;
  bh_body_signature : kes_signature;
}

type babbage_tx_body = {
  bb_inputs : tx_in list;
  bb_outputs : babbage_tx_out list;
  bb_fee : coin;
  bb_ttl : slot_number option;
  bb_certs : certificate list option;
  bb_withdrawals : withdrawals option;
  bb_update : update option;
  bb_auxiliary_data_hash : auxiliary_data_hash option;
  bb_validity_interval_start : slot_number option;
  bb_mint : multi_asset option;
  bb_script_data_hash : hash32 option;
  bb_collateral : tx_in list option;
  bb_required_signers : vkey_hash list option;
  bb_network_id : int64 option;
  bb_collateral_return : babbage_tx_out option;
  bb_total_collateral : coin option;
  bb_reference_inputs : tx_in list option;
}

type babbage_tx_witness_set = {
  bb_vkey_witnesses : vkey_witness list option;
  bb_native_scripts : timelock list option;
  bb_bootstrap_witnesses : Cbor.cbor_value option;
  bb_plutus_v1_scripts : bytes list option;
  bb_plutus_data : plutus_data list option;
  bb_redeemers : redeemer list option;
  bb_plutus_v2_scripts : bytes list option;
}

type babbage_tx = {
  babbage_tx_body : babbage_tx_body;
  babbage_tx_witness_set : babbage_tx_witness_set;
  babbage_tx_is_valid : bool;
  babbage_tx_auxiliary_data : Cbor.cbor_value option;
}

type babbage_block = {
  bbb_header : babbage_header;
  bbb_tx_bodies : babbage_tx_body list;
  bbb_tx_witness_sets : babbage_tx_witness_set list;
  bbb_tx_metadata : (int * Cbor.cbor_value) list;
  bbb_invalid_txs : int list;
}

(** {1 Conway era types} *)

type anchor = { anchor_url : string; anchor_hash : hash32 }

type drep =
  | Drep_key_hash of vkey_hash
  | Drep_script_hash of script_hash
  | Drep_always_abstain
  | Drep_always_no_confidence

type voter =
  | Cc_hot_key_hash of vkey_hash
  | Cc_hot_script_hash of script_hash
  | Drep_voter_key_hash of vkey_hash
  | Drep_voter_script_hash of script_hash
  | Spo_voter of pool_keyhash

type vote = Vote_no | Vote_yes | Vote_abstain

type governance_action_id = { ga_tx_id : tx_hash; ga_index : int64 }

type voting_procedure = { vp_vote : vote; vp_anchor : anchor option }

type constitution = {
  constitution_anchor : anchor;
  constitution_script_hash : script_hash option;
}

type governance_action =
  | Parameter_change of {
      prev_action : governance_action_id option;
      param_update : protocol_param_update;
      policy_hash : script_hash option;
    }
  | Hard_fork_initiation of {
      prev_action : governance_action_id option;
      protocol_version : protocol_version;
    }
  | Treasury_withdrawals of {
      withdrawals : (bytes * coin) list;
      policy_hash : script_hash option;
    }
  | No_confidence of { prev_action : governance_action_id option }
  | Update_committee of {
      prev_action : governance_action_id option;
      members_to_remove : credential list;
      members_to_add : (credential * epoch_number) list;
      quorum : unit_interval;
    }
  | New_constitution of {
      prev_action : governance_action_id option;
      constitution : constitution;
    }
  | Info_action

type proposal_procedure = {
  pp_deposit : coin;
  pp_return_addr : bytes;
  pp_governance_action : governance_action;
  pp_anchor : anchor;
}

type conway_certificate =
  | CC_Stake_registration of credential
  | CC_Stake_deregistration of credential
  | CC_Stake_delegation of { delegator : credential; pool : pool_keyhash }
  | CC_Pool_registration of pool_params
  | CC_Pool_retirement of { pool : pool_keyhash; epoch : epoch_number }
  | CC_Reg_cert of { credential : credential; deposit : coin }
  | CC_Unreg_cert of { credential : credential; deposit : coin }
  | CC_Vote_deleg_cert of { credential : credential; drep : drep }
  | CC_Stake_vote_deleg_cert of { credential : credential; pool : pool_keyhash; drep : drep }
  | CC_Stake_reg_deleg_cert of { credential : credential; pool : pool_keyhash; deposit : coin }
  | CC_Vote_reg_deleg_cert of { credential : credential; drep : drep; deposit : coin }
  | CC_Stake_vote_reg_deleg_cert of { credential : credential; pool : pool_keyhash; drep : drep; deposit : coin }
  | CC_Auth_committee_hot of { cold : credential; hot : credential }
  | CC_Resign_committee_cold of { cold : credential; anchor : anchor option }
  | CC_Reg_drep of { credential : credential; deposit : coin; anchor : anchor option }
  | CC_Unreg_drep of { credential : credential; deposit : coin }
  | CC_Update_drep of { credential : credential; anchor : anchor option }

type conway_tx_body = {
  cw_inputs : tx_in list;
  cw_outputs : babbage_tx_out list;
  cw_fee : coin;
  cw_ttl : slot_number option;
  cw_certs : conway_certificate list option;
  cw_withdrawals : withdrawals option;
  cw_auxiliary_data_hash : auxiliary_data_hash option;
  cw_validity_interval_start : slot_number option;
  cw_mint : multi_asset option;
  cw_script_data_hash : hash32 option;
  cw_collateral : tx_in list option;
  cw_required_signers : vkey_hash list option;
  cw_network_id : int64 option;
  cw_collateral_return : babbage_tx_out option;
  cw_total_collateral : coin option;
  cw_reference_inputs : tx_in list option;
  cw_voting_procedures : (voter * (governance_action_id * voting_procedure) list) list option;
  cw_proposal_procedures : proposal_procedure list option;
  cw_current_treasury_value : coin option;
  cw_donation : coin option;
}

type conway_tx_witness_set = {
  cw_vkey_witnesses : vkey_witness list option;
  cw_native_scripts : timelock list option;
  cw_bootstrap_witnesses : Cbor.cbor_value option;
  cw_plutus_v1_scripts : bytes list option;
  cw_plutus_data : plutus_data list option;
  cw_redeemers : redeemer list option;
  cw_plutus_v2_scripts : bytes list option;
  cw_plutus_v3_scripts : bytes list option;
}

type conway_tx = {
  conway_tx_body : conway_tx_body;
  conway_tx_witness_set : conway_tx_witness_set;
  conway_tx_is_valid : bool;
  conway_tx_auxiliary_data : Cbor.cbor_value option;
}

type conway_block = {
  cwb_header : babbage_header;
  cwb_tx_bodies : conway_tx_body list;
  cwb_tx_witness_sets : conway_tx_witness_set list;
  cwb_tx_metadata : (int * Cbor.cbor_value) list;
  cwb_invalid_txs : int list;
}

type block =
  | Byron_block of byron_block
  | Shelley_block of shelley_block
  | Allegra_block of allegra_block
  | Mary_block of mary_block
  | Alonzo_block of alonzo_block
  | Babbage_block of babbage_block
  | Conway_block of conway_block

(** {1 Byron CBOR encoding/decoding} *)

val encode_byron_address : byron_address -> Cbor.cbor_value
val decode_byron_address : Cbor.cbor_value -> (byron_address, string) result

val encode_byron_tx_in : byron_tx_in -> Cbor.cbor_value
val decode_byron_tx_in : Cbor.cbor_value -> (byron_tx_in, string) result

val encode_byron_tx_out : byron_tx_out -> Cbor.cbor_value
val decode_byron_tx_out : Cbor.cbor_value -> (byron_tx_out, string) result

val encode_byron_tx : byron_tx -> Cbor.cbor_value
val decode_byron_tx : Cbor.cbor_value -> (byron_tx, string) result

val encode_byron_consensus_data : byron_consensus_data -> Cbor.cbor_value
val decode_byron_consensus_data : Cbor.cbor_value -> (byron_consensus_data, string) result

val encode_byron_block_header : byron_block_header -> Cbor.cbor_value
val decode_byron_block_header : Cbor.cbor_value -> (byron_block_header, string) result

val encode_byron_block_body : byron_block_body -> Cbor.cbor_value
val decode_byron_block_body : Cbor.cbor_value -> (byron_block_body, string) result

val encode_byron_block : byron_block -> Cbor.cbor_value
val decode_byron_block : Cbor.cbor_value -> (byron_block, string) result

(** {1 Shelley address encoding (raw bytes)} *)

val encode_shelley_address : shelley_address -> bytes
val decode_shelley_address : bytes -> (shelley_address, string) result

(** {1 Shelley CBOR encoding/decoding} *)

val encode_rational : rational -> Cbor.cbor_value
val decode_rational : Cbor.cbor_value -> (rational, string) result

val encode_credential : credential -> Cbor.cbor_value
val decode_credential : Cbor.cbor_value -> (credential, string) result

val encode_tx_in : tx_in -> Cbor.cbor_value
val decode_tx_in : Cbor.cbor_value -> (tx_in, string) result

val encode_shelley_tx_out : shelley_tx_out -> Cbor.cbor_value
val decode_shelley_tx_out : Cbor.cbor_value -> (shelley_tx_out, string) result

val encode_pool_metadata : pool_metadata -> Cbor.cbor_value
val decode_pool_metadata : Cbor.cbor_value -> (pool_metadata, string) result

val encode_relay : relay -> Cbor.cbor_value
val decode_relay : Cbor.cbor_value -> (relay, string) result

val encode_certificate : certificate -> Cbor.cbor_value
val decode_certificate : Cbor.cbor_value -> (certificate, string) result

val encode_withdrawals : withdrawals -> Cbor.cbor_value
val decode_withdrawals : Cbor.cbor_value -> (withdrawals, string) result

val encode_protocol_param_update : protocol_param_update -> Cbor.cbor_value
val decode_protocol_param_update : Cbor.cbor_value -> (protocol_param_update, string) result

val empty_protocol_param_update : protocol_param_update

val encode_update : update -> Cbor.cbor_value
val decode_update : Cbor.cbor_value -> (update, string) result

val encode_shelley_tx_body : shelley_tx_body -> Cbor.cbor_value
val decode_shelley_tx_body : Cbor.cbor_value -> (shelley_tx_body, string) result

val encode_vkey_witness : vkey_witness -> Cbor.cbor_value
val decode_vkey_witness : Cbor.cbor_value -> (vkey_witness, string) result

val encode_multisig_script : multisig_script -> Cbor.cbor_value
val decode_multisig_script : Cbor.cbor_value -> (multisig_script, string) result

val encode_shelley_tx_witness_set : shelley_tx_witness_set -> Cbor.cbor_value
val decode_shelley_tx_witness_set : Cbor.cbor_value -> (shelley_tx_witness_set, string) result

val encode_vrf_cert : vrf_cert -> Cbor.cbor_value
val decode_vrf_cert : Cbor.cbor_value -> (vrf_cert, string) result

val encode_operational_cert : operational_cert -> Cbor.cbor_value list
val decode_operational_cert_flat : Cbor.cbor_value list -> (operational_cert, string) result

val encode_shelley_header_body : shelley_header_body -> Cbor.cbor_value
val decode_shelley_header_body : Cbor.cbor_value -> (shelley_header_body, string) result

val encode_shelley_header : shelley_header -> Cbor.cbor_value
val decode_shelley_header : Cbor.cbor_value -> (shelley_header, string) result

val encode_shelley_tx : shelley_tx -> Cbor.cbor_value
val decode_shelley_tx : Cbor.cbor_value -> (shelley_tx, string) result

val encode_shelley_block : shelley_block -> Cbor.cbor_value
val decode_shelley_block : Cbor.cbor_value -> (shelley_block, string) result

(** {1 Allegra CBOR encoding/decoding} *)

val encode_timelock : timelock -> Cbor.cbor_value
val decode_timelock : Cbor.cbor_value -> (timelock, string) result

val encode_allegra_tx_body : allegra_tx_body -> Cbor.cbor_value
val decode_allegra_tx_body : Cbor.cbor_value -> (allegra_tx_body, string) result

val encode_allegra_tx_witness_set : allegra_tx_witness_set -> Cbor.cbor_value
val decode_allegra_tx_witness_set : Cbor.cbor_value -> (allegra_tx_witness_set, string) result

val encode_allegra_tx : allegra_tx -> Cbor.cbor_value
val decode_allegra_tx : Cbor.cbor_value -> (allegra_tx, string) result

val encode_allegra_block : allegra_block -> Cbor.cbor_value
val decode_allegra_block : Cbor.cbor_value -> (allegra_block, string) result

(** {1 Mary CBOR encoding/decoding} *)

val encode_multi_asset : multi_asset -> Cbor.cbor_value
val decode_multi_asset : Cbor.cbor_value -> (multi_asset, string) result

val encode_value : value -> Cbor.cbor_value
val decode_value : Cbor.cbor_value -> (value, string) result

val encode_mary_tx_out : mary_tx_out -> Cbor.cbor_value
val decode_mary_tx_out : Cbor.cbor_value -> (mary_tx_out, string) result

val encode_mary_tx_body : mary_tx_body -> Cbor.cbor_value
val decode_mary_tx_body : Cbor.cbor_value -> (mary_tx_body, string) result

val encode_mary_tx_witness_set : mary_tx_witness_set -> Cbor.cbor_value
val decode_mary_tx_witness_set : Cbor.cbor_value -> (mary_tx_witness_set, string) result

val encode_mary_tx : mary_tx -> Cbor.cbor_value
val decode_mary_tx : Cbor.cbor_value -> (mary_tx, string) result

val encode_mary_block : mary_block -> Cbor.cbor_value
val decode_mary_block : Cbor.cbor_value -> (mary_block, string) result

(** {1 Alonzo CBOR encoding/decoding} *)

val encode_ex_units : ex_units -> Cbor.cbor_value
val decode_ex_units : Cbor.cbor_value -> (ex_units, string) result

val encode_redeemer_tag : redeemer_tag -> Cbor.cbor_value
val decode_redeemer_tag : Cbor.cbor_value -> (redeemer_tag, string) result

val encode_redeemer : redeemer -> Cbor.cbor_value
val decode_redeemer : Cbor.cbor_value -> (redeemer, string) result

val encode_alonzo_tx_out : alonzo_tx_out -> Cbor.cbor_value
val decode_alonzo_tx_out : Cbor.cbor_value -> (alonzo_tx_out, string) result

val encode_alonzo_tx_body : alonzo_tx_body -> Cbor.cbor_value
val decode_alonzo_tx_body : Cbor.cbor_value -> (alonzo_tx_body, string) result

val encode_alonzo_tx_witness_set : alonzo_tx_witness_set -> Cbor.cbor_value
val decode_alonzo_tx_witness_set : Cbor.cbor_value -> (alonzo_tx_witness_set, string) result

val encode_alonzo_tx : alonzo_tx -> Cbor.cbor_value
val decode_alonzo_tx : Cbor.cbor_value -> (alonzo_tx, string) result

val encode_alonzo_block : alonzo_block -> Cbor.cbor_value
val decode_alonzo_block : Cbor.cbor_value -> (alonzo_block, string) result

(** {1 Babbage CBOR encoding/decoding} *)

val encode_datum_option : datum_option -> Cbor.cbor_value
val decode_datum_option : Cbor.cbor_value -> (datum_option, string) result

val encode_script_ref : script_ref -> Cbor.cbor_value
val decode_script_ref : Cbor.cbor_value -> (script_ref, string) result

val encode_babbage_tx_out : babbage_tx_out -> Cbor.cbor_value
val decode_babbage_tx_out : Cbor.cbor_value -> (babbage_tx_out, string) result

val encode_babbage_header_body : babbage_header_body -> Cbor.cbor_value
val decode_babbage_header_body : Cbor.cbor_value -> (babbage_header_body, string) result

val encode_babbage_header : babbage_header -> Cbor.cbor_value
val decode_babbage_header : Cbor.cbor_value -> (babbage_header, string) result

val encode_babbage_tx_body : babbage_tx_body -> Cbor.cbor_value
val decode_babbage_tx_body : Cbor.cbor_value -> (babbage_tx_body, string) result

val encode_babbage_tx_witness_set : babbage_tx_witness_set -> Cbor.cbor_value
val decode_babbage_tx_witness_set : Cbor.cbor_value -> (babbage_tx_witness_set, string) result

val encode_babbage_tx : babbage_tx -> Cbor.cbor_value
val decode_babbage_tx : Cbor.cbor_value -> (babbage_tx, string) result

val encode_babbage_block : babbage_block -> Cbor.cbor_value
val decode_babbage_block : Cbor.cbor_value -> (babbage_block, string) result

(** {1 Conway CBOR encoding/decoding} *)

val encode_anchor : anchor -> Cbor.cbor_value
val decode_anchor : Cbor.cbor_value -> (anchor, string) result

val encode_drep : drep -> Cbor.cbor_value
val decode_drep : Cbor.cbor_value -> (drep, string) result

val encode_voter : voter -> Cbor.cbor_value
val decode_voter : Cbor.cbor_value -> (voter, string) result

val encode_vote : vote -> Cbor.cbor_value
val decode_vote : Cbor.cbor_value -> (vote, string) result

val encode_governance_action_id : governance_action_id -> Cbor.cbor_value
val decode_governance_action_id : Cbor.cbor_value -> (governance_action_id, string) result

val encode_voting_procedure : voting_procedure -> Cbor.cbor_value
val decode_voting_procedure : Cbor.cbor_value -> (voting_procedure, string) result

val encode_constitution : constitution -> Cbor.cbor_value
val decode_constitution : Cbor.cbor_value -> (constitution, string) result

val encode_governance_action : governance_action -> Cbor.cbor_value
val decode_governance_action : Cbor.cbor_value -> (governance_action, string) result

val encode_proposal_procedure : proposal_procedure -> Cbor.cbor_value
val decode_proposal_procedure : Cbor.cbor_value -> (proposal_procedure, string) result

val encode_conway_certificate : conway_certificate -> Cbor.cbor_value
val decode_conway_certificate : Cbor.cbor_value -> (conway_certificate, string) result

val encode_voting_procedures :
  (voter * (governance_action_id * voting_procedure) list) list -> Cbor.cbor_value
val decode_voting_procedures :
  Cbor.cbor_value -> ((voter * (governance_action_id * voting_procedure) list) list, string) result

val encode_conway_tx_body : conway_tx_body -> Cbor.cbor_value
val decode_conway_tx_body : Cbor.cbor_value -> (conway_tx_body, string) result

val encode_conway_tx_witness_set : conway_tx_witness_set -> Cbor.cbor_value
val decode_conway_tx_witness_set : Cbor.cbor_value -> (conway_tx_witness_set, string) result

val encode_conway_tx : conway_tx -> Cbor.cbor_value
val decode_conway_tx : Cbor.cbor_value -> (conway_tx, string) result

val encode_conway_block : conway_block -> Cbor.cbor_value
val decode_conway_block : Cbor.cbor_value -> (conway_block, string) result
