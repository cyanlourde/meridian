(* Cardano block and transaction types for Byron through Conway eras.

   References:
   - Byron CDDL:   cardano-ledger/eras/byron/cddl-spec/byron.cddl
   - Shelley CDDL: cardano-ledger/eras/shelley/impl/cddl-files/shelley.cddl
   - Allegra CDDL: cardano-ledger/eras/allegra/impl/cddl-files/allegra.cddl
   - Mary CDDL:    cardano-ledger/eras/mary/impl/cddl-files/mary.cddl
   - Alonzo CDDL:  cardano-ledger/eras/alonzo/impl/cddl-files/alonzo.cddl
   - Babbage CDDL:  cardano-ledger/eras/babbage/impl/cddl-files/babbage.cddl
   - Conway CDDL:  cardano-ledger/eras/conway/impl/cddl-files/conway.cddl
   - Shelley formal ledger spec (SL-D5) *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Common types                                                      *)
(* ================================================================ *)

(** 28-byte hash — Blake2b-224, used for key hashes and script hashes *)
type hash28 = bytes

(** 32-byte hash — Blake2b-256, used for block hashes, tx hashes *)
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

type vkey = bytes           (* 32-byte Ed25519 verification key *)
type signature = bytes      (* 64-byte Ed25519 signature *)
type vrf_vkey = bytes       (* 32-byte VRF verification key *)
type kes_vkey = bytes       (* 32-byte KES verification key *)
type kes_signature = bytes  (* KES signature *)

(** VRF certificate: [output, proof] per CDDL vrf_cert = [bytes, bytes] *)
type vrf_cert = {
  vrf_output : bytes;
  vrf_proof : bytes;
}

type slot_number = int64
type epoch_number = int64
type block_number = int64
type coin = int64

(** Rational number, encoded as #6.30([numerator, denominator]) *)
type rational = {
  numerator : int64;
  denominator : int64;
}

(** Unit interval — rational in [0, 1] *)
type unit_interval = rational

(* ================================================================ *)
(* Byron era types                                                   *)
(* ================================================================ *)

(** Byron address (CDDL: #6.24(bytes .cbor [root, attributes, type]))
    - address_root: Blake2b-224 hash of [type, spending_data, attributes]
    - address_attributes: map (may contain derivation path key 1, network magic key 2)
    - address_type: 0 = pubkey, 1 = script, 2 = redeem *)
type byron_address = {
  address_root : hash28;
  address_attributes : Cbor.cbor_value;
  address_type : int;
}

(** Byron transaction input (CDDL: [0, [tx_id, index]]) *)
type byron_tx_in = {
  byron_txin_id : tx_hash;
  byron_txin_index : int64;
}

(** Byron transaction output (CDDL: [address, amount]) *)
type byron_tx_out = {
  byron_txout_address : byron_address;
  byron_txout_amount : coin;
}

(** Byron transaction body (CDDL: [inputs, outputs, attributes]) *)
type byron_tx = {
  byron_tx_inputs : byron_tx_in list;
  byron_tx_outputs : byron_tx_out list;
  byron_tx_attributes : Cbor.cbor_value;
}

(** Byron consensus data within block header
    CDDL: [slot_id, pubkey, difficulty, block_signature]
    where slot_id = [epoch, slot_within_epoch]
    and difficulty = [chain_difficulty] *)
type byron_consensus_data = {
  byron_epoch : epoch_number;
  byron_slot_in_epoch : slot_number;
  byron_issuer : bytes;             (* extended public key, 64 bytes in Byron *)
  byron_difficulty : int64;
  byron_signature : Cbor.cbor_value; (* delegation/signature structure *)
}

(** Byron block header
    CDDL: [protocol_magic, prev_block, body_proof, consensus_data, extra_data] *)
type byron_block_header = {
  byron_protocol_magic : int64;
  byron_prev_block : block_hash;
  byron_body_proof : Cbor.cbor_value;
  byron_consensus : byron_consensus_data;
  byron_extra_data : Cbor.cbor_value;
}

(** Byron block body — payloads other than transactions kept opaque
    CDDL: [tx_payload, ssc_payload, dlg_payload, upd_payload] *)
type byron_block_body = {
  byron_tx_payload : Cbor.cbor_value;
  byron_ssc_payload : Cbor.cbor_value;
  byron_dlg_payload : Cbor.cbor_value;
  byron_upd_payload : Cbor.cbor_value;
}

(** Byron main block (CDDL: [header, body, extra]) *)
type byron_block = {
  byron_header : byron_block_header;
  byron_body : byron_block_body;
  byron_extra : Cbor.cbor_value list;
}

(* ================================================================ *)
(* Shelley era types                                                 *)
(* ================================================================ *)

(** Stake credential (CDDL: [0, addr_keyhash] / [1, script_hash]) *)
type credential =
  | Key_hash of vkey_hash
  | Script_hash of script_hash

(** Pointer for pointer addresses — slot, tx_index, cert_index
    encoded as variable-length naturals in address bytes *)
type stake_pointer = {
  slot : slot_number;
  tx_index : int64;
  cert_index : int64;
}

(** Shelley-era address variants, parsed from raw address bytes.
    Header byte layout: bits 7-4 = type, bits 3-0 = network id.
    Type 0-3: base address (payment + stake credentials)
    Type 4-5: pointer address (payment + chain pointer)
    Type 6-7: enterprise address (payment only)
    Type 8:   Byron compatibility
    Type 14-15: reward account (stake only) *)
type shelley_address =
  | Base_address of { network : int; payment : credential; stake : credential }
  | Pointer_address of { network : int; payment : credential; pointer : stake_pointer }
  | Enterprise_address of { network : int; payment : credential }
  | Reward_address of { network : int; stake : credential }
  | Byron_compat_address of bytes

(** Shelley transaction input (CDDL: [transaction_id, index]) *)
type tx_in = {
  tx_id : tx_hash;
  tx_index : int64;
}

(** Shelley transaction output (CDDL: [address, amount]) *)
type shelley_tx_out = {
  address : bytes;
  amount : coin;
}

(** Withdrawals: reward_account bytes -> coin *)
type withdrawals = (bytes * coin) list

(** Pool relay (CDDL: [0, port/null, ipv4/null, ipv6/null]
                     / [1, port/null, dns_name]
                     / [2, dns_name]) *)
type relay =
  | Single_host_addr of { port : int option; ipv4 : bytes option; ipv6 : bytes option }
  | Single_host_name of { port : int option; dns_name : string }
  | Multi_host_name of { dns_name : string }

(** Pool metadata (CDDL: [url, metadata_hash]) *)
type pool_metadata = {
  pool_url : string;
  pool_metadata_hash : hash32;
}

(** Pool registration parameters — flattened into certificate array
    CDDL: (operator, vrf_keyhash, pledge, cost, margin,
            reward_account, pool_owners, relays, pool_metadata/null) *)
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

(** Shelley certificate (CDDL: [type_tag, ...params]) *)
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

(** Protocol parameter update — CBOR map with optional fields keyed 0-15 *)
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

(** Proposed protocol updates: genesis_hash -> param_update *)
type proposed_protocol_updates = (genesis_hash * protocol_param_update) list

(** Update payload (CDDL: [proposed_updates, epoch]) *)
type update = {
  proposed_updates : proposed_protocol_updates;
  update_epoch : epoch_number;
}

(** Shelley transaction body — CBOR map with keys 0-7
    { 0: set<tx_in>, 1: [*tx_out], 2: fee, 3: ttl,
      ?4: [*certificate], ?5: withdrawals, ?6: update, ?7: aux_data_hash } *)
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

(** VKey witness (CDDL: [vkey, signature]) *)
type vkey_witness = {
  witness_vkey : vkey;
  witness_sig : signature;
}

(** Multisig native script (Shelley era)
    CDDL: [0, keyhash] / [1, [*script]] / [2, [*script]] / [3, n, [*script]] *)
type multisig_script =
  | Sig of vkey_hash
  | All_of of multisig_script list
  | Any_of of multisig_script list
  | N_of of int * multisig_script list

(** Shelley transaction witness set — CBOR map with optional fields
    { ?0: [*vkey_witness], ?1: [*multisig_script], ?2: [*bootstrap_witness] } *)
type shelley_tx_witness_set = {
  vkey_witnesses : vkey_witness list option;
  multisig_scripts : multisig_script list option;
  bootstrap_witnesses : Cbor.cbor_value option;
}

(** Shelley transaction: [body, witness_set, metadata/null] *)
type shelley_tx = {
  shelley_tx_body : shelley_tx_body;
  shelley_tx_witness_set : shelley_tx_witness_set;
  shelley_tx_metadata : Cbor.cbor_value option;
}

(** Operational certificate — fields inlined into header body array
    CDDL: (hot_vkey, sequence_number, kes_period, sigma) *)
type operational_cert = {
  hot_vkey : kes_vkey;
  sequence_number : int64;
  kes_period : int64;
  sigma : signature;
}

(** Protocol version: (major, minor) inlined into header body array *)
type protocol_version = {
  proto_major : int64;
  proto_minor : int64;
}

(** Shelley header body — CBOR array with 15 elements
    CDDL: [block_number, slot, prev_hash/null, issuer_vkey, vrf_vkey,
            nonce_vrf, leader_vrf, body_size, body_hash,
            hot_vkey, seq_num, kes_period, sigma,
            proto_major, proto_minor] *)
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

(** Shelley header (CDDL: [header_body, body_signature]) *)
type shelley_header = {
  sh_header_body : shelley_header_body;
  sh_body_signature : kes_signature;
}

(** Shelley block
    CDDL: [header, [*tx_body], [*tx_witness_set], {*uint => metadata}] *)
type shelley_block = {
  sb_header : shelley_header;
  sb_tx_bodies : shelley_tx_body list;
  sb_tx_witness_sets : shelley_tx_witness_set list;
  sb_tx_metadata : (int * Cbor.cbor_value) list;
}

(* ================================================================ *)
(* Allegra era types                                                 *)
(* ================================================================ *)

(* Timelock native scripts - replaces multisig with time bounds *)
type timelock =
  | TL_Signature of vkey_hash          (* 0 *)
  | TL_AllOf of timelock list           (* 1 *)
  | TL_AnyOf of timelock list           (* 2 *)
  | TL_MOfN of int * timelock list      (* 3 *)
  | TL_InvalidBefore of slot_number     (* 4 *)
  | TL_InvalidHereafter of slot_number  (* 5 *)

(* Allegra tx body: ttl becomes optional, adds validity_interval_start (key 8) *)
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

(* ================================================================ *)
(* Mary era types                                                    *)
(* ================================================================ *)

(* Multi-asset: policy_id -> [(asset_name, quantity)] *)
type multi_asset = (script_hash * (bytes * int64) list) list

type value =
  | Lovelace of coin
  | Lovelace_and_assets of coin * multi_asset

type mary_tx_out = {
  ma_address : bytes;
  ma_value : value;
}

(* Mary tx body: adds mint field (key 9) *)
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

(* Mary uses same witness set as Allegra *)
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

(* ================================================================ *)
(* Alonzo era types                                                  *)
(* ================================================================ *)

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

(* Alonzo tx body: adds script_data_hash (11), collateral (13), required_signers (14), network_id (15) *)
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

(* Witness set adds Plutus V1 scripts (3), plutus data (4), redeemers (5) *)
type alonzo_tx_witness_set = {
  az_vkey_witnesses : vkey_witness list option;
  az_native_scripts : timelock list option;
  az_bootstrap_witnesses : Cbor.cbor_value option;
  az_plutus_v1_scripts : bytes list option;
  az_plutus_data : plutus_data list option;
  az_redeemers : redeemer list option;
}

(* Alonzo tx has 4 elements: adds is_valid bool *)
type alonzo_tx = {
  alonzo_tx_body : alonzo_tx_body;
  alonzo_tx_witness_set : alonzo_tx_witness_set;
  alonzo_tx_is_valid : bool;
  alonzo_tx_auxiliary_data : Cbor.cbor_value option;
}

(* Alonzo block: 5 elements, adds invalid_txs *)
type alonzo_block = {
  azb_header : shelley_header;
  azb_tx_bodies : alonzo_tx_body list;
  azb_tx_witness_sets : alonzo_tx_witness_set list;
  azb_tx_metadata : (int * Cbor.cbor_value) list;
  azb_invalid_txs : int list;
}

(* ================================================================ *)
(* Babbage era types                                                 *)
(* ================================================================ *)

type datum_option =
  | Datum_hash of hash32
  | Inline_datum of plutus_data

type script_ref =
  | Native_script_ref of timelock
  | Plutus_v1_script_ref of bytes
  | Plutus_v2_script_ref of bytes
  | Plutus_v3_script_ref of bytes  (* for forward compat with Conway *)

(* Babbage tx output is a map: {0: address, 1: value, ?2: datum_option, ?3: script_ref} *)
type babbage_tx_out = {
  bb_address : bytes;
  bb_value : value;
  bb_datum_option : datum_option option;
  bb_script_ref : script_ref option;
}

(* Babbage header: single VRF result instead of nonce+leader (14 elements) *)
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

(* Babbage tx body: adds collateral_return (16), total_collateral (17), reference_inputs (18) *)
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

(* Witness set adds Plutus V2 scripts (key 6) *)
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

(* ================================================================ *)
(* Conway era types                                                  *)
(* ================================================================ *)

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

(* Conway certificates: legacy 0-4, new 7-18 *)
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

(* Conway tx body: removes update (key 6), adds voting (19), proposals (20), treasury (21), donation (22) *)
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

(* Conway witness set adds Plutus V3 (key 7) *)
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

(** Era-tagged block *)
type block =
  | Byron_block of byron_block
  | Shelley_block of shelley_block
  | Allegra_block of allegra_block
  | Mary_block of mary_block
  | Alonzo_block of alonzo_block
  | Babbage_block of babbage_block
  | Conway_block of conway_block

(* ================================================================ *)
(* CBOR encoding/decoding helpers                                    *)
(* ================================================================ *)

let list_map_result f items =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs ->
      match f x with
      | Ok v -> go (v :: acc) xs
      | Error _ as e -> e
  in
  go [] items

(** Look up an integer key in a CBOR map *)
let find_map_entry key pairs =
  List.find_map (fun (k, v) ->
    match k with
    | Cbor.Uint n when Int64.equal n (Int64.of_int key) -> Some v
    | _ -> None
  ) pairs

let decode_uint = function
  | Cbor.Uint n -> Ok n
  | _ -> Error "expected uint"

let decode_bytes = function
  | Cbor.Bytes b -> Ok b
  | _ -> Error "expected bytes"

let decode_hash28 = function
  | Cbor.Bytes b when Bytes.length b = 28 -> Ok b
  | Cbor.Bytes b -> Error (Printf.sprintf "expected 28 bytes, got %d" (Bytes.length b))
  | _ -> Error "expected bytes (hash28)"

let decode_hash32 = function
  | Cbor.Bytes b when Bytes.length b = 32 -> Ok b
  | Cbor.Bytes b -> Error (Printf.sprintf "expected 32 bytes, got %d" (Bytes.length b))
  | _ -> Error "expected bytes (hash32)"

(* Keep alias for backward compatibility *)
let _decode_hash32 = decode_hash32

(* ================================================================ *)
(* Rational number encoding                                          *)
(* ================================================================ *)

let encode_rational r =
  Cbor.Tag (30L, Cbor.Array [Cbor.Uint r.numerator; Cbor.Uint r.denominator])

let decode_rational = function
  | Cbor.Tag (30L, Cbor.Array [Cbor.Uint n; Cbor.Uint d]) ->
    Ok { numerator = n; denominator = d }
  | _ -> Error "rational: expected #6.30([uint, uint])"

(* ================================================================ *)
(* Byron CBOR encoding/decoding                                      *)
(* ================================================================ *)

let encode_byron_address addr =
  let inner = Cbor.Array [
    Cbor.Bytes addr.address_root;
    addr.address_attributes;
    Cbor.Uint (Int64.of_int addr.address_type);
  ] in
  let inner_bytes = Cbor.encode inner in
  Cbor.Tag (24L, Cbor.Bytes inner_bytes)

let decode_byron_address = function
  | Cbor.Tag (24L, Cbor.Bytes inner_bytes) ->
    let* inner = Cbor.decode inner_bytes in
    (match inner with
     | Cbor.Array [Cbor.Bytes root; attrs; Cbor.Uint addr_type]
       when Bytes.length root = 28 ->
       Ok { address_root = root;
            address_attributes = attrs;
            address_type = Int64.to_int addr_type }
     | _ -> Error "byron_address: invalid inner structure")
  | _ -> Error "byron_address: expected #6.24(bytes)"

let encode_byron_tx_in inp =
  Cbor.Array [
    Cbor.Uint 0L;
    Cbor.Tag (24L, Cbor.Bytes (Cbor.encode
      (Cbor.Array [Cbor.Bytes inp.byron_txin_id; Cbor.Uint inp.byron_txin_index])))
  ]

let decode_byron_tx_in = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Tag (24L, Cbor.Bytes inner_bytes)] ->
    let* inner = Cbor.decode inner_bytes in
    (match inner with
     | Cbor.Array [Cbor.Bytes tx_id; Cbor.Uint idx] when Bytes.length tx_id = 32 ->
       Ok { byron_txin_id = tx_id; byron_txin_index = idx }
     | _ -> Error "byron_tx_in: invalid inner structure")
  | _ -> Error "byron_tx_in: expected [0, #6.24(bytes)]"

let encode_byron_tx_out out =
  Cbor.Array [
    encode_byron_address out.byron_txout_address;
    Cbor.Uint out.byron_txout_amount;
  ]

let decode_byron_tx_out = function
  | Cbor.Array [addr_cbor; Cbor.Uint amount] ->
    let* addr = decode_byron_address addr_cbor in
    Ok { byron_txout_address = addr; byron_txout_amount = amount }
  | _ -> Error "byron_tx_out: expected [address, uint]"

let encode_byron_tx tx =
  Cbor.Array [
    Cbor.Array (List.map encode_byron_tx_in tx.byron_tx_inputs);
    Cbor.Array (List.map encode_byron_tx_out tx.byron_tx_outputs);
    tx.byron_tx_attributes;
  ]

let decode_byron_tx = function
  | Cbor.Array [Cbor.Array inputs; Cbor.Array outputs; attrs] ->
    let* inputs = list_map_result decode_byron_tx_in inputs in
    let* outputs = list_map_result decode_byron_tx_out outputs in
    Ok { byron_tx_inputs = inputs;
         byron_tx_outputs = outputs;
         byron_tx_attributes = attrs }
  | _ -> Error "byron_tx: expected [inputs, outputs, attributes]"

let encode_byron_consensus_data cd =
  Cbor.Array [
    Cbor.Array [Cbor.Uint cd.byron_epoch; Cbor.Uint cd.byron_slot_in_epoch];
    Cbor.Bytes cd.byron_issuer;
    Cbor.Array [Cbor.Uint cd.byron_difficulty];
    cd.byron_signature;
  ]

let decode_byron_consensus_data = function
  | Cbor.Array [Cbor.Array [Cbor.Uint epoch; Cbor.Uint slot];
                Cbor.Bytes issuer;
                Cbor.Array [Cbor.Uint difficulty];
                signature] ->
    Ok { byron_epoch = epoch;
         byron_slot_in_epoch = slot;
         byron_issuer = issuer;
         byron_difficulty = difficulty;
         byron_signature = signature }
  | _ -> Error "byron_consensus_data: invalid structure"

let encode_byron_block_header hdr =
  Cbor.Array [
    Cbor.Uint hdr.byron_protocol_magic;
    Cbor.Bytes hdr.byron_prev_block;
    hdr.byron_body_proof;
    encode_byron_consensus_data hdr.byron_consensus;
    hdr.byron_extra_data;
  ]

let decode_byron_block_header = function
  | Cbor.Array [Cbor.Uint magic; Cbor.Bytes prev; proof; consensus; extra] ->
    let* consensus = decode_byron_consensus_data consensus in
    Ok { byron_protocol_magic = magic;
         byron_prev_block = prev;
         byron_body_proof = proof;
         byron_consensus = consensus;
         byron_extra_data = extra }
  | _ -> Error "byron_block_header: expected 5-element array"

let encode_byron_block_body body =
  Cbor.Array [
    body.byron_tx_payload;
    body.byron_ssc_payload;
    body.byron_dlg_payload;
    body.byron_upd_payload;
  ]

let decode_byron_block_body = function
  | Cbor.Array [tx; ssc; dlg; upd] ->
    Ok { byron_tx_payload = tx;
         byron_ssc_payload = ssc;
         byron_dlg_payload = dlg;
         byron_upd_payload = upd }
  | _ -> Error "byron_block_body: expected 4-element array"

let encode_byron_block block =
  Cbor.Array [
    encode_byron_block_header block.byron_header;
    encode_byron_block_body block.byron_body;
    Cbor.Array block.byron_extra;
  ]

let decode_byron_block = function
  | Cbor.Array [header; body; Cbor.Array extra] ->
    let* header = decode_byron_block_header header in
    let* body = decode_byron_block_body body in
    Ok { byron_header = header; byron_body = body; byron_extra = extra }
  | _ -> Error "byron_block: expected [header, body, extra]"

(* ================================================================ *)
(* Shelley address encoding (raw bytes, not CBOR)                    *)
(* ================================================================ *)

(** Encode a variable-length natural number (used in pointer addresses).
    Big-endian, 7 bits per byte, high bit = continuation. *)
let encode_var_nat buf n =
  let rec byte_count n =
    if Int64.compare n 128L < 0 then 1
    else 1 + byte_count (Int64.shift_right_logical n 7)
  in
  let count = byte_count n in
  let tmp = Bytes.create count in
  let rec fill i n =
    if i < 0 then ()
    else begin
      let b = Int64.to_int (Int64.logand n 0x7FL) in
      let b = if i < count - 1 then b lor 0x80 else b in
      Bytes.set_uint8 tmp i b;
      fill (i - 1) (Int64.shift_right_logical n 7)
    end
  in
  fill (count - 1) n;
  Buffer.add_bytes buf tmp

(** Decode a variable-length natural number from raw bytes at given position. *)
let decode_var_nat data pos =
  let len = Bytes.length data in
  let rec go pos acc =
    if pos >= len then Error "unexpected end of variable-length natural"
    else
      let b = Bytes.get_uint8 data pos in
      let acc = Int64.logor (Int64.shift_left acc 7) (Int64.of_int (b land 0x7F)) in
      if b land 0x80 = 0 then Ok (acc, pos + 1)
      else go (pos + 1) acc
  in
  go pos 0L

let credential_of_header_and_bytes header_type hash =
  if header_type = 0 then Key_hash hash
  else Script_hash hash

let encode_shelley_address addr =
  match addr with
  | Base_address { network; payment; stake } ->
    let pay_type = (match payment with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let stk_type = (match stake with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let addr_type = (pay_type * 2) lor stk_type in
    let header = (addr_type lsl 4) lor (network land 0x0F) in
    let buf = Buffer.create 57 in
    Buffer.add_uint8 buf header;
    (match payment with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    (match stake with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    Buffer.to_bytes buf
  | Pointer_address { network; payment; pointer } ->
    let pay_type = (match payment with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let addr_type = 4 + pay_type in
    let header = (addr_type lsl 4) lor (network land 0x0F) in
    let buf = Buffer.create 40 in
    Buffer.add_uint8 buf header;
    (match payment with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    encode_var_nat buf pointer.slot;
    encode_var_nat buf pointer.tx_index;
    encode_var_nat buf pointer.cert_index;
    Buffer.to_bytes buf
  | Enterprise_address { network; payment } ->
    let pay_type = (match payment with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let addr_type = 6 + pay_type in
    let header = (addr_type lsl 4) lor (network land 0x0F) in
    let buf = Buffer.create 29 in
    Buffer.add_uint8 buf header;
    (match payment with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    Buffer.to_bytes buf
  | Reward_address { network; stake } ->
    let stk_type = (match stake with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let addr_type = 14 + stk_type in
    let header = (addr_type lsl 4) lor (network land 0x0F) in
    let buf = Buffer.create 29 in
    Buffer.add_uint8 buf header;
    (match stake with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    Buffer.to_bytes buf
  | Byron_compat_address raw -> raw

let decode_shelley_address data =
  let len = Bytes.length data in
  if len < 1 then Error "address: empty"
  else
    let header = Bytes.get_uint8 data 0 in
    let addr_type = header lsr 4 in
    let network = header land 0x0F in
    match addr_type with
    | 0 | 1 | 2 | 3 when len = 57 ->
      let pay_hash = Bytes.sub data 1 28 in
      let stk_hash = Bytes.sub data 29 28 in
      let payment = credential_of_header_and_bytes (addr_type lsr 1) pay_hash in
      let stake = credential_of_header_and_bytes (addr_type land 1) stk_hash in
      Ok (Base_address { network; payment; stake })
    | 4 | 5 when len >= 30 ->
      let pay_hash = Bytes.sub data 1 28 in
      let payment = credential_of_header_and_bytes (addr_type - 4) pay_hash in
      let* slot, pos = decode_var_nat data 29 in
      let* tx_index, pos = decode_var_nat data pos in
      let* cert_index, _pos = decode_var_nat data pos in
      Ok (Pointer_address { network; payment;
                            pointer = { slot; tx_index; cert_index } })
    | 6 | 7 when len = 29 ->
      let pay_hash = Bytes.sub data 1 28 in
      let payment = credential_of_header_and_bytes (addr_type - 6) pay_hash in
      Ok (Enterprise_address { network; payment })
    | 8 ->
      Ok (Byron_compat_address data)
    | 14 | 15 when len = 29 ->
      let stk_hash = Bytes.sub data 1 28 in
      let stake = credential_of_header_and_bytes (addr_type - 14) stk_hash in
      Ok (Reward_address { network; stake })
    | _ -> Error (Printf.sprintf "address: unsupported type %d" addr_type)

(* ================================================================ *)
(* Shelley CBOR encoding/decoding                                    *)
(* ================================================================ *)

(* -- Credential -- *)

let encode_credential = function
  | Key_hash h -> Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h]
  | Script_hash h -> Cbor.Array [Cbor.Uint 1L; Cbor.Bytes h]

let decode_credential = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Key_hash h)
  | Cbor.Array [Cbor.Uint 1L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Script_hash h)
  | _ -> Error "credential: expected [0, hash28] or [1, hash28]"

(* -- Transaction input -- *)

let encode_tx_in inp =
  Cbor.Array [Cbor.Bytes inp.tx_id; Cbor.Uint inp.tx_index]

let decode_tx_in = function
  | Cbor.Array [Cbor.Bytes tx_id; Cbor.Uint idx] when Bytes.length tx_id = 32 ->
    Ok { tx_id; tx_index = idx }
  | _ -> Error "tx_in: expected [hash32, uint]"

(* -- Transaction output -- *)

let encode_shelley_tx_out out =
  Cbor.Array [Cbor.Bytes out.address; Cbor.Uint out.amount]

let decode_shelley_tx_out = function
  | Cbor.Array [Cbor.Bytes addr; Cbor.Uint amount] ->
    Ok { address = addr; amount }
  | _ -> Error "shelley_tx_out: expected [bytes, uint]"

(* -- Pool metadata -- *)

let encode_pool_metadata md =
  Cbor.Array [Cbor.Text md.pool_url; Cbor.Bytes md.pool_metadata_hash]

let decode_pool_metadata = function
  | Cbor.Array [Cbor.Text url; Cbor.Bytes hash] when Bytes.length hash = 32 ->
    Ok { pool_url = url; pool_metadata_hash = hash }
  | _ -> Error "pool_metadata: expected [text, hash32]"

(* -- Relay -- *)

let encode_relay = function
  | Single_host_addr { port; ipv4; ipv6 } ->
    Cbor.Array [
      Cbor.Uint 0L;
      (match port with Some p -> Cbor.Uint (Int64.of_int p) | None -> Cbor.Null);
      (match ipv4 with Some b -> Cbor.Bytes b | None -> Cbor.Null);
      (match ipv6 with Some b -> Cbor.Bytes b | None -> Cbor.Null);
    ]
  | Single_host_name { port; dns_name } ->
    Cbor.Array [
      Cbor.Uint 1L;
      (match port with Some p -> Cbor.Uint (Int64.of_int p) | None -> Cbor.Null);
      Cbor.Text dns_name;
    ]
  | Multi_host_name { dns_name } ->
    Cbor.Array [Cbor.Uint 2L; Cbor.Text dns_name]

let decode_relay = function
  | Cbor.Array [Cbor.Uint 0L; port; ipv4; ipv6] ->
    let port = match port with Cbor.Uint p -> Some (Int64.to_int p) | _ -> None in
    let ipv4 = match ipv4 with Cbor.Bytes b -> Some b | _ -> None in
    let ipv6 = match ipv6 with Cbor.Bytes b -> Some b | _ -> None in
    Ok (Single_host_addr { port; ipv4; ipv6 })
  | Cbor.Array [Cbor.Uint 1L; port; Cbor.Text dns] ->
    let port = match port with Cbor.Uint p -> Some (Int64.to_int p) | _ -> None in
    Ok (Single_host_name { port; dns_name = dns })
  | Cbor.Array [Cbor.Uint 2L; Cbor.Text dns] ->
    Ok (Multi_host_name { dns_name = dns })
  | _ -> Error "relay: unrecognized structure"

(* -- Pool params (flattened for certificate encoding) -- *)

let encode_pool_params_flat params =
  [ Cbor.Bytes params.pool_operator;
    Cbor.Bytes params.pool_vrf_keyhash;
    Cbor.Uint params.pool_pledge;
    Cbor.Uint params.pool_cost;
    encode_rational params.pool_margin;
    Cbor.Bytes params.pool_reward_account;
    Cbor.Array (List.map (fun h -> Cbor.Bytes h) params.pool_owners);
    Cbor.Array (List.map encode_relay params.pool_relays);
    (match params.pool_metadata with
     | None -> Cbor.Null
     | Some md -> encode_pool_metadata md) ]

let decode_pool_params_flat = function
  | [Cbor.Bytes operator; Cbor.Bytes vrf_kh; Cbor.Uint pledge; Cbor.Uint cost;
     margin_cbor; Cbor.Bytes reward_acct; Cbor.Array owners_cbor;
     Cbor.Array relays_cbor; metadata_cbor]
    when Bytes.length operator = 28 && Bytes.length vrf_kh = 32 ->
    let* margin = decode_rational margin_cbor in
    let* pool_owners = list_map_result (function
      | Cbor.Bytes h when Bytes.length h = 28 -> Ok h
      | _ -> Error "pool_params: invalid owner hash"
    ) owners_cbor in
    let* pool_relays = list_map_result decode_relay relays_cbor in
    let* pool_metadata = match metadata_cbor with
      | Cbor.Null -> Ok None
      | v -> let* md = decode_pool_metadata v in Ok (Some md)
    in
    Ok { pool_operator = operator;
         pool_vrf_keyhash = vrf_kh;
         pool_pledge = pledge;
         pool_cost = cost;
         pool_margin = margin;
         pool_reward_account = reward_acct;
         pool_owners;
         pool_relays;
         pool_metadata }
  | _ -> Error "pool_params: invalid flat structure"

(* -- Certificate -- *)

let encode_certificate = function
  | Stake_registration cred ->
    Cbor.Array [Cbor.Uint 0L; encode_credential cred]
  | Stake_deregistration cred ->
    Cbor.Array [Cbor.Uint 1L; encode_credential cred]
  | Stake_delegation { delegator; pool } ->
    Cbor.Array [Cbor.Uint 2L; encode_credential delegator; Cbor.Bytes pool]
  | Pool_registration params ->
    Cbor.Array (Cbor.Uint 3L :: encode_pool_params_flat params)
  | Pool_retirement { pool; epoch } ->
    Cbor.Array [Cbor.Uint 4L; Cbor.Bytes pool; Cbor.Uint epoch]
  | Genesis_key_delegation { genesis_hash; delegate_hash; vrf_keyhash } ->
    Cbor.Array [Cbor.Uint 5L; Cbor.Bytes genesis_hash;
                Cbor.Bytes delegate_hash; Cbor.Bytes vrf_keyhash]
  | Move_instantaneous_rewards v ->
    Cbor.Array [Cbor.Uint 6L; v]

let decode_certificate = function
  | Cbor.Array [Cbor.Uint 0L; cred] ->
    let* c = decode_credential cred in Ok (Stake_registration c)
  | Cbor.Array [Cbor.Uint 1L; cred] ->
    let* c = decode_credential cred in Ok (Stake_deregistration c)
  | Cbor.Array [Cbor.Uint 2L; cred; Cbor.Bytes pool]
    when Bytes.length pool = 28 ->
    let* c = decode_credential cred in
    Ok (Stake_delegation { delegator = c; pool })
  | Cbor.Array (Cbor.Uint 3L :: rest) when List.length rest = 9 ->
    let* params = decode_pool_params_flat rest in
    Ok (Pool_registration params)
  | Cbor.Array [Cbor.Uint 4L; Cbor.Bytes pool; Cbor.Uint epoch]
    when Bytes.length pool = 28 ->
    Ok (Pool_retirement { pool; epoch })
  | Cbor.Array [Cbor.Uint 5L; Cbor.Bytes gh; Cbor.Bytes dh; Cbor.Bytes vrf]
    when Bytes.length gh = 28 && Bytes.length dh = 28 && Bytes.length vrf = 32 ->
    Ok (Genesis_key_delegation { genesis_hash = gh; delegate_hash = dh; vrf_keyhash = vrf })
  | Cbor.Array [Cbor.Uint 6L; v] ->
    Ok (Move_instantaneous_rewards v)
  | _ -> Error "certificate: unrecognized structure"

(* -- Withdrawals -- *)

let encode_withdrawals wds =
  Cbor.Map (List.map (fun (acct, amount) ->
    (Cbor.Bytes acct, Cbor.Uint amount)) wds)

let decode_withdrawals = function
  | Cbor.Map pairs ->
    list_map_result (fun (k, v) ->
      let* acct = decode_bytes k in
      let* amount = decode_uint v in
      Ok (acct, amount)
    ) pairs
  | _ -> Error "withdrawals: expected map"

(* -- Protocol parameter update -- *)

let encode_protocol_param_update p =
  let opt_uint key v =
    match v with None -> [] | Some n -> [(Cbor.Uint (Int64.of_int key), Cbor.Uint n)]
  in
  let opt_rational key v =
    match v with None -> [] | Some r -> [(Cbor.Uint (Int64.of_int key), encode_rational r)]
  in
  let entries =
    opt_uint 0 p.min_fee_a
    @ opt_uint 1 p.min_fee_b
    @ opt_uint 2 p.max_block_body_size
    @ opt_uint 3 p.max_tx_size
    @ opt_uint 4 p.max_block_header_size
    @ opt_uint 5 p.key_deposit
    @ opt_uint 6 p.pool_deposit
    @ opt_uint 7 p.max_epoch
    @ opt_uint 8 p.n_opt
    @ opt_rational 9 p.pool_pledge_influence
    @ opt_rational 10 p.expansion_rate
    @ opt_rational 11 p.treasury_growth_rate
    @ opt_rational 12 p.decentralization_param
    @ (match p.extra_entropy with
       | None -> []
       | Some v -> [(Cbor.Uint 13L, v)])
    @ (match p.protocol_version with
       | None -> []
       | Some (maj, min) ->
         [(Cbor.Uint 14L, Cbor.Array [Cbor.Uint maj; Cbor.Uint min])])
    @ opt_uint 15 p.min_utxo_value
  in
  Cbor.Map entries

let decode_protocol_param_update = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let opt_uint key =
      match find key with
      | Some (Cbor.Uint n) -> Ok (Some n)
      | None -> Ok None
      | _ -> Error (Printf.sprintf "protocol_param_update: key %d: expected uint" key)
    in
    let opt_rational key =
      match find key with
      | Some v -> let* r = decode_rational v in Ok (Some r)
      | None -> Ok None
    in
    let* min_fee_a = opt_uint 0 in
    let* min_fee_b = opt_uint 1 in
    let* max_block_body_size = opt_uint 2 in
    let* max_tx_size = opt_uint 3 in
    let* max_block_header_size = opt_uint 4 in
    let* key_deposit = opt_uint 5 in
    let* pool_deposit = opt_uint 6 in
    let* max_epoch = opt_uint 7 in
    let* n_opt = opt_uint 8 in
    let* pool_pledge_influence = opt_rational 9 in
    let* expansion_rate = opt_rational 10 in
    let* treasury_growth_rate = opt_rational 11 in
    let* decentralization_param = opt_rational 12 in
    let extra_entropy = find 13 in
    let* protocol_version = match find 14 with
      | Some (Cbor.Array [Cbor.Uint maj; Cbor.Uint min]) -> Ok (Some (maj, min))
      | None -> Ok None
      | _ -> Error "protocol_param_update: key 14: expected [uint, uint]"
    in
    let* min_utxo_value = opt_uint 15 in
    Ok { min_fee_a; min_fee_b; max_block_body_size; max_tx_size;
         max_block_header_size; key_deposit; pool_deposit; max_epoch;
         n_opt; pool_pledge_influence; expansion_rate; treasury_growth_rate;
         decentralization_param; extra_entropy; protocol_version; min_utxo_value }
  | _ -> Error "protocol_param_update: expected map"

let empty_protocol_param_update = {
  min_fee_a = None; min_fee_b = None; max_block_body_size = None;
  max_tx_size = None; max_block_header_size = None; key_deposit = None;
  pool_deposit = None; max_epoch = None; n_opt = None;
  pool_pledge_influence = None; expansion_rate = None;
  treasury_growth_rate = None; decentralization_param = None;
  extra_entropy = None; protocol_version = None; min_utxo_value = None;
}

(* -- Update -- *)

let encode_update upd =
  Cbor.Array [
    Cbor.Map (List.map (fun (hash, params) ->
      (Cbor.Bytes hash, encode_protocol_param_update params)
    ) upd.proposed_updates);
    Cbor.Uint upd.update_epoch;
  ]

let decode_update = function
  | Cbor.Array [Cbor.Map proposals; Cbor.Uint epoch] ->
    let* proposed_updates = list_map_result (fun (k, v) ->
      let* hash = decode_hash28 k in
      let* params = decode_protocol_param_update v in
      Ok (hash, params)
    ) proposals in
    Ok { proposed_updates; update_epoch = epoch }
  | _ -> Error "update: expected [map, uint]"

(* -- Shelley transaction body -- *)

let encode_shelley_tx_body body =
  let entries = [
    (Cbor.Uint 0L, Cbor.Array (List.map encode_tx_in body.inputs));
    (Cbor.Uint 1L, Cbor.Array (List.map encode_shelley_tx_out body.outputs));
    (Cbor.Uint 2L, Cbor.Uint body.fee);
    (Cbor.Uint 3L, Cbor.Uint body.ttl);
  ] in
  let entries = match body.certs with
    | None | Some [] -> entries
    | Some certs ->
      entries @ [(Cbor.Uint 4L, Cbor.Array (List.map encode_certificate certs))]
  in
  let entries = match body.withdrawals with
    | None | Some [] -> entries
    | Some wds -> entries @ [(Cbor.Uint 5L, encode_withdrawals wds)]
  in
  let entries = match body.update with
    | None -> entries
    | Some upd -> entries @ [(Cbor.Uint 6L, encode_update upd)]
  in
  let entries = match body.auxiliary_data_hash with
    | None -> entries
    | Some hash -> entries @ [(Cbor.Uint 7L, Cbor.Bytes hash)]
  in
  Cbor.Map entries

let decode_shelley_tx_body = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* inputs = match find 0 with
      | Some (Cbor.Array items) -> list_map_result decode_tx_in items
      | _ -> Error "tx_body: missing or invalid inputs (key 0)"
    in
    let* outputs = match find 1 with
      | Some (Cbor.Array items) -> list_map_result decode_shelley_tx_out items
      | _ -> Error "tx_body: missing or invalid outputs (key 1)"
    in
    let* fee = match find 2 with
      | Some (Cbor.Uint n) -> Ok n
      | _ -> Error "tx_body: missing or invalid fee (key 2)"
    in
    let* ttl = match find 3 with
      | Some (Cbor.Uint n) -> Ok n
      | _ -> Error "tx_body: missing or invalid ttl (key 3)"
    in
    let* certs = match find 4 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_certificate items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "tx_body: invalid certificates (key 4)"
    in
    let* withdrawals = match find 5 with
      | Some v -> let* wds = decode_withdrawals v in Ok (Some wds)
      | None -> Ok None
    in
    let* update = match find 6 with
      | Some v -> let* u = decode_update v in Ok (Some u)
      | None -> Ok None
    in
    let* auxiliary_data_hash = match find 7 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "tx_body: invalid auxiliary_data_hash (key 7)"
    in
    Ok { inputs; outputs; fee; ttl; certs; withdrawals; update; auxiliary_data_hash }
  | _ -> Error "tx_body: expected map"

(* -- VKey witness -- *)

let encode_vkey_witness w =
  Cbor.Array [Cbor.Bytes w.witness_vkey; Cbor.Bytes w.witness_sig]

let decode_vkey_witness = function
  | Cbor.Array [Cbor.Bytes vk; Cbor.Bytes sig_] ->
    Ok { witness_vkey = vk; witness_sig = sig_ }
  | _ -> Error "vkey_witness: expected [bytes, bytes]"

(* -- Multisig script -- *)

let rec encode_multisig_script = function
  | Sig hash ->
    Cbor.Array [Cbor.Uint 0L; Cbor.Bytes hash]
  | All_of scripts ->
    Cbor.Array [Cbor.Uint 1L; Cbor.Array (List.map encode_multisig_script scripts)]
  | Any_of scripts ->
    Cbor.Array [Cbor.Uint 2L; Cbor.Array (List.map encode_multisig_script scripts)]
  | N_of (n, scripts) ->
    Cbor.Array [Cbor.Uint 3L; Cbor.Uint (Int64.of_int n);
                Cbor.Array (List.map encode_multisig_script scripts)]

let rec decode_multisig_script = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Sig h)
  | Cbor.Array [Cbor.Uint 1L; Cbor.Array scripts] ->
    let* ss = list_map_result decode_multisig_script scripts in
    Ok (All_of ss)
  | Cbor.Array [Cbor.Uint 2L; Cbor.Array scripts] ->
    let* ss = list_map_result decode_multisig_script scripts in
    Ok (Any_of ss)
  | Cbor.Array [Cbor.Uint 3L; Cbor.Uint n; Cbor.Array scripts] ->
    let* ss = list_map_result decode_multisig_script scripts in
    Ok (N_of (Int64.to_int n, ss))
  | _ -> Error "multisig_script: unrecognized structure"

(* -- Shelley transaction witness set -- *)

let encode_shelley_tx_witness_set ws =
  let entries = [] in
  let entries = match ws.vkey_witnesses with
    | None | Some [] -> entries
    | Some vkws ->
      entries @ [(Cbor.Uint 0L, Cbor.Array (List.map encode_vkey_witness vkws))]
  in
  let entries = match ws.multisig_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 1L, Cbor.Array (List.map encode_multisig_script scripts))]
  in
  let entries = match ws.bootstrap_witnesses with
    | None -> entries
    | Some bw -> entries @ [(Cbor.Uint 2L, bw)]
  in
  Cbor.Map entries

let decode_shelley_tx_witness_set = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* vkey_witnesses = match find 0 with
      | Some (Cbor.Array items) ->
        let* ws = list_map_result decode_vkey_witness items in Ok (Some ws)
      | None -> Ok None
      | _ -> Error "tx_witness_set: invalid vkey_witnesses (key 0)"
    in
    let* multisig_scripts = match find 1 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_multisig_script items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "tx_witness_set: invalid multisig_scripts (key 1)"
    in
    let bootstrap_witnesses = find 2 in
    Ok { vkey_witnesses; multisig_scripts; bootstrap_witnesses }
  | _ -> Error "tx_witness_set: expected map"

(* -- VRF cert -- *)

let encode_vrf_cert cert =
  Cbor.Array [Cbor.Bytes cert.vrf_output; Cbor.Bytes cert.vrf_proof]

let decode_vrf_cert = function
  | Cbor.Array [Cbor.Bytes output; Cbor.Bytes proof] ->
    Ok { vrf_output = output; vrf_proof = proof }
  | _ -> Error "vrf_cert: expected [bytes, bytes]"

(* -- Operational cert -- *)

let encode_operational_cert oc =
  [ Cbor.Bytes oc.hot_vkey;
    Cbor.Uint oc.sequence_number;
    Cbor.Uint oc.kes_period;
    Cbor.Bytes oc.sigma ]

let decode_operational_cert_flat = function
  | [Cbor.Bytes hot_vkey; Cbor.Uint seq; Cbor.Uint period; Cbor.Bytes sigma] ->
    Ok { hot_vkey; sequence_number = seq; kes_period = period; sigma }
  | _ -> Error "operational_cert: invalid flat structure"

(* -- Shelley header body -- *)

let encode_shelley_header_body hb =
  Cbor.Array ([
    Cbor.Uint hb.shb_block_number;
    Cbor.Uint hb.shb_slot;
    (match hb.shb_prev_hash with None -> Cbor.Null | Some h -> Cbor.Bytes h);
    Cbor.Bytes hb.shb_issuer_vkey;
    Cbor.Bytes hb.shb_vrf_vkey;
    encode_vrf_cert hb.shb_nonce_vrf;
    encode_vrf_cert hb.shb_leader_vrf;
    Cbor.Uint hb.shb_body_size;
    Cbor.Bytes hb.shb_body_hash;
  ] @ encode_operational_cert hb.shb_operational_cert @ [
    Cbor.Uint hb.shb_protocol_version.proto_major;
    Cbor.Uint hb.shb_protocol_version.proto_minor;
  ])

let decode_shelley_header_body = function
  | Cbor.Array [Cbor.Uint block_num; Cbor.Uint slot; prev_hash_cbor;
                Cbor.Bytes issuer; Cbor.Bytes vrf_vk;
                nonce_vrf_cbor; leader_vrf_cbor;
                Cbor.Uint body_size; Cbor.Bytes body_hash;
                Cbor.Bytes hot_vk; Cbor.Uint seq; Cbor.Uint period;
                Cbor.Bytes sigma;
                Cbor.Uint proto_maj; Cbor.Uint proto_min] ->
    let* prev_hash = match prev_hash_cbor with
      | Cbor.Null -> Ok None
      | Cbor.Bytes h when Bytes.length h = 32 -> Ok (Some h)
      | _ -> Error "header_body: invalid prev_hash"
    in
    let* nonce_vrf = decode_vrf_cert nonce_vrf_cbor in
    let* leader_vrf = decode_vrf_cert leader_vrf_cbor in
    Ok { shb_block_number = block_num;
         shb_slot = slot;
         shb_prev_hash = prev_hash;
         shb_issuer_vkey = issuer;
         shb_vrf_vkey = vrf_vk;
         shb_nonce_vrf = nonce_vrf;
         shb_leader_vrf = leader_vrf;
         shb_body_size = body_size;
         shb_body_hash = body_hash;
         shb_operational_cert = { hot_vkey = hot_vk; sequence_number = seq;
                                  kes_period = period; sigma };
         shb_protocol_version = { proto_major = proto_maj; proto_minor = proto_min } }
  | _ -> Error "header_body: expected 15-element array"

(* -- Shelley header -- *)

let encode_shelley_header hdr =
  Cbor.Array [
    encode_shelley_header_body hdr.sh_header_body;
    Cbor.Bytes hdr.sh_body_signature;
  ]

let decode_shelley_header = function
  | Cbor.Array [hb_cbor; Cbor.Bytes sig_] ->
    let* hb = decode_shelley_header_body hb_cbor in
    Ok { sh_header_body = hb; sh_body_signature = sig_ }
  | _ -> Error "header: expected [header_body, signature]"

(* -- Shelley transaction -- *)

let encode_shelley_tx tx =
  Cbor.Array [
    encode_shelley_tx_body tx.shelley_tx_body;
    encode_shelley_tx_witness_set tx.shelley_tx_witness_set;
    (match tx.shelley_tx_metadata with None -> Cbor.Null | Some m -> m);
  ]

let decode_shelley_tx = function
  | Cbor.Array [body_cbor; ws_cbor; meta_cbor] ->
    let* body = decode_shelley_tx_body body_cbor in
    let* ws = decode_shelley_tx_witness_set ws_cbor in
    let meta = match meta_cbor with Cbor.Null -> None | v -> Some v in
    Ok { shelley_tx_body = body;
         shelley_tx_witness_set = ws;
         shelley_tx_metadata = meta }
  | _ -> Error "shelley_tx: expected [body, witnesses, metadata/null]"

(* -- Shelley block -- *)

let encode_shelley_block block =
  Cbor.Array [
    encode_shelley_header block.sb_header;
    Cbor.Array (List.map encode_shelley_tx_body block.sb_tx_bodies);
    Cbor.Array (List.map encode_shelley_tx_witness_set block.sb_tx_witness_sets);
    Cbor.Map (List.map (fun (idx, meta) ->
      (Cbor.Uint (Int64.of_int idx), meta)
    ) block.sb_tx_metadata);
  ]

let decode_shelley_block = function
  | Cbor.Array [hdr_cbor; Cbor.Array bodies_cbor;
                Cbor.Array witnesses_cbor; Cbor.Map meta_cbor] ->
    let* header = decode_shelley_header hdr_cbor in
    let* bodies = list_map_result decode_shelley_tx_body bodies_cbor in
    let* witnesses = list_map_result decode_shelley_tx_witness_set witnesses_cbor in
    let* metadata = list_map_result (fun (k, v) ->
      let* idx = decode_uint k in
      Ok (Int64.to_int idx, v)
    ) meta_cbor in
    Ok { sb_header = header;
         sb_tx_bodies = bodies;
         sb_tx_witness_sets = witnesses;
         sb_tx_metadata = metadata }
  | _ -> Error "shelley_block: expected [header, [bodies], [witnesses], {metadata}]"

(* ================================================================ *)
(* Allegra CBOR encoding/decoding                                    *)
(* ================================================================ *)

(* -- Timelock -- *)

let rec encode_timelock = function
  | TL_Signature hash ->
    Cbor.Array [Cbor.Uint 0L; Cbor.Bytes hash]
  | TL_AllOf scripts ->
    Cbor.Array [Cbor.Uint 1L; Cbor.Array (List.map encode_timelock scripts)]
  | TL_AnyOf scripts ->
    Cbor.Array [Cbor.Uint 2L; Cbor.Array (List.map encode_timelock scripts)]
  | TL_MOfN (n, scripts) ->
    Cbor.Array [Cbor.Uint 3L; Cbor.Uint (Int64.of_int n);
                Cbor.Array (List.map encode_timelock scripts)]
  | TL_InvalidBefore slot ->
    Cbor.Array [Cbor.Uint 4L; Cbor.Uint slot]
  | TL_InvalidHereafter slot ->
    Cbor.Array [Cbor.Uint 5L; Cbor.Uint slot]

let rec decode_timelock = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (TL_Signature h)
  | Cbor.Array [Cbor.Uint 1L; Cbor.Array scripts] ->
    let* ss = list_map_result decode_timelock scripts in
    Ok (TL_AllOf ss)
  | Cbor.Array [Cbor.Uint 2L; Cbor.Array scripts] ->
    let* ss = list_map_result decode_timelock scripts in
    Ok (TL_AnyOf ss)
  | Cbor.Array [Cbor.Uint 3L; Cbor.Uint n; Cbor.Array scripts] ->
    let* ss = list_map_result decode_timelock scripts in
    Ok (TL_MOfN (Int64.to_int n, ss))
  | Cbor.Array [Cbor.Uint 4L; Cbor.Uint slot] ->
    Ok (TL_InvalidBefore slot)
  | Cbor.Array [Cbor.Uint 5L; Cbor.Uint slot] ->
    Ok (TL_InvalidHereafter slot)
  | _ -> Error "timelock: unrecognized structure"

(* -- Allegra tx body -- *)

let encode_allegra_tx_body body =
  let entries = [
    (Cbor.Uint 0L, Cbor.Array (List.map encode_tx_in body.al_inputs));
    (Cbor.Uint 1L, Cbor.Array (List.map encode_shelley_tx_out body.al_outputs));
    (Cbor.Uint 2L, Cbor.Uint body.al_fee);
  ] in
  let entries = match body.al_ttl with
    | None -> entries
    | Some ttl -> entries @ [(Cbor.Uint 3L, Cbor.Uint ttl)]
  in
  let entries = match body.al_certs with
    | None | Some [] -> entries
    | Some certs ->
      entries @ [(Cbor.Uint 4L, Cbor.Array (List.map encode_certificate certs))]
  in
  let entries = match body.al_withdrawals with
    | None | Some [] -> entries
    | Some wds -> entries @ [(Cbor.Uint 5L, encode_withdrawals wds)]
  in
  let entries = match body.al_update with
    | None -> entries
    | Some upd -> entries @ [(Cbor.Uint 6L, encode_update upd)]
  in
  let entries = match body.al_auxiliary_data_hash with
    | None -> entries
    | Some hash -> entries @ [(Cbor.Uint 7L, Cbor.Bytes hash)]
  in
  let entries = match body.al_validity_interval_start with
    | None -> entries
    | Some slot -> entries @ [(Cbor.Uint 8L, Cbor.Uint slot)]
  in
  Cbor.Map entries

let decode_allegra_tx_body = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* al_inputs = match find 0 with
      | Some (Cbor.Array items) -> list_map_result decode_tx_in items
      | _ -> Error "allegra_tx_body: missing or invalid inputs (key 0)"
    in
    let* al_outputs = match find 1 with
      | Some (Cbor.Array items) -> list_map_result decode_shelley_tx_out items
      | _ -> Error "allegra_tx_body: missing or invalid outputs (key 1)"
    in
    let* al_fee = match find 2 with
      | Some (Cbor.Uint n) -> Ok n
      | _ -> Error "allegra_tx_body: missing or invalid fee (key 2)"
    in
    let al_ttl = match find 3 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* al_certs = match find 4 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_certificate items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "allegra_tx_body: invalid certificates (key 4)"
    in
    let* al_withdrawals = match find 5 with
      | Some v -> let* wds = decode_withdrawals v in Ok (Some wds)
      | None -> Ok None
    in
    let* al_update = match find 6 with
      | Some v -> let* u = decode_update v in Ok (Some u)
      | None -> Ok None
    in
    let* al_auxiliary_data_hash = match find 7 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "allegra_tx_body: invalid auxiliary_data_hash (key 7)"
    in
    let al_validity_interval_start = match find 8 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    Ok { al_inputs; al_outputs; al_fee; al_ttl; al_certs; al_withdrawals;
         al_update; al_auxiliary_data_hash; al_validity_interval_start }
  | _ -> Error "allegra_tx_body: expected map"

(* -- Allegra tx witness set -- *)

let encode_allegra_tx_witness_set ws =
  let entries = [] in
  let entries = match ws.al_vkey_witnesses with
    | None | Some [] -> entries
    | Some vkws ->
      entries @ [(Cbor.Uint 0L, Cbor.Array (List.map encode_vkey_witness vkws))]
  in
  let entries = match ws.al_native_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 1L, Cbor.Array (List.map encode_timelock scripts))]
  in
  let entries = match ws.al_bootstrap_witnesses with
    | None -> entries
    | Some bw -> entries @ [(Cbor.Uint 2L, bw)]
  in
  Cbor.Map entries

let decode_allegra_tx_witness_set = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* al_vkey_witnesses = match find 0 with
      | Some (Cbor.Array items) ->
        let* ws = list_map_result decode_vkey_witness items in Ok (Some ws)
      | None -> Ok None
      | _ -> Error "allegra_tx_witness_set: invalid vkey_witnesses (key 0)"
    in
    let* al_native_scripts = match find 1 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_timelock items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "allegra_tx_witness_set: invalid native_scripts (key 1)"
    in
    let al_bootstrap_witnesses = find 2 in
    Ok { al_vkey_witnesses; al_native_scripts; al_bootstrap_witnesses }
  | _ -> Error "allegra_tx_witness_set: expected map"

(* -- Allegra transaction -- *)

let encode_allegra_tx tx =
  Cbor.Array [
    encode_allegra_tx_body tx.allegra_tx_body;
    encode_allegra_tx_witness_set tx.allegra_tx_witness_set;
    (match tx.allegra_tx_metadata with None -> Cbor.Null | Some m -> m);
  ]

let decode_allegra_tx = function
  | Cbor.Array [body_cbor; ws_cbor; meta_cbor] ->
    let* body = decode_allegra_tx_body body_cbor in
    let* ws = decode_allegra_tx_witness_set ws_cbor in
    let meta = match meta_cbor with Cbor.Null -> None | v -> Some v in
    Ok { allegra_tx_body = body;
         allegra_tx_witness_set = ws;
         allegra_tx_metadata = meta }
  | _ -> Error "allegra_tx: expected [body, witnesses, metadata/null]"

(* -- Allegra block -- *)

let encode_allegra_block block =
  Cbor.Array [
    encode_shelley_header block.alb_header;
    Cbor.Array (List.map encode_allegra_tx_body block.alb_tx_bodies);
    Cbor.Array (List.map encode_allegra_tx_witness_set block.alb_tx_witness_sets);
    Cbor.Map (List.map (fun (idx, meta) ->
      (Cbor.Uint (Int64.of_int idx), meta)
    ) block.alb_tx_metadata);
  ]

let decode_allegra_block = function
  | Cbor.Array [hdr_cbor; Cbor.Array bodies_cbor;
                Cbor.Array witnesses_cbor; Cbor.Map meta_cbor] ->
    let* header = decode_shelley_header hdr_cbor in
    let* bodies = list_map_result decode_allegra_tx_body bodies_cbor in
    let* witnesses = list_map_result decode_allegra_tx_witness_set witnesses_cbor in
    let* metadata = list_map_result (fun (k, v) ->
      let* idx = decode_uint k in
      Ok (Int64.to_int idx, v)
    ) meta_cbor in
    Ok { alb_header = header;
         alb_tx_bodies = bodies;
         alb_tx_witness_sets = witnesses;
         alb_tx_metadata = metadata }
  | _ -> Error "allegra_block: expected [header, [bodies], [witnesses], {metadata}]"

(* ================================================================ *)
(* Mary CBOR encoding/decoding                                       *)
(* ================================================================ *)

(* -- Multi-asset -- *)

let encode_multi_asset ma =
  Cbor.Map (List.map (fun (policy_id, assets) ->
    (Cbor.Bytes policy_id,
     Cbor.Map (List.map (fun (name, qty) ->
       (Cbor.Bytes name,
        if Int64.compare qty 0L >= 0 then Cbor.Uint qty
        else Cbor.Nint qty)
     ) assets))
  ) ma)

let decode_multi_asset = function
  | Cbor.Map pairs ->
    list_map_result (fun (k, v) ->
      let* policy_id = decode_hash28 k in
      match v with
      | Cbor.Map assets ->
        let* assets = list_map_result (fun (ak, av) ->
          let* name = decode_bytes ak in
          let* qty = match av with
            | Cbor.Uint n -> Ok n
            | Cbor.Nint n -> Ok n
            | _ -> Error "multi_asset: expected uint or nint for quantity"
          in
          Ok (name, qty)
        ) assets in
        Ok (policy_id, assets)
      | _ -> Error "multi_asset: expected inner map"
    ) pairs
  | _ -> Error "multi_asset: expected map"

(* -- Value -- *)

let encode_value = function
  | Lovelace c -> Cbor.Uint c
  | Lovelace_and_assets (c, ma) ->
    Cbor.Array [Cbor.Uint c; encode_multi_asset ma]

let decode_value = function
  | Cbor.Uint c -> Ok (Lovelace c)
  | Cbor.Array [Cbor.Uint c; ma_cbor] ->
    let* ma = decode_multi_asset ma_cbor in
    Ok (Lovelace_and_assets (c, ma))
  | _ -> Error "value: expected uint or [uint, multi_asset]"

(* -- Mary tx out -- *)

let encode_mary_tx_out out =
  Cbor.Array [Cbor.Bytes out.ma_address; encode_value out.ma_value]

let decode_mary_tx_out = function
  | Cbor.Array [Cbor.Bytes addr; value_cbor] ->
    let* v = decode_value value_cbor in
    Ok { ma_address = addr; ma_value = v }
  | _ -> Error "mary_tx_out: expected [bytes, value]"

(* -- Mary tx body -- *)

let encode_mary_tx_body body =
  let entries = [
    (Cbor.Uint 0L, Cbor.Array (List.map encode_tx_in body.ma_inputs));
    (Cbor.Uint 1L, Cbor.Array (List.map encode_mary_tx_out body.ma_outputs));
    (Cbor.Uint 2L, Cbor.Uint body.ma_fee);
  ] in
  let entries = match body.ma_ttl with
    | None -> entries
    | Some ttl -> entries @ [(Cbor.Uint 3L, Cbor.Uint ttl)]
  in
  let entries = match body.ma_certs with
    | None | Some [] -> entries
    | Some certs ->
      entries @ [(Cbor.Uint 4L, Cbor.Array (List.map encode_certificate certs))]
  in
  let entries = match body.ma_withdrawals with
    | None | Some [] -> entries
    | Some wds -> entries @ [(Cbor.Uint 5L, encode_withdrawals wds)]
  in
  let entries = match body.ma_update with
    | None -> entries
    | Some upd -> entries @ [(Cbor.Uint 6L, encode_update upd)]
  in
  let entries = match body.ma_auxiliary_data_hash with
    | None -> entries
    | Some hash -> entries @ [(Cbor.Uint 7L, Cbor.Bytes hash)]
  in
  let entries = match body.ma_validity_interval_start with
    | None -> entries
    | Some slot -> entries @ [(Cbor.Uint 8L, Cbor.Uint slot)]
  in
  let entries = match body.ma_mint with
    | None -> entries
    | Some ma -> entries @ [(Cbor.Uint 9L, encode_multi_asset ma)]
  in
  Cbor.Map entries

let decode_mary_tx_body = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* ma_inputs = match find 0 with
      | Some (Cbor.Array items) -> list_map_result decode_tx_in items
      | _ -> Error "mary_tx_body: missing or invalid inputs (key 0)"
    in
    let* ma_outputs = match find 1 with
      | Some (Cbor.Array items) -> list_map_result decode_mary_tx_out items
      | _ -> Error "mary_tx_body: missing or invalid outputs (key 1)"
    in
    let* ma_fee = match find 2 with
      | Some (Cbor.Uint n) -> Ok n
      | _ -> Error "mary_tx_body: missing or invalid fee (key 2)"
    in
    let ma_ttl = match find 3 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* ma_certs = match find 4 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_certificate items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "mary_tx_body: invalid certificates (key 4)"
    in
    let* ma_withdrawals = match find 5 with
      | Some v -> let* wds = decode_withdrawals v in Ok (Some wds)
      | None -> Ok None
    in
    let* ma_update = match find 6 with
      | Some v -> let* u = decode_update v in Ok (Some u)
      | None -> Ok None
    in
    let* ma_auxiliary_data_hash = match find 7 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "mary_tx_body: invalid auxiliary_data_hash (key 7)"
    in
    let ma_validity_interval_start = match find 8 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* ma_mint = match find 9 with
      | Some v -> let* m = decode_multi_asset v in Ok (Some m)
      | None -> Ok None
    in
    Ok { ma_inputs; ma_outputs; ma_fee; ma_ttl; ma_certs; ma_withdrawals;
         ma_update; ma_auxiliary_data_hash; ma_validity_interval_start; ma_mint }
  | _ -> Error "mary_tx_body: expected map"

(* Mary witness set reuses Allegra *)
let encode_mary_tx_witness_set = encode_allegra_tx_witness_set
let decode_mary_tx_witness_set = decode_allegra_tx_witness_set

(* -- Mary transaction -- *)

let encode_mary_tx tx =
  Cbor.Array [
    encode_mary_tx_body tx.mary_tx_body;
    encode_mary_tx_witness_set tx.mary_tx_witness_set;
    (match tx.mary_tx_metadata with None -> Cbor.Null | Some m -> m);
  ]

let decode_mary_tx = function
  | Cbor.Array [body_cbor; ws_cbor; meta_cbor] ->
    let* body = decode_mary_tx_body body_cbor in
    let* ws = decode_mary_tx_witness_set ws_cbor in
    let meta = match meta_cbor with Cbor.Null -> None | v -> Some v in
    Ok { mary_tx_body = body;
         mary_tx_witness_set = ws;
         mary_tx_metadata = meta }
  | _ -> Error "mary_tx: expected [body, witnesses, metadata/null]"

(* -- Mary block -- *)

let encode_mary_block block =
  Cbor.Array [
    encode_shelley_header block.mab_header;
    Cbor.Array (List.map encode_mary_tx_body block.mab_tx_bodies);
    Cbor.Array (List.map encode_mary_tx_witness_set block.mab_tx_witness_sets);
    Cbor.Map (List.map (fun (idx, meta) ->
      (Cbor.Uint (Int64.of_int idx), meta)
    ) block.mab_tx_metadata);
  ]

let decode_mary_block = function
  | Cbor.Array [hdr_cbor; Cbor.Array bodies_cbor;
                Cbor.Array witnesses_cbor; Cbor.Map meta_cbor] ->
    let* header = decode_shelley_header hdr_cbor in
    let* bodies = list_map_result decode_mary_tx_body bodies_cbor in
    let* witnesses = list_map_result decode_mary_tx_witness_set witnesses_cbor in
    let* metadata = list_map_result (fun (k, v) ->
      let* idx = decode_uint k in
      Ok (Int64.to_int idx, v)
    ) meta_cbor in
    Ok { mab_header = header;
         mab_tx_bodies = bodies;
         mab_tx_witness_sets = witnesses;
         mab_tx_metadata = metadata }
  | _ -> Error "mary_block: expected [header, [bodies], [witnesses], {metadata}]"

(* ================================================================ *)
(* Alonzo CBOR encoding/decoding                                     *)
(* ================================================================ *)

(* -- Ex units -- *)

let encode_ex_units eu =
  Cbor.Array [Cbor.Uint eu.mem; Cbor.Uint eu.steps]

let decode_ex_units = function
  | Cbor.Array [Cbor.Uint mem; Cbor.Uint steps] ->
    Ok { mem; steps }
  | _ -> Error "ex_units: expected [uint, uint]"

(* -- Redeemer tag -- *)

let encode_redeemer_tag = function
  | Spend -> Cbor.Uint 0L
  | Mint -> Cbor.Uint 1L
  | Cert -> Cbor.Uint 2L
  | Reward -> Cbor.Uint 3L

let decode_redeemer_tag = function
  | Cbor.Uint 0L -> Ok Spend
  | Cbor.Uint 1L -> Ok Mint
  | Cbor.Uint 2L -> Ok Cert
  | Cbor.Uint 3L -> Ok Reward
  | _ -> Error "redeemer_tag: expected 0-3"

(* -- Redeemer -- *)

let encode_redeemer r =
  Cbor.Array [encode_redeemer_tag r.rd_tag; Cbor.Uint r.rd_index;
              r.rd_data; encode_ex_units r.rd_ex_units]

let decode_redeemer = function
  | Cbor.Array [tag_cbor; Cbor.Uint index; data; eu_cbor] ->
    let* rd_tag = decode_redeemer_tag tag_cbor in
    let* rd_ex_units = decode_ex_units eu_cbor in
    Ok { rd_tag; rd_index = index; rd_data = data; rd_ex_units }
  | _ -> Error "redeemer: expected [tag, index, data, ex_units]"

(* -- Alonzo tx out -- *)

let encode_alonzo_tx_out out =
  match out.az_datum_hash with
  | None ->
    Cbor.Array [Cbor.Bytes out.az_address; encode_value out.az_value]
  | Some dh ->
    Cbor.Array [Cbor.Bytes out.az_address; encode_value out.az_value; Cbor.Bytes dh]

let decode_alonzo_tx_out = function
  | Cbor.Array [Cbor.Bytes addr; value_cbor] ->
    let* v = decode_value value_cbor in
    Ok { az_address = addr; az_value = v; az_datum_hash = None }
  | Cbor.Array [Cbor.Bytes addr; value_cbor; Cbor.Bytes dh]
    when Bytes.length dh = 32 ->
    let* v = decode_value value_cbor in
    Ok { az_address = addr; az_value = v; az_datum_hash = Some dh }
  | _ -> Error "alonzo_tx_out: expected [bytes, value, ?hash32]"

(* -- Alonzo tx body -- *)

let encode_alonzo_tx_body body =
  let entries = [
    (Cbor.Uint 0L, Cbor.Array (List.map encode_tx_in body.az_inputs));
    (Cbor.Uint 1L, Cbor.Array (List.map encode_alonzo_tx_out body.az_outputs));
    (Cbor.Uint 2L, Cbor.Uint body.az_fee);
  ] in
  let entries = match body.az_ttl with
    | None -> entries
    | Some ttl -> entries @ [(Cbor.Uint 3L, Cbor.Uint ttl)]
  in
  let entries = match body.az_certs with
    | None | Some [] -> entries
    | Some certs ->
      entries @ [(Cbor.Uint 4L, Cbor.Array (List.map encode_certificate certs))]
  in
  let entries = match body.az_withdrawals with
    | None | Some [] -> entries
    | Some wds -> entries @ [(Cbor.Uint 5L, encode_withdrawals wds)]
  in
  let entries = match body.az_update with
    | None -> entries
    | Some upd -> entries @ [(Cbor.Uint 6L, encode_update upd)]
  in
  let entries = match body.az_auxiliary_data_hash with
    | None -> entries
    | Some hash -> entries @ [(Cbor.Uint 7L, Cbor.Bytes hash)]
  in
  let entries = match body.az_validity_interval_start with
    | None -> entries
    | Some slot -> entries @ [(Cbor.Uint 8L, Cbor.Uint slot)]
  in
  let entries = match body.az_mint with
    | None -> entries
    | Some ma -> entries @ [(Cbor.Uint 9L, encode_multi_asset ma)]
  in
  let entries = match body.az_script_data_hash with
    | None -> entries
    | Some h -> entries @ [(Cbor.Uint 11L, Cbor.Bytes h)]
  in
  let entries = match body.az_collateral with
    | None | Some [] -> entries
    | Some cols ->
      entries @ [(Cbor.Uint 13L, Cbor.Array (List.map encode_tx_in cols))]
  in
  let entries = match body.az_required_signers with
    | None | Some [] -> entries
    | Some sigs ->
      entries @ [(Cbor.Uint 14L, Cbor.Array (List.map (fun h -> Cbor.Bytes h) sigs))]
  in
  let entries = match body.az_network_id with
    | None -> entries
    | Some nid -> entries @ [(Cbor.Uint 15L, Cbor.Uint nid)]
  in
  Cbor.Map entries

let decode_alonzo_tx_body = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* az_inputs = match find 0 with
      | Some (Cbor.Array items) -> list_map_result decode_tx_in items
      | _ -> Error "alonzo_tx_body: missing or invalid inputs (key 0)"
    in
    let* az_outputs = match find 1 with
      | Some (Cbor.Array items) -> list_map_result decode_alonzo_tx_out items
      | _ -> Error "alonzo_tx_body: missing or invalid outputs (key 1)"
    in
    let* az_fee = match find 2 with
      | Some (Cbor.Uint n) -> Ok n
      | _ -> Error "alonzo_tx_body: missing or invalid fee (key 2)"
    in
    let az_ttl = match find 3 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* az_certs = match find 4 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_certificate items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "alonzo_tx_body: invalid certificates (key 4)"
    in
    let* az_withdrawals = match find 5 with
      | Some v -> let* wds = decode_withdrawals v in Ok (Some wds)
      | None -> Ok None
    in
    let* az_update = match find 6 with
      | Some v -> let* u = decode_update v in Ok (Some u)
      | None -> Ok None
    in
    let* az_auxiliary_data_hash = match find 7 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "alonzo_tx_body: invalid auxiliary_data_hash (key 7)"
    in
    let az_validity_interval_start = match find 8 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* az_mint = match find 9 with
      | Some v -> let* m = decode_multi_asset v in Ok (Some m)
      | None -> Ok None
    in
    let* az_script_data_hash = match find 11 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "alonzo_tx_body: invalid script_data_hash (key 11)"
    in
    let* az_collateral = match find 13 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_tx_in items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "alonzo_tx_body: invalid collateral (key 13)"
    in
    let* az_required_signers = match find 14 with
      | Some (Cbor.Array items) ->
        let* sigs = list_map_result (fun v ->
          match v with
          | Cbor.Bytes h when Bytes.length h = 28 -> Ok h
          | _ -> Error "alonzo_tx_body: invalid required_signer hash"
        ) items in Ok (Some sigs)
      | None -> Ok None
      | _ -> Error "alonzo_tx_body: invalid required_signers (key 14)"
    in
    let az_network_id = match find 15 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    Ok { az_inputs; az_outputs; az_fee; az_ttl; az_certs; az_withdrawals;
         az_update; az_auxiliary_data_hash; az_validity_interval_start;
         az_mint; az_script_data_hash; az_collateral; az_required_signers;
         az_network_id }
  | _ -> Error "alonzo_tx_body: expected map"

(* -- Alonzo tx witness set -- *)

let encode_alonzo_tx_witness_set ws =
  let entries = [] in
  let entries = match ws.az_vkey_witnesses with
    | None | Some [] -> entries
    | Some vkws ->
      entries @ [(Cbor.Uint 0L, Cbor.Array (List.map encode_vkey_witness vkws))]
  in
  let entries = match ws.az_native_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 1L, Cbor.Array (List.map encode_timelock scripts))]
  in
  let entries = match ws.az_bootstrap_witnesses with
    | None -> entries
    | Some bw -> entries @ [(Cbor.Uint 2L, bw)]
  in
  let entries = match ws.az_plutus_v1_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 3L, Cbor.Array (List.map (fun b -> Cbor.Bytes b) scripts))]
  in
  let entries = match ws.az_plutus_data with
    | None | Some [] -> entries
    | Some data ->
      entries @ [(Cbor.Uint 4L, Cbor.Array data)]
  in
  let entries = match ws.az_redeemers with
    | None | Some [] -> entries
    | Some reds ->
      entries @ [(Cbor.Uint 5L, Cbor.Array (List.map encode_redeemer reds))]
  in
  Cbor.Map entries

let decode_alonzo_tx_witness_set = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* az_vkey_witnesses = match find 0 with
      | Some (Cbor.Array items) ->
        let* ws = list_map_result decode_vkey_witness items in Ok (Some ws)
      | None -> Ok None
      | _ -> Error "alonzo_tx_witness_set: invalid vkey_witnesses (key 0)"
    in
    let* az_native_scripts = match find 1 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_timelock items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "alonzo_tx_witness_set: invalid native_scripts (key 1)"
    in
    let az_bootstrap_witnesses = find 2 in
    let* az_plutus_v1_scripts = match find 3 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_bytes items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "alonzo_tx_witness_set: invalid plutus_v1_scripts (key 3)"
    in
    let az_plutus_data = match find 4 with
      | Some (Cbor.Array items) -> Some items
      | _ -> None
    in
    let* az_redeemers = match find 5 with
      | Some (Cbor.Array items) ->
        let* rs = list_map_result decode_redeemer items in Ok (Some rs)
      | None -> Ok None
      | _ -> Error "alonzo_tx_witness_set: invalid redeemers (key 5)"
    in
    Ok { az_vkey_witnesses; az_native_scripts; az_bootstrap_witnesses;
         az_plutus_v1_scripts; az_plutus_data; az_redeemers }
  | _ -> Error "alonzo_tx_witness_set: expected map"

(* -- Alonzo transaction -- *)

let encode_alonzo_tx tx =
  Cbor.Array [
    encode_alonzo_tx_body tx.alonzo_tx_body;
    encode_alonzo_tx_witness_set tx.alonzo_tx_witness_set;
    Cbor.Bool tx.alonzo_tx_is_valid;
    (match tx.alonzo_tx_auxiliary_data with None -> Cbor.Null | Some m -> m);
  ]

let decode_alonzo_tx = function
  | Cbor.Array [body_cbor; ws_cbor; Cbor.Bool is_valid; aux_cbor] ->
    let* body = decode_alonzo_tx_body body_cbor in
    let* ws = decode_alonzo_tx_witness_set ws_cbor in
    let aux = match aux_cbor with Cbor.Null -> None | v -> Some v in
    Ok { alonzo_tx_body = body;
         alonzo_tx_witness_set = ws;
         alonzo_tx_is_valid = is_valid;
         alonzo_tx_auxiliary_data = aux }
  | _ -> Error "alonzo_tx: expected [body, witnesses, bool, aux/null]"

(* -- Alonzo block -- *)

let encode_alonzo_block block =
  Cbor.Array [
    encode_shelley_header block.azb_header;
    Cbor.Array (List.map encode_alonzo_tx_body block.azb_tx_bodies);
    Cbor.Array (List.map encode_alonzo_tx_witness_set block.azb_tx_witness_sets);
    Cbor.Map (List.map (fun (idx, meta) ->
      (Cbor.Uint (Int64.of_int idx), meta)
    ) block.azb_tx_metadata);
    Cbor.Array (List.map (fun i -> Cbor.Uint (Int64.of_int i)) block.azb_invalid_txs);
  ]

let decode_alonzo_block = function
  | Cbor.Array [hdr_cbor; Cbor.Array bodies_cbor;
                Cbor.Array witnesses_cbor; Cbor.Map meta_cbor;
                Cbor.Array invalid_cbor] ->
    let* header = decode_shelley_header hdr_cbor in
    let* bodies = list_map_result decode_alonzo_tx_body bodies_cbor in
    let* witnesses = list_map_result decode_alonzo_tx_witness_set witnesses_cbor in
    let* metadata = list_map_result (fun (k, v) ->
      let* idx = decode_uint k in
      Ok (Int64.to_int idx, v)
    ) meta_cbor in
    let* invalid_txs = list_map_result (fun v ->
      let* n = decode_uint v in Ok (Int64.to_int n)
    ) invalid_cbor in
    Ok { azb_header = header;
         azb_tx_bodies = bodies;
         azb_tx_witness_sets = witnesses;
         azb_tx_metadata = metadata;
         azb_invalid_txs = invalid_txs }
  | _ -> Error "alonzo_block: expected [header, [bodies], [witnesses], {metadata}, [invalid]]"

(* ================================================================ *)
(* Babbage CBOR encoding/decoding                                    *)
(* ================================================================ *)

(* -- Datum option -- *)

let encode_datum_option = function
  | Datum_hash h -> Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h]
  | Inline_datum d -> Cbor.Array [Cbor.Uint 1L; d]

let decode_datum_option = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h] when Bytes.length h = 32 ->
    Ok (Datum_hash h)
  | Cbor.Array [Cbor.Uint 1L; d] ->
    Ok (Inline_datum d)
  | _ -> Error "datum_option: expected [0, hash32] or [1, data]"

(* -- Script ref -- *)

let encode_script_ref sr =
  let inner = match sr with
    | Native_script_ref tl ->
      Cbor.Array [Cbor.Uint 0L; encode_timelock tl]
    | Plutus_v1_script_ref b ->
      Cbor.Array [Cbor.Uint 1L; Cbor.Bytes b]
    | Plutus_v2_script_ref b ->
      Cbor.Array [Cbor.Uint 2L; Cbor.Bytes b]
    | Plutus_v3_script_ref b ->
      Cbor.Array [Cbor.Uint 3L; Cbor.Bytes b]
  in
  let inner_bytes = Cbor.encode inner in
  Cbor.Tag (24L, Cbor.Bytes inner_bytes)

let decode_script_ref = function
  | Cbor.Tag (24L, Cbor.Bytes inner_bytes) ->
    let* inner = Cbor.decode inner_bytes in
    (match inner with
     | Cbor.Array [Cbor.Uint 0L; tl_cbor] ->
       let* tl = decode_timelock tl_cbor in Ok (Native_script_ref tl)
     | Cbor.Array [Cbor.Uint 1L; Cbor.Bytes b] ->
       Ok (Plutus_v1_script_ref b)
     | Cbor.Array [Cbor.Uint 2L; Cbor.Bytes b] ->
       Ok (Plutus_v2_script_ref b)
     | Cbor.Array [Cbor.Uint 3L; Cbor.Bytes b] ->
       Ok (Plutus_v3_script_ref b)
     | _ -> Error "script_ref: unrecognized inner structure")
  | _ -> Error "script_ref: expected #6.24(bytes)"

(* -- Babbage tx out (map-based) -- *)

let encode_babbage_tx_out out =
  let entries = [
    (Cbor.Uint 0L, Cbor.Bytes out.bb_address);
    (Cbor.Uint 1L, encode_value out.bb_value);
  ] in
  let entries = match out.bb_datum_option with
    | None -> entries
    | Some d -> entries @ [(Cbor.Uint 2L, encode_datum_option d)]
  in
  let entries = match out.bb_script_ref with
    | None -> entries
    | Some sr -> entries @ [(Cbor.Uint 3L, encode_script_ref sr)]
  in
  Cbor.Map entries

let decode_babbage_tx_out = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* bb_address = match find 0 with
      | Some (Cbor.Bytes b) -> Ok b
      | _ -> Error "babbage_tx_out: missing or invalid address (key 0)"
    in
    let* bb_value = match find 1 with
      | Some v -> decode_value v
      | None -> Error "babbage_tx_out: missing value (key 1)"
    in
    let* bb_datum_option = match find 2 with
      | Some v -> let* d = decode_datum_option v in Ok (Some d)
      | None -> Ok None
    in
    let* bb_script_ref = match find 3 with
      | Some v -> let* sr = decode_script_ref v in Ok (Some sr)
      | None -> Ok None
    in
    Ok { bb_address; bb_value; bb_datum_option; bb_script_ref }
  | _ -> Error "babbage_tx_out: expected map"

(* -- Babbage header body (14 elements) -- *)

let encode_babbage_header_body hb =
  Cbor.Array ([
    Cbor.Uint hb.bhb_block_number;
    Cbor.Uint hb.bhb_slot;
    (match hb.bhb_prev_hash with None -> Cbor.Null | Some h -> Cbor.Bytes h);
    Cbor.Bytes hb.bhb_issuer_vkey;
    Cbor.Bytes hb.bhb_vrf_vkey;
    encode_vrf_cert hb.bhb_vrf_result;
    Cbor.Uint hb.bhb_body_size;
    Cbor.Bytes hb.bhb_body_hash;
  ] @ encode_operational_cert hb.bhb_operational_cert @ [
    Cbor.Uint hb.bhb_protocol_version.proto_major;
    Cbor.Uint hb.bhb_protocol_version.proto_minor;
  ])

let decode_babbage_header_body = function
  | Cbor.Array [Cbor.Uint block_num; Cbor.Uint slot; prev_hash_cbor;
                Cbor.Bytes issuer; Cbor.Bytes vrf_vk;
                vrf_result_cbor;
                Cbor.Uint body_size; Cbor.Bytes body_hash;
                Cbor.Bytes hot_vk; Cbor.Uint seq; Cbor.Uint period;
                Cbor.Bytes sigma;
                Cbor.Uint proto_maj; Cbor.Uint proto_min] ->
    let* prev_hash = match prev_hash_cbor with
      | Cbor.Null -> Ok None
      | Cbor.Bytes h when Bytes.length h = 32 -> Ok (Some h)
      | _ -> Error "babbage_header_body: invalid prev_hash"
    in
    let* vrf_result = decode_vrf_cert vrf_result_cbor in
    Ok { bhb_block_number = block_num;
         bhb_slot = slot;
         bhb_prev_hash = prev_hash;
         bhb_issuer_vkey = issuer;
         bhb_vrf_vkey = vrf_vk;
         bhb_vrf_result = vrf_result;
         bhb_body_size = body_size;
         bhb_body_hash = body_hash;
         bhb_operational_cert = { hot_vkey = hot_vk; sequence_number = seq;
                                  kes_period = period; sigma };
         bhb_protocol_version = { proto_major = proto_maj; proto_minor = proto_min } }
  | _ -> Error "babbage_header_body: expected 14-element array"

(* -- Babbage header -- *)

let encode_babbage_header hdr =
  Cbor.Array [
    encode_babbage_header_body hdr.bh_header_body;
    Cbor.Bytes hdr.bh_body_signature;
  ]

let decode_babbage_header = function
  | Cbor.Array [hb_cbor; Cbor.Bytes sig_] ->
    let* hb = decode_babbage_header_body hb_cbor in
    Ok { bh_header_body = hb; bh_body_signature = sig_ }
  | _ -> Error "babbage_header: expected [header_body, signature]"

(* -- Babbage tx body -- *)

let encode_babbage_tx_body body =
  let entries = [
    (Cbor.Uint 0L, Cbor.Array (List.map encode_tx_in body.bb_inputs));
    (Cbor.Uint 1L, Cbor.Array (List.map encode_babbage_tx_out body.bb_outputs));
    (Cbor.Uint 2L, Cbor.Uint body.bb_fee);
  ] in
  let entries = match body.bb_ttl with
    | None -> entries
    | Some ttl -> entries @ [(Cbor.Uint 3L, Cbor.Uint ttl)]
  in
  let entries = match body.bb_certs with
    | None | Some [] -> entries
    | Some certs ->
      entries @ [(Cbor.Uint 4L, Cbor.Array (List.map encode_certificate certs))]
  in
  let entries = match body.bb_withdrawals with
    | None | Some [] -> entries
    | Some wds -> entries @ [(Cbor.Uint 5L, encode_withdrawals wds)]
  in
  let entries = match body.bb_update with
    | None -> entries
    | Some upd -> entries @ [(Cbor.Uint 6L, encode_update upd)]
  in
  let entries = match body.bb_auxiliary_data_hash with
    | None -> entries
    | Some hash -> entries @ [(Cbor.Uint 7L, Cbor.Bytes hash)]
  in
  let entries = match body.bb_validity_interval_start with
    | None -> entries
    | Some slot -> entries @ [(Cbor.Uint 8L, Cbor.Uint slot)]
  in
  let entries = match body.bb_mint with
    | None -> entries
    | Some ma -> entries @ [(Cbor.Uint 9L, encode_multi_asset ma)]
  in
  let entries = match body.bb_script_data_hash with
    | None -> entries
    | Some h -> entries @ [(Cbor.Uint 11L, Cbor.Bytes h)]
  in
  let entries = match body.bb_collateral with
    | None | Some [] -> entries
    | Some cols ->
      entries @ [(Cbor.Uint 13L, Cbor.Array (List.map encode_tx_in cols))]
  in
  let entries = match body.bb_required_signers with
    | None | Some [] -> entries
    | Some sigs ->
      entries @ [(Cbor.Uint 14L, Cbor.Array (List.map (fun h -> Cbor.Bytes h) sigs))]
  in
  let entries = match body.bb_network_id with
    | None -> entries
    | Some nid -> entries @ [(Cbor.Uint 15L, Cbor.Uint nid)]
  in
  let entries = match body.bb_collateral_return with
    | None -> entries
    | Some out -> entries @ [(Cbor.Uint 16L, encode_babbage_tx_out out)]
  in
  let entries = match body.bb_total_collateral with
    | None -> entries
    | Some c -> entries @ [(Cbor.Uint 17L, Cbor.Uint c)]
  in
  let entries = match body.bb_reference_inputs with
    | None | Some [] -> entries
    | Some refs ->
      entries @ [(Cbor.Uint 18L, Cbor.Array (List.map encode_tx_in refs))]
  in
  Cbor.Map entries

let decode_babbage_tx_body = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* bb_inputs = match find 0 with
      | Some (Cbor.Array items) -> list_map_result decode_tx_in items
      | _ -> Error "babbage_tx_body: missing or invalid inputs (key 0)"
    in
    let* bb_outputs = match find 1 with
      | Some (Cbor.Array items) -> list_map_result decode_babbage_tx_out items
      | _ -> Error "babbage_tx_body: missing or invalid outputs (key 1)"
    in
    let* bb_fee = match find 2 with
      | Some (Cbor.Uint n) -> Ok n
      | _ -> Error "babbage_tx_body: missing or invalid fee (key 2)"
    in
    let bb_ttl = match find 3 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* bb_certs = match find 4 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_certificate items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "babbage_tx_body: invalid certificates (key 4)"
    in
    let* bb_withdrawals = match find 5 with
      | Some v -> let* wds = decode_withdrawals v in Ok (Some wds)
      | None -> Ok None
    in
    let* bb_update = match find 6 with
      | Some v -> let* u = decode_update v in Ok (Some u)
      | None -> Ok None
    in
    let* bb_auxiliary_data_hash = match find 7 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "babbage_tx_body: invalid auxiliary_data_hash (key 7)"
    in
    let bb_validity_interval_start = match find 8 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* bb_mint = match find 9 with
      | Some v -> let* m = decode_multi_asset v in Ok (Some m)
      | None -> Ok None
    in
    let* bb_script_data_hash = match find 11 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "babbage_tx_body: invalid script_data_hash (key 11)"
    in
    let* bb_collateral = match find 13 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_tx_in items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "babbage_tx_body: invalid collateral (key 13)"
    in
    let* bb_required_signers = match find 14 with
      | Some (Cbor.Array items) ->
        let* sigs = list_map_result (fun v ->
          match v with
          | Cbor.Bytes h when Bytes.length h = 28 -> Ok h
          | _ -> Error "babbage_tx_body: invalid required_signer hash"
        ) items in Ok (Some sigs)
      | None -> Ok None
      | _ -> Error "babbage_tx_body: invalid required_signers (key 14)"
    in
    let bb_network_id = match find 15 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* bb_collateral_return = match find 16 with
      | Some v -> let* out = decode_babbage_tx_out v in Ok (Some out)
      | None -> Ok None
    in
    let* bb_total_collateral = match find 17 with
      | Some (Cbor.Uint n) -> Ok (Some n)
      | None -> Ok None
      | _ -> Error "babbage_tx_body: invalid total_collateral (key 17)"
    in
    let* bb_reference_inputs = match find 18 with
      | Some (Cbor.Array items) ->
        let* rs = list_map_result decode_tx_in items in Ok (Some rs)
      | None -> Ok None
      | _ -> Error "babbage_tx_body: invalid reference_inputs (key 18)"
    in
    Ok { bb_inputs; bb_outputs; bb_fee; bb_ttl; bb_certs; bb_withdrawals;
         bb_update; bb_auxiliary_data_hash; bb_validity_interval_start;
         bb_mint; bb_script_data_hash; bb_collateral; bb_required_signers;
         bb_network_id; bb_collateral_return; bb_total_collateral;
         bb_reference_inputs }
  | _ -> Error "babbage_tx_body: expected map"

(* -- Babbage tx witness set -- *)

let encode_babbage_tx_witness_set ws =
  let entries = [] in
  let entries = match ws.bb_vkey_witnesses with
    | None | Some [] -> entries
    | Some vkws ->
      entries @ [(Cbor.Uint 0L, Cbor.Array (List.map encode_vkey_witness vkws))]
  in
  let entries = match ws.bb_native_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 1L, Cbor.Array (List.map encode_timelock scripts))]
  in
  let entries = match ws.bb_bootstrap_witnesses with
    | None -> entries
    | Some bw -> entries @ [(Cbor.Uint 2L, bw)]
  in
  let entries = match ws.bb_plutus_v1_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 3L, Cbor.Array (List.map (fun b -> Cbor.Bytes b) scripts))]
  in
  let entries = match ws.bb_plutus_data with
    | None | Some [] -> entries
    | Some data ->
      entries @ [(Cbor.Uint 4L, Cbor.Array data)]
  in
  let entries = match ws.bb_redeemers with
    | None | Some [] -> entries
    | Some reds ->
      entries @ [(Cbor.Uint 5L, Cbor.Array (List.map encode_redeemer reds))]
  in
  let entries = match ws.bb_plutus_v2_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 6L, Cbor.Array (List.map (fun b -> Cbor.Bytes b) scripts))]
  in
  Cbor.Map entries

let decode_babbage_tx_witness_set = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* bb_vkey_witnesses = match find 0 with
      | Some (Cbor.Array items) ->
        let* ws = list_map_result decode_vkey_witness items in Ok (Some ws)
      | None -> Ok None
      | _ -> Error "babbage_tx_witness_set: invalid vkey_witnesses (key 0)"
    in
    let* bb_native_scripts = match find 1 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_timelock items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "babbage_tx_witness_set: invalid native_scripts (key 1)"
    in
    let bb_bootstrap_witnesses = find 2 in
    let* bb_plutus_v1_scripts = match find 3 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_bytes items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "babbage_tx_witness_set: invalid plutus_v1_scripts (key 3)"
    in
    let bb_plutus_data = match find 4 with
      | Some (Cbor.Array items) -> Some items
      | _ -> None
    in
    let* bb_redeemers = match find 5 with
      | Some (Cbor.Array items) ->
        let* rs = list_map_result decode_redeemer items in Ok (Some rs)
      | None -> Ok None
      | _ -> Error "babbage_tx_witness_set: invalid redeemers (key 5)"
    in
    let* bb_plutus_v2_scripts = match find 6 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_bytes items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "babbage_tx_witness_set: invalid plutus_v2_scripts (key 6)"
    in
    Ok { bb_vkey_witnesses; bb_native_scripts; bb_bootstrap_witnesses;
         bb_plutus_v1_scripts; bb_plutus_data; bb_redeemers;
         bb_plutus_v2_scripts }
  | _ -> Error "babbage_tx_witness_set: expected map"

(* -- Babbage transaction -- *)

let encode_babbage_tx tx =
  Cbor.Array [
    encode_babbage_tx_body tx.babbage_tx_body;
    encode_babbage_tx_witness_set tx.babbage_tx_witness_set;
    Cbor.Bool tx.babbage_tx_is_valid;
    (match tx.babbage_tx_auxiliary_data with None -> Cbor.Null | Some m -> m);
  ]

let decode_babbage_tx = function
  | Cbor.Array [body_cbor; ws_cbor; Cbor.Bool is_valid; aux_cbor] ->
    let* body = decode_babbage_tx_body body_cbor in
    let* ws = decode_babbage_tx_witness_set ws_cbor in
    let aux = match aux_cbor with Cbor.Null -> None | v -> Some v in
    Ok { babbage_tx_body = body;
         babbage_tx_witness_set = ws;
         babbage_tx_is_valid = is_valid;
         babbage_tx_auxiliary_data = aux }
  | _ -> Error "babbage_tx: expected [body, witnesses, bool, aux/null]"

(* -- Babbage block -- *)

let encode_babbage_block block =
  Cbor.Array [
    encode_babbage_header block.bbb_header;
    Cbor.Array (List.map encode_babbage_tx_body block.bbb_tx_bodies);
    Cbor.Array (List.map encode_babbage_tx_witness_set block.bbb_tx_witness_sets);
    Cbor.Map (List.map (fun (idx, meta) ->
      (Cbor.Uint (Int64.of_int idx), meta)
    ) block.bbb_tx_metadata);
    Cbor.Array (List.map (fun i -> Cbor.Uint (Int64.of_int i)) block.bbb_invalid_txs);
  ]

let decode_babbage_block = function
  | Cbor.Array [hdr_cbor; Cbor.Array bodies_cbor;
                Cbor.Array witnesses_cbor; Cbor.Map meta_cbor;
                Cbor.Array invalid_cbor] ->
    let* header = decode_babbage_header hdr_cbor in
    let* bodies = list_map_result decode_babbage_tx_body bodies_cbor in
    let* witnesses = list_map_result decode_babbage_tx_witness_set witnesses_cbor in
    let* metadata = list_map_result (fun (k, v) ->
      let* idx = decode_uint k in
      Ok (Int64.to_int idx, v)
    ) meta_cbor in
    let* invalid_txs = list_map_result (fun v ->
      let* n = decode_uint v in Ok (Int64.to_int n)
    ) invalid_cbor in
    Ok { bbb_header = header;
         bbb_tx_bodies = bodies;
         bbb_tx_witness_sets = witnesses;
         bbb_tx_metadata = metadata;
         bbb_invalid_txs = invalid_txs }
  | _ -> Error "babbage_block: expected [header, [bodies], [witnesses], {metadata}, [invalid]]"

(* ================================================================ *)
(* Conway CBOR encoding/decoding                                     *)
(* ================================================================ *)

(* -- Anchor -- *)

let encode_anchor a =
  Cbor.Array [Cbor.Text a.anchor_url; Cbor.Bytes a.anchor_hash]

let decode_anchor = function
  | Cbor.Array [Cbor.Text url; Cbor.Bytes hash] when Bytes.length hash = 32 ->
    Ok { anchor_url = url; anchor_hash = hash }
  | _ -> Error "anchor: expected [text, hash32]"

(* -- DRep -- *)

let encode_drep = function
  | Drep_key_hash h -> Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h]
  | Drep_script_hash h -> Cbor.Array [Cbor.Uint 1L; Cbor.Bytes h]
  | Drep_always_abstain -> Cbor.Array [Cbor.Uint 2L]
  | Drep_always_no_confidence -> Cbor.Array [Cbor.Uint 3L]

let decode_drep = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Drep_key_hash h)
  | Cbor.Array [Cbor.Uint 1L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Drep_script_hash h)
  | Cbor.Array [Cbor.Uint 2L] -> Ok Drep_always_abstain
  | Cbor.Array [Cbor.Uint 3L] -> Ok Drep_always_no_confidence
  | _ -> Error "drep: unrecognized structure"

(* -- Voter -- *)

let encode_voter = function
  | Cc_hot_key_hash h -> Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h]
  | Cc_hot_script_hash h -> Cbor.Array [Cbor.Uint 1L; Cbor.Bytes h]
  | Drep_voter_key_hash h -> Cbor.Array [Cbor.Uint 2L; Cbor.Bytes h]
  | Drep_voter_script_hash h -> Cbor.Array [Cbor.Uint 3L; Cbor.Bytes h]
  | Spo_voter h -> Cbor.Array [Cbor.Uint 4L; Cbor.Bytes h]

let decode_voter = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Cc_hot_key_hash h)
  | Cbor.Array [Cbor.Uint 1L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Cc_hot_script_hash h)
  | Cbor.Array [Cbor.Uint 2L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Drep_voter_key_hash h)
  | Cbor.Array [Cbor.Uint 3L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Drep_voter_script_hash h)
  | Cbor.Array [Cbor.Uint 4L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Spo_voter h)
  | _ -> Error "voter: unrecognized structure"

(* -- Vote -- *)

let encode_vote = function
  | Vote_no -> Cbor.Uint 0L
  | Vote_yes -> Cbor.Uint 1L
  | Vote_abstain -> Cbor.Uint 2L

let decode_vote = function
  | Cbor.Uint 0L -> Ok Vote_no
  | Cbor.Uint 1L -> Ok Vote_yes
  | Cbor.Uint 2L -> Ok Vote_abstain
  | _ -> Error "vote: expected 0, 1, or 2"

(* -- Governance action id -- *)

let encode_governance_action_id ga =
  Cbor.Array [Cbor.Bytes ga.ga_tx_id; Cbor.Uint ga.ga_index]

let decode_governance_action_id = function
  | Cbor.Array [Cbor.Bytes tx_id; Cbor.Uint idx] when Bytes.length tx_id = 32 ->
    Ok { ga_tx_id = tx_id; ga_index = idx }
  | _ -> Error "governance_action_id: expected [hash32, uint]"

(* -- Voting procedure -- *)

let encode_voting_procedure vp =
  Cbor.Array [
    encode_vote vp.vp_vote;
    (match vp.vp_anchor with None -> Cbor.Null | Some a -> encode_anchor a);
  ]

let decode_voting_procedure = function
  | Cbor.Array [vote_cbor; anchor_cbor] ->
    let* vp_vote = decode_vote vote_cbor in
    let* vp_anchor = match anchor_cbor with
      | Cbor.Null -> Ok None
      | v -> let* a = decode_anchor v in Ok (Some a)
    in
    Ok { vp_vote; vp_anchor }
  | _ -> Error "voting_procedure: expected [vote, anchor/null]"

(* -- Constitution -- *)

let encode_constitution c =
  Cbor.Array [
    encode_anchor c.constitution_anchor;
    (match c.constitution_script_hash with
     | None -> Cbor.Null
     | Some h -> Cbor.Bytes h);
  ]

let decode_constitution = function
  | Cbor.Array [anchor_cbor; script_cbor] ->
    let* constitution_anchor = decode_anchor anchor_cbor in
    let* constitution_script_hash = match script_cbor with
      | Cbor.Null -> Ok None
      | Cbor.Bytes h when Bytes.length h = 28 -> Ok (Some h)
      | _ -> Error "constitution: invalid script_hash"
    in
    Ok { constitution_anchor; constitution_script_hash }
  | _ -> Error "constitution: expected [anchor, script_hash/null]"

(* -- Governance action -- *)

let encode_opt_gov_action_id = function
  | None -> Cbor.Null
  | Some ga -> encode_governance_action_id ga

let decode_opt_gov_action_id = function
  | Cbor.Null -> Ok None
  | v -> let* ga = decode_governance_action_id v in Ok (Some ga)

let encode_governance_action = function
  | Parameter_change { prev_action; param_update; policy_hash } ->
    Cbor.Array [Cbor.Uint 0L; encode_opt_gov_action_id prev_action;
                encode_protocol_param_update param_update;
                (match policy_hash with None -> Cbor.Null | Some h -> Cbor.Bytes h)]
  | Hard_fork_initiation { prev_action; protocol_version = pv } ->
    Cbor.Array [Cbor.Uint 1L; encode_opt_gov_action_id prev_action;
                Cbor.Array [Cbor.Uint pv.proto_major; Cbor.Uint pv.proto_minor]]
  | Treasury_withdrawals { withdrawals = wds; policy_hash } ->
    Cbor.Array [Cbor.Uint 2L;
                Cbor.Map (List.map (fun (addr, amt) ->
                  (Cbor.Bytes addr, Cbor.Uint amt)) wds);
                (match policy_hash with None -> Cbor.Null | Some h -> Cbor.Bytes h)]
  | No_confidence { prev_action } ->
    Cbor.Array [Cbor.Uint 3L; encode_opt_gov_action_id prev_action]
  | Update_committee { prev_action; members_to_remove; members_to_add; quorum } ->
    Cbor.Array [Cbor.Uint 4L; encode_opt_gov_action_id prev_action;
                Cbor.Array (List.map encode_credential members_to_remove);
                Cbor.Map (List.map (fun (cred, epoch) ->
                  (encode_credential cred, Cbor.Uint epoch)) members_to_add);
                encode_rational quorum]
  | New_constitution { prev_action; constitution } ->
    Cbor.Array [Cbor.Uint 5L; encode_opt_gov_action_id prev_action;
                encode_constitution constitution]
  | Info_action ->
    Cbor.Array [Cbor.Uint 6L]

let decode_governance_action = function
  | Cbor.Array [Cbor.Uint 0L; prev_cbor; update_cbor; policy_cbor] ->
    let* prev_action = decode_opt_gov_action_id prev_cbor in
    let* param_update = decode_protocol_param_update update_cbor in
    let* policy_hash = match policy_cbor with
      | Cbor.Null -> Ok None
      | Cbor.Bytes h when Bytes.length h = 28 -> Ok (Some h)
      | _ -> Error "governance_action: invalid policy_hash"
    in
    Ok (Parameter_change { prev_action; param_update; policy_hash })
  | Cbor.Array [Cbor.Uint 1L; prev_cbor; Cbor.Array [Cbor.Uint maj; Cbor.Uint min]] ->
    let* prev_action = decode_opt_gov_action_id prev_cbor in
    Ok (Hard_fork_initiation { prev_action;
                               protocol_version = { proto_major = maj; proto_minor = min } })
  | Cbor.Array [Cbor.Uint 2L; Cbor.Map wds_cbor; policy_cbor] ->
    let* withdrawals = list_map_result (fun (k, v) ->
      let* addr = decode_bytes k in
      let* amt = decode_uint v in
      Ok (addr, amt)
    ) wds_cbor in
    let* policy_hash = match policy_cbor with
      | Cbor.Null -> Ok None
      | Cbor.Bytes h when Bytes.length h = 28 -> Ok (Some h)
      | _ -> Error "governance_action: invalid policy_hash"
    in
    Ok (Treasury_withdrawals { withdrawals; policy_hash })
  | Cbor.Array [Cbor.Uint 3L; prev_cbor] ->
    let* prev_action = decode_opt_gov_action_id prev_cbor in
    Ok (No_confidence { prev_action })
  | Cbor.Array [Cbor.Uint 4L; prev_cbor; Cbor.Array remove_cbor;
                Cbor.Map add_cbor; quorum_cbor] ->
    let* prev_action = decode_opt_gov_action_id prev_cbor in
    let* members_to_remove = list_map_result decode_credential remove_cbor in
    let* members_to_add = list_map_result (fun (k, v) ->
      let* cred = decode_credential k in
      let* epoch = decode_uint v in
      Ok (cred, epoch)
    ) add_cbor in
    let* quorum = decode_rational quorum_cbor in
    Ok (Update_committee { prev_action; members_to_remove; members_to_add; quorum })
  | Cbor.Array [Cbor.Uint 5L; prev_cbor; const_cbor] ->
    let* prev_action = decode_opt_gov_action_id prev_cbor in
    let* constitution = decode_constitution const_cbor in
    Ok (New_constitution { prev_action; constitution })
  | Cbor.Array [Cbor.Uint 6L] ->
    Ok Info_action
  | _ -> Error "governance_action: unrecognized structure"

(* -- Proposal procedure -- *)

let encode_proposal_procedure pp =
  Cbor.Array [
    Cbor.Uint pp.pp_deposit;
    Cbor.Bytes pp.pp_return_addr;
    encode_governance_action pp.pp_governance_action;
    encode_anchor pp.pp_anchor;
  ]

let decode_proposal_procedure = function
  | Cbor.Array [Cbor.Uint deposit; Cbor.Bytes return_addr; ga_cbor; anchor_cbor] ->
    let* pp_governance_action = decode_governance_action ga_cbor in
    let* pp_anchor = decode_anchor anchor_cbor in
    Ok { pp_deposit = deposit; pp_return_addr = return_addr;
         pp_governance_action; pp_anchor }
  | _ -> Error "proposal_procedure: expected [uint, bytes, gov_action, anchor]"

(* -- Conway certificate -- *)

let encode_conway_certificate = function
  | CC_Stake_registration cred ->
    Cbor.Array [Cbor.Uint 0L; encode_credential cred]
  | CC_Stake_deregistration cred ->
    Cbor.Array [Cbor.Uint 1L; encode_credential cred]
  | CC_Stake_delegation { delegator; pool } ->
    Cbor.Array [Cbor.Uint 2L; encode_credential delegator; Cbor.Bytes pool]
  | CC_Pool_registration params ->
    Cbor.Array (Cbor.Uint 3L :: encode_pool_params_flat params)
  | CC_Pool_retirement { pool; epoch } ->
    Cbor.Array [Cbor.Uint 4L; Cbor.Bytes pool; Cbor.Uint epoch]
  | CC_Reg_cert { credential; deposit } ->
    Cbor.Array [Cbor.Uint 7L; encode_credential credential; Cbor.Uint deposit]
  | CC_Unreg_cert { credential; deposit } ->
    Cbor.Array [Cbor.Uint 8L; encode_credential credential; Cbor.Uint deposit]
  | CC_Vote_deleg_cert { credential; drep } ->
    Cbor.Array [Cbor.Uint 9L; encode_credential credential; encode_drep drep]
  | CC_Stake_vote_deleg_cert { credential; pool; drep } ->
    Cbor.Array [Cbor.Uint 10L; encode_credential credential;
                Cbor.Bytes pool; encode_drep drep]
  | CC_Stake_reg_deleg_cert { credential; pool; deposit } ->
    Cbor.Array [Cbor.Uint 11L; encode_credential credential;
                Cbor.Bytes pool; Cbor.Uint deposit]
  | CC_Vote_reg_deleg_cert { credential; drep; deposit } ->
    Cbor.Array [Cbor.Uint 12L; encode_credential credential;
                encode_drep drep; Cbor.Uint deposit]
  | CC_Stake_vote_reg_deleg_cert { credential; pool; drep; deposit } ->
    Cbor.Array [Cbor.Uint 13L; encode_credential credential;
                Cbor.Bytes pool; encode_drep drep; Cbor.Uint deposit]
  | CC_Auth_committee_hot { cold; hot } ->
    Cbor.Array [Cbor.Uint 14L; encode_credential cold; encode_credential hot]
  | CC_Resign_committee_cold { cold; anchor } ->
    Cbor.Array [Cbor.Uint 15L; encode_credential cold;
                (match anchor with None -> Cbor.Null | Some a -> encode_anchor a)]
  | CC_Reg_drep { credential; deposit; anchor } ->
    Cbor.Array [Cbor.Uint 16L; encode_credential credential; Cbor.Uint deposit;
                (match anchor with None -> Cbor.Null | Some a -> encode_anchor a)]
  | CC_Unreg_drep { credential; deposit } ->
    Cbor.Array [Cbor.Uint 17L; encode_credential credential; Cbor.Uint deposit]
  | CC_Update_drep { credential; anchor } ->
    Cbor.Array [Cbor.Uint 18L; encode_credential credential;
                (match anchor with None -> Cbor.Null | Some a -> encode_anchor a)]

let decode_conway_certificate = function
  | Cbor.Array [Cbor.Uint 0L; cred] ->
    let* c = decode_credential cred in Ok (CC_Stake_registration c)
  | Cbor.Array [Cbor.Uint 1L; cred] ->
    let* c = decode_credential cred in Ok (CC_Stake_deregistration c)
  | Cbor.Array [Cbor.Uint 2L; cred; Cbor.Bytes pool] when Bytes.length pool = 28 ->
    let* c = decode_credential cred in
    Ok (CC_Stake_delegation { delegator = c; pool })
  | Cbor.Array (Cbor.Uint 3L :: rest) when List.length rest = 9 ->
    let* params = decode_pool_params_flat rest in
    Ok (CC_Pool_registration params)
  | Cbor.Array [Cbor.Uint 4L; Cbor.Bytes pool; Cbor.Uint epoch]
    when Bytes.length pool = 28 ->
    Ok (CC_Pool_retirement { pool; epoch })
  | Cbor.Array [Cbor.Uint 7L; cred; Cbor.Uint deposit] ->
    let* credential = decode_credential cred in
    Ok (CC_Reg_cert { credential; deposit })
  | Cbor.Array [Cbor.Uint 8L; cred; Cbor.Uint deposit] ->
    let* credential = decode_credential cred in
    Ok (CC_Unreg_cert { credential; deposit })
  | Cbor.Array [Cbor.Uint 9L; cred; drep_cbor] ->
    let* credential = decode_credential cred in
    let* drep = decode_drep drep_cbor in
    Ok (CC_Vote_deleg_cert { credential; drep })
  | Cbor.Array [Cbor.Uint 10L; cred; Cbor.Bytes pool; drep_cbor]
    when Bytes.length pool = 28 ->
    let* credential = decode_credential cred in
    let* drep = decode_drep drep_cbor in
    Ok (CC_Stake_vote_deleg_cert { credential; pool; drep })
  | Cbor.Array [Cbor.Uint 11L; cred; Cbor.Bytes pool; Cbor.Uint deposit]
    when Bytes.length pool = 28 ->
    let* credential = decode_credential cred in
    Ok (CC_Stake_reg_deleg_cert { credential; pool; deposit })
  | Cbor.Array [Cbor.Uint 12L; cred; drep_cbor; Cbor.Uint deposit] ->
    let* credential = decode_credential cred in
    let* drep = decode_drep drep_cbor in
    Ok (CC_Vote_reg_deleg_cert { credential; drep; deposit })
  | Cbor.Array [Cbor.Uint 13L; cred; Cbor.Bytes pool; drep_cbor; Cbor.Uint deposit]
    when Bytes.length pool = 28 ->
    let* credential = decode_credential cred in
    let* drep = decode_drep drep_cbor in
    Ok (CC_Stake_vote_reg_deleg_cert { credential; pool; drep; deposit })
  | Cbor.Array [Cbor.Uint 14L; cold_cbor; hot_cbor] ->
    let* cold = decode_credential cold_cbor in
    let* hot = decode_credential hot_cbor in
    Ok (CC_Auth_committee_hot { cold; hot })
  | Cbor.Array [Cbor.Uint 15L; cold_cbor; anchor_cbor] ->
    let* cold = decode_credential cold_cbor in
    let* anchor = match anchor_cbor with
      | Cbor.Null -> Ok None
      | v -> let* a = decode_anchor v in Ok (Some a)
    in
    Ok (CC_Resign_committee_cold { cold; anchor })
  | Cbor.Array [Cbor.Uint 16L; cred; Cbor.Uint deposit; anchor_cbor] ->
    let* credential = decode_credential cred in
    let* anchor = match anchor_cbor with
      | Cbor.Null -> Ok None
      | v -> let* a = decode_anchor v in Ok (Some a)
    in
    Ok (CC_Reg_drep { credential; deposit; anchor })
  | Cbor.Array [Cbor.Uint 17L; cred; Cbor.Uint deposit] ->
    let* credential = decode_credential cred in
    Ok (CC_Unreg_drep { credential; deposit })
  | Cbor.Array [Cbor.Uint 18L; cred; anchor_cbor] ->
    let* credential = decode_credential cred in
    let* anchor = match anchor_cbor with
      | Cbor.Null -> Ok None
      | v -> let* a = decode_anchor v in Ok (Some a)
    in
    Ok (CC_Update_drep { credential; anchor })
  | _ -> Error "conway_certificate: unrecognized structure"

(* -- Voting procedures (nested map) -- *)

let encode_voting_procedures vps =
  Cbor.Map (List.map (fun (voter, actions) ->
    (encode_voter voter,
     Cbor.Map (List.map (fun (ga_id, vp) ->
       (encode_governance_action_id ga_id, encode_voting_procedure vp)
     ) actions))
  ) vps)

let decode_voting_procedures = function
  | Cbor.Map pairs ->
    list_map_result (fun (voter_cbor, inner_cbor) ->
      let* voter = decode_voter voter_cbor in
      match inner_cbor with
      | Cbor.Map inner_pairs ->
        let* actions = list_map_result (fun (ga_cbor, vp_cbor) ->
          let* ga_id = decode_governance_action_id ga_cbor in
          let* vp = decode_voting_procedure vp_cbor in
          Ok (ga_id, vp)
        ) inner_pairs in
        Ok (voter, actions)
      | _ -> Error "voting_procedures: expected inner map"
    ) pairs
  | _ -> Error "voting_procedures: expected map"

(* -- Conway tx body -- *)

let encode_conway_tx_body body =
  let entries = [
    (Cbor.Uint 0L, Cbor.Array (List.map encode_tx_in body.cw_inputs));
    (Cbor.Uint 1L, Cbor.Array (List.map encode_babbage_tx_out body.cw_outputs));
    (Cbor.Uint 2L, Cbor.Uint body.cw_fee);
  ] in
  let entries = match body.cw_ttl with
    | None -> entries
    | Some ttl -> entries @ [(Cbor.Uint 3L, Cbor.Uint ttl)]
  in
  let entries = match body.cw_certs with
    | None | Some [] -> entries
    | Some certs ->
      entries @ [(Cbor.Uint 4L, Cbor.Array (List.map encode_conway_certificate certs))]
  in
  let entries = match body.cw_withdrawals with
    | None | Some [] -> entries
    | Some wds -> entries @ [(Cbor.Uint 5L, encode_withdrawals wds)]
  in
  (* Conway has no key 6 (update removed) *)
  let entries = match body.cw_auxiliary_data_hash with
    | None -> entries
    | Some hash -> entries @ [(Cbor.Uint 7L, Cbor.Bytes hash)]
  in
  let entries = match body.cw_validity_interval_start with
    | None -> entries
    | Some slot -> entries @ [(Cbor.Uint 8L, Cbor.Uint slot)]
  in
  let entries = match body.cw_mint with
    | None -> entries
    | Some ma -> entries @ [(Cbor.Uint 9L, encode_multi_asset ma)]
  in
  let entries = match body.cw_script_data_hash with
    | None -> entries
    | Some h -> entries @ [(Cbor.Uint 11L, Cbor.Bytes h)]
  in
  let entries = match body.cw_collateral with
    | None | Some [] -> entries
    | Some cols ->
      entries @ [(Cbor.Uint 13L, Cbor.Array (List.map encode_tx_in cols))]
  in
  let entries = match body.cw_required_signers with
    | None | Some [] -> entries
    | Some sigs ->
      entries @ [(Cbor.Uint 14L, Cbor.Array (List.map (fun h -> Cbor.Bytes h) sigs))]
  in
  let entries = match body.cw_network_id with
    | None -> entries
    | Some nid -> entries @ [(Cbor.Uint 15L, Cbor.Uint nid)]
  in
  let entries = match body.cw_collateral_return with
    | None -> entries
    | Some out -> entries @ [(Cbor.Uint 16L, encode_babbage_tx_out out)]
  in
  let entries = match body.cw_total_collateral with
    | None -> entries
    | Some c -> entries @ [(Cbor.Uint 17L, Cbor.Uint c)]
  in
  let entries = match body.cw_reference_inputs with
    | None | Some [] -> entries
    | Some refs ->
      entries @ [(Cbor.Uint 18L, Cbor.Array (List.map encode_tx_in refs))]
  in
  let entries = match body.cw_voting_procedures with
    | None | Some [] -> entries
    | Some vps -> entries @ [(Cbor.Uint 19L, encode_voting_procedures vps)]
  in
  let entries = match body.cw_proposal_procedures with
    | None | Some [] -> entries
    | Some pps ->
      entries @ [(Cbor.Uint 20L, Cbor.Array (List.map encode_proposal_procedure pps))]
  in
  let entries = match body.cw_current_treasury_value with
    | None -> entries
    | Some c -> entries @ [(Cbor.Uint 21L, Cbor.Uint c)]
  in
  let entries = match body.cw_donation with
    | None -> entries
    | Some c -> entries @ [(Cbor.Uint 22L, Cbor.Uint c)]
  in
  Cbor.Map entries

let decode_conway_tx_body = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* cw_inputs = match find 0 with
      | Some (Cbor.Array items) -> list_map_result decode_tx_in items
      | _ -> Error "conway_tx_body: missing or invalid inputs (key 0)"
    in
    let* cw_outputs = match find 1 with
      | Some (Cbor.Array items) -> list_map_result decode_babbage_tx_out items
      | _ -> Error "conway_tx_body: missing or invalid outputs (key 1)"
    in
    let* cw_fee = match find 2 with
      | Some (Cbor.Uint n) -> Ok n
      | _ -> Error "conway_tx_body: missing or invalid fee (key 2)"
    in
    let cw_ttl = match find 3 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* cw_certs = match find 4 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_conway_certificate items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid certificates (key 4)"
    in
    let* cw_withdrawals = match find 5 with
      | Some v -> let* wds = decode_withdrawals v in Ok (Some wds)
      | None -> Ok None
    in
    let* cw_auxiliary_data_hash = match find 7 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid auxiliary_data_hash (key 7)"
    in
    let cw_validity_interval_start = match find 8 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* cw_mint = match find 9 with
      | Some v -> let* m = decode_multi_asset v in Ok (Some m)
      | None -> Ok None
    in
    let* cw_script_data_hash = match find 11 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid script_data_hash (key 11)"
    in
    let* cw_collateral = match find 13 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_tx_in items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid collateral (key 13)"
    in
    let* cw_required_signers = match find 14 with
      | Some (Cbor.Array items) ->
        let* sigs = list_map_result (fun v ->
          match v with
          | Cbor.Bytes h when Bytes.length h = 28 -> Ok h
          | _ -> Error "conway_tx_body: invalid required_signer hash"
        ) items in Ok (Some sigs)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid required_signers (key 14)"
    in
    let cw_network_id = match find 15 with
      | Some (Cbor.Uint n) -> Some n
      | _ -> None
    in
    let* cw_collateral_return = match find 16 with
      | Some v -> let* out = decode_babbage_tx_out v in Ok (Some out)
      | None -> Ok None
    in
    let* cw_total_collateral = match find 17 with
      | Some (Cbor.Uint n) -> Ok (Some n)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid total_collateral (key 17)"
    in
    let* cw_reference_inputs = match find 18 with
      | Some (Cbor.Array items) ->
        let* rs = list_map_result decode_tx_in items in Ok (Some rs)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid reference_inputs (key 18)"
    in
    let* cw_voting_procedures = match find 19 with
      | Some v -> let* vps = decode_voting_procedures v in Ok (Some vps)
      | None -> Ok None
    in
    let* cw_proposal_procedures = match find 20 with
      | Some (Cbor.Array items) ->
        let* pps = list_map_result decode_proposal_procedure items in Ok (Some pps)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid proposal_procedures (key 20)"
    in
    let* cw_current_treasury_value = match find 21 with
      | Some (Cbor.Uint n) -> Ok (Some n)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid current_treasury_value (key 21)"
    in
    let* cw_donation = match find 22 with
      | Some (Cbor.Uint n) -> Ok (Some n)
      | None -> Ok None
      | _ -> Error "conway_tx_body: invalid donation (key 22)"
    in
    Ok { cw_inputs; cw_outputs; cw_fee; cw_ttl; cw_certs; cw_withdrawals;
         cw_auxiliary_data_hash; cw_validity_interval_start; cw_mint;
         cw_script_data_hash; cw_collateral; cw_required_signers;
         cw_network_id; cw_collateral_return; cw_total_collateral;
         cw_reference_inputs; cw_voting_procedures; cw_proposal_procedures;
         cw_current_treasury_value; cw_donation }
  | _ -> Error "conway_tx_body: expected map"

(* -- Conway tx witness set -- *)

let encode_conway_tx_witness_set ws =
  let entries = [] in
  let entries = match ws.cw_vkey_witnesses with
    | None | Some [] -> entries
    | Some vkws ->
      entries @ [(Cbor.Uint 0L, Cbor.Array (List.map encode_vkey_witness vkws))]
  in
  let entries = match ws.cw_native_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 1L, Cbor.Array (List.map encode_timelock scripts))]
  in
  let entries = match ws.cw_bootstrap_witnesses with
    | None -> entries
    | Some bw -> entries @ [(Cbor.Uint 2L, bw)]
  in
  let entries = match ws.cw_plutus_v1_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 3L, Cbor.Array (List.map (fun b -> Cbor.Bytes b) scripts))]
  in
  let entries = match ws.cw_plutus_data with
    | None | Some [] -> entries
    | Some data ->
      entries @ [(Cbor.Uint 4L, Cbor.Array data)]
  in
  let entries = match ws.cw_redeemers with
    | None | Some [] -> entries
    | Some reds ->
      entries @ [(Cbor.Uint 5L, Cbor.Array (List.map encode_redeemer reds))]
  in
  let entries = match ws.cw_plutus_v2_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 6L, Cbor.Array (List.map (fun b -> Cbor.Bytes b) scripts))]
  in
  let entries = match ws.cw_plutus_v3_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 7L, Cbor.Array (List.map (fun b -> Cbor.Bytes b) scripts))]
  in
  Cbor.Map entries

let decode_conway_tx_witness_set = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* cw_vkey_witnesses = match find 0 with
      | Some (Cbor.Array items) ->
        let* ws = list_map_result decode_vkey_witness items in Ok (Some ws)
      | None -> Ok None
      | _ -> Error "conway_tx_witness_set: invalid vkey_witnesses (key 0)"
    in
    let* cw_native_scripts = match find 1 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_timelock items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "conway_tx_witness_set: invalid native_scripts (key 1)"
    in
    let cw_bootstrap_witnesses = find 2 in
    let* cw_plutus_v1_scripts = match find 3 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_bytes items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "conway_tx_witness_set: invalid plutus_v1_scripts (key 3)"
    in
    let cw_plutus_data = match find 4 with
      | Some (Cbor.Array items) -> Some items
      | _ -> None
    in
    let* cw_redeemers = match find 5 with
      | Some (Cbor.Array items) ->
        let* rs = list_map_result decode_redeemer items in Ok (Some rs)
      | None -> Ok None
      | _ -> Error "conway_tx_witness_set: invalid redeemers (key 5)"
    in
    let* cw_plutus_v2_scripts = match find 6 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_bytes items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "conway_tx_witness_set: invalid plutus_v2_scripts (key 6)"
    in
    let* cw_plutus_v3_scripts = match find 7 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_bytes items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "conway_tx_witness_set: invalid plutus_v3_scripts (key 7)"
    in
    Ok { cw_vkey_witnesses; cw_native_scripts; cw_bootstrap_witnesses;
         cw_plutus_v1_scripts; cw_plutus_data; cw_redeemers;
         cw_plutus_v2_scripts; cw_plutus_v3_scripts }
  | _ -> Error "conway_tx_witness_set: expected map"

(* -- Conway transaction -- *)

let encode_conway_tx tx =
  Cbor.Array [
    encode_conway_tx_body tx.conway_tx_body;
    encode_conway_tx_witness_set tx.conway_tx_witness_set;
    Cbor.Bool tx.conway_tx_is_valid;
    (match tx.conway_tx_auxiliary_data with None -> Cbor.Null | Some m -> m);
  ]

let decode_conway_tx = function
  | Cbor.Array [body_cbor; ws_cbor; Cbor.Bool is_valid; aux_cbor] ->
    let* body = decode_conway_tx_body body_cbor in
    let* ws = decode_conway_tx_witness_set ws_cbor in
    let aux = match aux_cbor with Cbor.Null -> None | v -> Some v in
    Ok { conway_tx_body = body;
         conway_tx_witness_set = ws;
         conway_tx_is_valid = is_valid;
         conway_tx_auxiliary_data = aux }
  | _ -> Error "conway_tx: expected [body, witnesses, bool, aux/null]"

(* -- Conway block -- *)

let encode_conway_block block =
  Cbor.Array [
    encode_babbage_header block.cwb_header;
    Cbor.Array (List.map encode_conway_tx_body block.cwb_tx_bodies);
    Cbor.Array (List.map encode_conway_tx_witness_set block.cwb_tx_witness_sets);
    Cbor.Map (List.map (fun (idx, meta) ->
      (Cbor.Uint (Int64.of_int idx), meta)
    ) block.cwb_tx_metadata);
    Cbor.Array (List.map (fun i -> Cbor.Uint (Int64.of_int i)) block.cwb_invalid_txs);
  ]

let decode_conway_block = function
  | Cbor.Array [hdr_cbor; Cbor.Array bodies_cbor;
                Cbor.Array witnesses_cbor; Cbor.Map meta_cbor;
                Cbor.Array invalid_cbor] ->
    let* header = decode_babbage_header hdr_cbor in
    let* bodies = list_map_result decode_conway_tx_body bodies_cbor in
    let* witnesses = list_map_result decode_conway_tx_witness_set witnesses_cbor in
    let* metadata = list_map_result (fun (k, v) ->
      let* idx = decode_uint k in
      Ok (Int64.to_int idx, v)
    ) meta_cbor in
    let* invalid_txs = list_map_result (fun v ->
      let* n = decode_uint v in Ok (Int64.to_int n)
    ) invalid_cbor in
    Ok { cwb_header = header;
         cwb_tx_bodies = bodies;
         cwb_tx_witness_sets = witnesses;
         cwb_tx_metadata = metadata;
         cwb_invalid_txs = invalid_txs }
  | _ -> Error "conway_block: expected [header, [bodies], [witnesses], {metadata}, [invalid]]"
