(** Cardano block and transaction types for Byron and Shelley eras.

    References:
    - Byron CDDL:   cardano-ledger/eras/byron/cddl-spec/byron.cddl
    - Shelley CDDL: cardano-ledger/eras/shelley/impl/cddl-files/shelley.cddl
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

type block =
  | Byron_block of byron_block
  | Shelley_block of shelley_block

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
