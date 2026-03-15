(** Ouroboros Praos consensus.

    Chain selection, slot leader eligibility, and epoch arithmetic.

    Reference: "Ouroboros Praos" (David, Gaži, Kiayias, Russell) *)

(** {1 Protocol parameters} *)

type protocol_params = {
  security_param : int;
  active_slot_coeff_num : int;
  active_slot_coeff_den : int;
  epoch_length : int64;
  slot_length_ms : int;
  slots_per_kes_period : int64;
  max_kes_evolutions : int;
}

val mainnet_params : protocol_params

(** {1 Epoch and slot arithmetic} *)

val epoch_of_slot : protocol_params -> Cardano_types.slot_number -> Cardano_types.epoch_number
val slot_in_epoch : protocol_params -> Cardano_types.slot_number -> Cardano_types.slot_number
val kes_period_of_slot : protocol_params -> Cardano_types.slot_number -> int64
val max_kes_slot : protocol_params -> int64
val kes_key_valid : protocol_params -> kes_start_period:int64 -> Cardano_types.slot_number -> bool

(** {1 Chain selection} *)

type candidate_chain = {
  chain_tip : Chain_sync.point;
  chain_block_number : Cardano_types.block_number;
  chain_slot : Cardano_types.slot_number;
}

type chain_preference = Prefer | Reject | Equal

val compare_chains :
  protocol_params ->
  our:candidate_chain -> candidate:candidate_chain ->
  fork_block_number:Cardano_types.block_number ->
  chain_preference

(** {1 Slot leader eligibility} *)

val is_slot_leader :
  protocol_params ->
  vrf_output:bytes ->
  stake_num:int64 -> stake_den:int64 ->
  bool

(** {1 Chain state} *)

type chain_state = {
  cs_tip : Chain_sync.point;
  cs_block_number : Cardano_types.block_number;
  cs_slot : Cardano_types.slot_number;
  cs_epoch : Cardano_types.epoch_number;
}

val chain_state_of_candidate : protocol_params -> candidate_chain -> chain_state
val genesis_state : chain_state
