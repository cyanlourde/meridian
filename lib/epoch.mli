(** Epoch and slot arithmetic for Cardano. *)

type epoch_params = {
  byron_epoch_length : int64;
  shelley_epoch_length : int64;
  shelley_start_slot : int64;
  shelley_start_epoch : int64;
}

val mainnet_epoch_params : epoch_params
val preview_epoch_params : epoch_params

val slot_to_epoch : epoch_params -> int64 -> int64
val epoch_to_first_slot : epoch_params -> int64 -> int64
val slot_in_epoch : epoch_params -> int64 -> int64
val is_epoch_boundary : epoch_params -> prev_slot:int64 -> slot:int64 -> bool
