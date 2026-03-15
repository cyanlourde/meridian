(** Slot leader election per Ouroboros Praos. *)

val vrf_input : slot:int64 -> epoch_nonce:bytes -> bytes
val test_vrf : slot:int64 -> epoch_nonce:bytes -> bytes * bytes

val check_leadership :
  slot:int64 -> epoch_nonce:bytes ->
  pool_stake:int64 -> total_stake:int64 ->
  active_slots_coeff_num:int -> active_slots_coeff_den:int ->
  bool * bytes * bytes

val kes_period_of_slot : slots_per_kes_period:int64 -> int64 -> int64
