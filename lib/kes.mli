(** KES key management per Ouroboros Praos. *)

type kes_key

val create : secret_key:bytes -> start_period:int64 -> max_period:int -> kes_key
val remaining_periods : kes_key -> int
val evolve : kes_key -> (unit, string) result
val sign : kes_key:kes_key -> message:bytes -> period:int64 -> (bytes, string) result

val opcert_sign :
  cold_skey:bytes -> hot_vkey:bytes ->
  sequence_number:int64 -> kes_period:int64 ->
  (Block_decoder.opcert, string) result

val opcert_encode : Block_decoder.opcert -> Cbor.cbor_value list
