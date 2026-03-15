(** KES (Key Evolving Signature) — sum composition per Ouroboros Praos.

    Depth 6 = 64 periods. Built on Ed25519 via libsodium. *)

type kes_key

val generate : depth:int -> (kes_key, string) result
val sign : kes_key:kes_key -> message:bytes -> period:int -> (bytes, string) result
val verify : vkey:bytes -> signature:bytes -> message:bytes -> period:int -> depth:int -> bool

val create : secret_key:bytes -> start_period:int64 -> max_period:int -> kes_key
val remaining_periods : kes_key -> int
val evolve : kes_key -> (unit, string) result
val period : kes_key -> int
val vkey_of_key : kes_key -> bytes
val depth : kes_key -> int

val opcert_sign :
  cold_skey:bytes -> hot_vkey:bytes ->
  sequence_number:int64 -> kes_period:int64 ->
  (Block_decoder.opcert, string) result

val opcert_encode : Block_decoder.opcert -> Cbor.cbor_value list
