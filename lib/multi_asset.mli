(** Multi-asset value type for Cardano (Mary+ eras).

    CDDL: value = coin / [coin, multiasset<uint>] *)

type policy_id = bytes
type asset_name = bytes
type assets = (policy_id * (asset_name * int64) list) list

type value = {
  lovelace : int64;
  assets : assets;
}

val zero : value
val of_lovelace : int64 -> value
val lovelace_of : value -> int64
val asset_count : value -> int
val is_lovelace_only : value -> bool

val add : value -> value -> value
val subtract : value -> value -> value
val equal : value -> value -> bool
val is_positive : value -> bool
val filter_zero : value -> value

val of_cbor : Cbor.cbor_value -> value
val mint_of_cbor : Cbor.cbor_value -> value
