(** Block header validation per Cardano formal ledger spec. *)

type validation_error = string

val validate_slot_number : prev_slot:int64 -> slot:int64 -> (unit, string) result
val validate_prev_hash : expected_hash:bytes -> header:Block_decoder.block_header -> (unit, string) result
val validate_block_number : prev_block_number:int64 -> header:Block_decoder.block_header -> (unit, string) result
val validate_protocol_version : header:Block_decoder.block_header -> (unit, string) result
val validate_block_body_hash : header:Block_decoder.block_header -> raw_body_cbor:bytes -> (unit, string) result
val validate_header_size : raw_header_cbor:bytes -> max_size:int -> (unit, string) result
val validate_block_size : raw_block_cbor:bytes -> era:Block_decoder.era -> (unit, string) result
val max_block_size_for_era : Block_decoder.era -> int

val validate_header :
  prev_slot:int64 -> prev_block_number:int64 -> prev_hash:bytes ->
  header:Block_decoder.block_header ->
  raw_header_cbor:bytes -> raw_body_cbor:bytes ->
  (unit, string list) result
