(** Chain integrity validation over stored block sequences. *)

type chain_error = {
  ce_slot : int64;
  ce_errors : string list;
}

val validate_genesis_block : Block_decoder.block_header -> (unit, string) result

val validate_chain :
  Store.store -> from_slot:int64 -> to_slot:int64 -> chain_error list

val validate_chain_tip :
  Store.store -> count:int -> chain_error list

val find_chain_breaks :
  Store.store -> from_slot:int64 -> to_slot:int64 -> chain_error list
