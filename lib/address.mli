(** Address decoding for Cardano. *)

type address_type =
  | Base_addr | Pointer_addr | Enterprise_addr
  | Reward_addr | Byron_addr | Unknown_addr of int

type credential_type = Key_credential | Script_credential

type decoded_address = {
  addr_type : address_type;
  network_id : int;
  payment_credential : (credential_type * bytes) option;
  staking_credential : (credential_type * bytes) option;
  raw_bytes : bytes;
}

val decode_address : bytes -> (decoded_address, string) result
val addr_type_name : address_type -> string
