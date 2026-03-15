(* Address decoding for Cardano.

   Parses the raw address bytes to determine address type, network ID,
   and embedded credentials. Follows the Shelley address format where
   the first byte encodes type (upper 4 bits) and network (lower 4 bits).

   Address types:
     0-3: Base address (payment + staking credential)
     4-5: Pointer address (payment + chain pointer)
     6-7: Enterprise address (payment only)
     8:   Byron/bootstrap address
     14-15: Reward account (staking only) *)

type address_type =
  | Base_addr
  | Pointer_addr
  | Enterprise_addr
  | Reward_addr
  | Byron_addr
  | Unknown_addr of int

type credential_type = Key_credential | Script_credential

type decoded_address = {
  addr_type : address_type;
  network_id : int;
  payment_credential : (credential_type * bytes) option;
  staking_credential : (credential_type * bytes) option;
  raw_bytes : bytes;
}

let credential_type_of_bit b =
  if b = 0 then Key_credential else Script_credential

let decode_address raw =
  let len = Bytes.length raw in
  if len < 1 then
    Error "address: empty"
  else
    let header = Bytes.get_uint8 raw 0 in
    let addr_type_bits = header lsr 4 in
    let network_id = header land 0x0F in
    match addr_type_bits with
    | 0 | 1 | 2 | 3 when len >= 57 ->
      let pay_type = credential_type_of_bit (addr_type_bits lsr 1) in
      let stk_type = credential_type_of_bit (addr_type_bits land 1) in
      let pay_hash = Bytes.sub raw 1 28 in
      let stk_hash = Bytes.sub raw 29 28 in
      Ok { addr_type = Base_addr; network_id;
           payment_credential = Some (pay_type, pay_hash);
           staking_credential = Some (stk_type, stk_hash);
           raw_bytes = raw }
    | 4 | 5 when len >= 30 ->
      let pay_type = credential_type_of_bit (addr_type_bits - 4) in
      let pay_hash = Bytes.sub raw 1 28 in
      Ok { addr_type = Pointer_addr; network_id;
           payment_credential = Some (pay_type, pay_hash);
           staking_credential = None;
           raw_bytes = raw }
    | 6 | 7 when len >= 29 ->
      let pay_type = credential_type_of_bit (addr_type_bits - 6) in
      let pay_hash = Bytes.sub raw 1 28 in
      Ok { addr_type = Enterprise_addr; network_id;
           payment_credential = Some (pay_type, pay_hash);
           staking_credential = None;
           raw_bytes = raw }
    | 8 ->
      Ok { addr_type = Byron_addr; network_id;
           payment_credential = None; staking_credential = None;
           raw_bytes = raw }
    | 14 | 15 when len >= 29 ->
      let stk_type = credential_type_of_bit (addr_type_bits - 14) in
      let stk_hash = Bytes.sub raw 1 28 in
      Ok { addr_type = Reward_addr; network_id;
           payment_credential = None;
           staking_credential = Some (stk_type, stk_hash);
           raw_bytes = raw }
    | t ->
      Ok { addr_type = Unknown_addr t; network_id;
           payment_credential = None; staking_credential = None;
           raw_bytes = raw }

let addr_type_name = function
  | Base_addr -> "base" | Pointer_addr -> "pointer"
  | Enterprise_addr -> "enterprise" | Reward_addr -> "reward"
  | Byron_addr -> "byron" | Unknown_addr n -> Printf.sprintf "unknown(%d)" n
