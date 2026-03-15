(** Era-aware block deserialization from raw CBOR bytes. *)

type era = Byron | Shelley | Allegra | Mary | Alonzo | Babbage | Conway

type block_header = {
  bh_slot : int64;
  bh_block_number : int64;
  bh_prev_hash : bytes option;
  bh_issuer_vkey : bytes;
  bh_body_hash : bytes;
  bh_protocol_version : int64 * int64;
  bh_era : era;
}

type decoded_block = {
  db_era : era;
  db_header : block_header;
  db_tx_count : int;
  db_tx_raw : Cbor.cbor_value list;
  db_raw_cbor : Cbor.cbor_value;
}

val era_name : era -> string
val decode_block : bytes -> (decoded_block, string) result
val decode_block_header : bytes -> (block_header, string) result
