(** Block construction from mempool transactions. *)

type forged_block = {
  fb_slot : int64;
  fb_block_number : int64;
  fb_prev_hash : bytes;
  fb_issuer_vkey : bytes;
  fb_body_hash : bytes;
  fb_protocol_version : int64 * int64;
  fb_tx_count : int;
  fb_total_fees : int64;
  fb_encoded : bytes;
}

val forge_block :
  slot:int64 -> block_number:int64 -> prev_hash:bytes ->
  issuer_vkey:bytes -> vrf_vkey:bytes ->
  vrf_output:bytes -> vrf_proof:bytes ->
  opcert:Block_decoder.opcert -> kes_signature:bytes ->
  protocol_version:int64 * int64 ->
  mempool:Mempool.t -> utxo:Utxo.utxo_set ->
  max_block_body_size:int ->
  forged_block
