(* Slot leader election per Ouroboros Praos Section 4.

   Determines if a pool is elected as slot leader for a given slot
   using VRF output compared against a stake-weighted threshold. *)

(** VRF input for slot leader check: Blake2b-256("L" || slot_bytes || nonce). *)
let vrf_input ~slot ~epoch_nonce =
  let slot_cbor = Cbor.encode (Cbor.Uint slot) in
  let buf = Buffer.create 64 in
  Buffer.add_char buf 'L';
  Buffer.add_bytes buf slot_cbor;
  Buffer.add_bytes buf epoch_nonce;
  Buffer.to_bytes buf

(** Deterministic test-mode VRF: Blake2b-256 of the VRF input.
    Not cryptographically valid but deterministic for testing. *)
let test_vrf ~slot ~epoch_nonce =
  let input = vrf_input ~slot ~epoch_nonce in
  let output = Crypto.blake2b_256 input in
  let proof = Bytes.make 80 '\x00' in
  Bytes.blit output 0 proof 0 32;
  (output, proof)

(** Check if a pool is elected as slot leader.
    Returns (is_leader, vrf_output, vrf_proof). *)
let check_leadership ~slot ~epoch_nonce ~pool_stake ~total_stake
    ~active_slots_coeff_num ~active_slots_coeff_den =
  let (vrf_output, vrf_proof) = test_vrf ~slot ~epoch_nonce in
  let is_leader = Consensus.is_slot_leader
    Consensus.{ mainnet_params with
                active_slot_coeff_num = active_slots_coeff_num;
                active_slot_coeff_den = active_slots_coeff_den }
    ~vrf_output ~stake_num:pool_stake ~stake_den:total_stake in
  (is_leader, vrf_output, vrf_proof)

(** Compute KES period for a slot. *)
let kes_period_of_slot ~slots_per_kes_period slot =
  Int64.div slot slots_per_kes_period
