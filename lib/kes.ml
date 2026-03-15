(* KES key management per Ouroboros Praos Section 6.

   KES (Key Evolving Signature) provides forward security: after evolving
   to period N, the key can no longer sign for periods < N.

   Cardano uses sum_6 composition: 2^6 = 64 max periods.
   Signature size: 448 bytes (7 * 64 for depth 6).

   NOTE: Real KES signing requires the Cardano libsodium fork.
   This module provides the structure with Ed25519-based stubs. *)

type kes_key = {
  mutable current_period : int64;
  secret_key : bytes;
  max_period : int;
}

let create ~secret_key ~start_period ~max_period =
  { current_period = start_period; secret_key; max_period }

let remaining_periods key =
  Int64.to_int (Int64.sub (Int64.of_int key.max_period) key.current_period)

let evolve key =
  if remaining_periods key > 0 then begin
    key.current_period <- Int64.add key.current_period 1L;
    Ok ()
  end else
    Error "KES key exhausted"

(** Sign a message with KES. Stub: returns a 448-byte placeholder. *)
let sign ~kes_key:_ ~message:_ ~period:_ =
  (* Real KES not available — return placeholder signature *)
  Ok (Bytes.make 448 '\x00')

(** Sign an operational certificate with the cold key (real Ed25519). *)
let opcert_sign ~cold_skey ~hot_vkey ~sequence_number ~kes_period =
  let msg = Cbor.encode (Cbor.Array [
    Cbor.Bytes hot_vkey;
    Cbor.Uint sequence_number;
    Cbor.Uint kes_period;
  ]) in
  match Crypto.ed25519_sign ~secret_key:cold_skey ~message:msg with
  | Ok signature ->
    Ok Block_decoder.{
      oc_hot_vkey = hot_vkey;
      oc_sequence_number = sequence_number;
      oc_kes_period = kes_period;
      oc_cold_signature = signature;
    }
  | Error e -> Error e

(** Encode an opcert as CBOR for block header inclusion. *)
let opcert_encode (oc : Block_decoder.opcert) =
  [ Cbor.Bytes oc.oc_hot_vkey;
    Cbor.Uint oc.oc_sequence_number;
    Cbor.Uint oc.oc_kes_period;
    Cbor.Bytes oc.oc_cold_signature ]
