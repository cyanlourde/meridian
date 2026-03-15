(* Cryptographic block validation.

   Reference: Ouroboros Praos paper Section 4, Shelley formal ledger
   spec Section 14 (OCERT rule).

   The operational certificate chain:
   1. Cold key (pool registration key) signs opcert:
      cold_sig = Ed25519.sign(cold_sk, hot_vkey || seq_num || kes_period)
   2. Hot key (KES key) signs header body:
      kes_sig = KES.sign(hot_sk, header_body_hash)
   3. We can verify step 1 with Ed25519 (cold key is issuer_vkey).
      Step 2 requires KES verification (stub for now). *)

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type crypto_error =
  | Opcert_signature_invalid of string
  | Opcert_cold_key_bad_size of int
  | Opcert_hot_key_bad_size of int
  | Opcert_cold_sig_bad_size of int
  | Vrf_vkey_bad_size of int
  | Kes_signature_bad_size of { expected : int; actual : int }
  | Byron_skip  (** Byron blocks use different crypto — not checked *)

let error_to_string = function
  | Opcert_signature_invalid msg -> Printf.sprintf "opcert signature invalid: %s" msg
  | Opcert_cold_key_bad_size n -> Printf.sprintf "cold key %d bytes (expected 32)" n
  | Opcert_hot_key_bad_size n -> Printf.sprintf "hot key %d bytes (expected 32)" n
  | Opcert_cold_sig_bad_size n -> Printf.sprintf "cold sig %d bytes (expected 64)" n
  | Vrf_vkey_bad_size n -> Printf.sprintf "vrf vkey %d bytes (expected 32)" n
  | Kes_signature_bad_size { expected; actual } ->
    Printf.sprintf "KES sig %d bytes (expected %d)" actual expected
  | Byron_skip -> "byron block — crypto skipped"

(* ================================================================ *)
(* Opcert signature construction                                     *)
(* ================================================================ *)

(** Build the message that the cold key signs for the opcert.
    Per Shelley spec: the signed data is the CBOR encoding of
    (hot_vkey, sequence_number, kes_period). *)
let opcert_signed_data (oc : Block_decoder.opcert) =
  let cbor = Cbor.Array [
    Cbor.Bytes oc.oc_hot_vkey;
    Cbor.Uint oc.oc_sequence_number;
    Cbor.Uint oc.oc_kes_period;
  ] in
  Cbor.encode cbor

(* ================================================================ *)
(* Individual checks                                                 *)
(* ================================================================ *)

(** Verify the opcert Ed25519 signature: cold_key signs (hot_vkey||seq||period). *)
let verify_opcert_signature ~issuer_vkey (oc : Block_decoder.opcert) =
  let message = opcert_signed_data oc in
  match Crypto.ed25519_verify ~public_key:issuer_vkey ~message
          ~signature:oc.oc_cold_signature with
  | Ok true -> Ok ()
  | Ok false -> Error (Opcert_signature_invalid "Ed25519 verify returned false")
  | Error e -> Error (Opcert_signature_invalid e)

let verify_cold_key_size ~issuer_vkey =
  let n = Bytes.length issuer_vkey in
  if n = 32 then Ok () else Error (Opcert_cold_key_bad_size n)

let verify_hot_key_size (oc : Block_decoder.opcert) =
  let n = Bytes.length oc.oc_hot_vkey in
  if n = 32 then Ok () else Error (Opcert_hot_key_bad_size n)

let verify_cold_sig_size (oc : Block_decoder.opcert) =
  let n = Bytes.length oc.oc_cold_signature in
  if n = 64 then Ok () else Error (Opcert_cold_sig_bad_size n)

let verify_vrf_vkey_size ~vrf_vkey =
  let n = Bytes.length vrf_vkey in
  if n = 32 then Ok () else Error (Vrf_vkey_bad_size n)

(** KES signature size for depth d is (d+1)*64 + 32*d bytes.
    For Shelley/Babbage/Conway depth=6: 7*64 = 448 bytes. *)
let expected_kes_sig_size = 448

let verify_kes_sig_size ~block_signature =
  let actual = Bytes.length block_signature in
  if actual = expected_kes_sig_size then Ok ()
  else Error (Kes_signature_bad_size { expected = expected_kes_sig_size; actual })

(* ================================================================ *)
(* Combined validation                                               *)
(* ================================================================ *)

(** Run all available crypto checks on a decoded block.
    Byron blocks are skipped (different crypto scheme).
    For Shelley+, checks opcert signature, key/sig sizes, VRF key size. *)
let validate_block_crypto (block : Block_decoder.decoded_block) =
  let hdr = block.db_header in
  match block.db_era with
  | Byron -> Ok ()  (* Skip Byron — different scheme *)
  | _ ->
    let errors = ref [] in
    let check r = match r with Error e -> errors := e :: !errors | Ok () -> () in
    (* Key sizes *)
    check (verify_cold_key_size ~issuer_vkey:hdr.bh_issuer_vkey);
    check (verify_vrf_vkey_size ~vrf_vkey:hdr.bh_vrf_vkey);
    check (verify_kes_sig_size ~block_signature:hdr.bh_block_signature);
    (* Opcert checks *)
    (match hdr.bh_opcert with
     | None -> ()  (* No opcert extracted — skip *)
     | Some oc ->
       check (verify_hot_key_size oc);
       check (verify_cold_sig_size oc);
       check (verify_opcert_signature ~issuer_vkey:hdr.bh_issuer_vkey oc));
    if !errors = [] then Ok ()
    else Error (List.rev !errors)
