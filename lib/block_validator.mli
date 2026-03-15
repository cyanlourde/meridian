(** Cryptographic block validation.

    Verifies operational certificate Ed25519 signatures, key/signature
    sizes, and VRF key validity. KES verification is a stub.

    Reference: Ouroboros Praos paper Section 4, Shelley spec Section 14 *)

type crypto_error =
  | Opcert_signature_invalid of string
  | Opcert_cold_key_bad_size of int
  | Opcert_hot_key_bad_size of int
  | Opcert_cold_sig_bad_size of int
  | Vrf_vkey_bad_size of int
  | Kes_signature_bad_size of { expected : int; actual : int }
  | Byron_skip

val error_to_string : crypto_error -> string

val opcert_signed_data : Block_decoder.opcert -> bytes
val verify_opcert_signature :
  issuer_vkey:bytes -> Block_decoder.opcert -> (unit, crypto_error) result
val verify_cold_key_size : issuer_vkey:bytes -> (unit, crypto_error) result
val verify_vrf_vkey_size : vrf_vkey:bytes -> (unit, crypto_error) result
val verify_kes_sig_size : block_signature:bytes -> (unit, crypto_error) result

val validate_block_crypto :
  Block_decoder.decoded_block -> (unit, crypto_error list) result
