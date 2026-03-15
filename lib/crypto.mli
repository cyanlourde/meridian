(** Cryptographic primitives for Cardano.

    Blake2b: pure OCaml (RFC 7693) + optional libsodium fast path.
    Ed25519: real verification via libsodium (dlopen at runtime).
    VRF/KES: stubs awaiting Cardano-specific libsodium fork. *)

(** {1 Initialization} *)

val init : unit -> unit
(** Initialize libsodium via dlopen. Must be called before using
    Ed25519 functions. Blake2b works without initialization (pure OCaml fallback). *)

val libsodium_available : bool ref
(** [true] after successful [init]. *)

(** {1 Blake2b hashing} *)

val blake2b : ?key:bytes -> nn:int -> bytes -> bytes
(** Pure OCaml Blake2b per RFC 7693. *)

val blake2b_256 : bytes -> bytes
val blake2b_224 : bytes -> bytes

val blake2b_256_sodium : bytes -> bytes
(** Libsodium fast path, falls back to pure OCaml. *)

val blake2b_224_sodium : bytes -> bytes

val blake2b_256_cross_check : bytes -> (bytes, string) result
(** Hash with both implementations, verify identical output. *)

(** {1 Ed25519} *)

val ed25519_verify :
  public_key:bytes -> message:bytes -> signature:bytes ->
  (bool, string) result

val ed25519_sign :
  secret_key:bytes -> message:bytes ->
  (bytes, string) result

val ed25519_keypair : unit -> (bytes * bytes, string) result
(** Returns [(public_key, secret_key)]. *)

(** {1 VRF (stub)} *)

val vrf_verify :
  public_key:bytes -> proof:bytes -> message:bytes ->
  (bytes * bool, string) result

(** {1 KES (stub)} *)

val kes_verify :
  public_key:bytes -> period:int -> message:bytes -> signature:bytes ->
  (bool, string) result
