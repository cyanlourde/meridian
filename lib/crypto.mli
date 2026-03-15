(** Cryptographic primitives for Cardano.

    Blake2b hashing is implemented in pure OCaml per RFC 7693.
    Ed25519, VRF, and KES are stubs awaiting libsodium bindings. *)

(** {1 Blake2b hashing} *)

val blake2b : ?key:bytes -> nn:int -> bytes -> bytes
(** [blake2b ~nn data] computes a Blake2b hash of [data] with output
    length [nn] bytes (1..64). Optional [key] for keyed hashing. *)

val blake2b_256 : bytes -> bytes
(** 32-byte Blake2b-256 hash. *)

val blake2b_224 : bytes -> bytes
(** 28-byte Blake2b-224 hash. *)

(** {1 Ed25519 signature verification} *)

val ed25519_verify :
  public_key:bytes -> message:bytes -> signature:bytes ->
  (bool, string) result
(** Verify an Ed25519 signature. Stub: returns [Error] until
    libsodium bindings are available. *)

(** {1 VRF verification} *)

val vrf_verify :
  public_key:bytes -> proof:bytes -> message:bytes ->
  (bytes * bool, string) result
(** Verify a VRF proof. Returns [(output, is_valid)].
    Stub: returns [Error] until libsodium VRF bindings are available. *)

(** {1 KES verification} *)

val kes_verify :
  public_key:bytes -> period:int -> message:bytes -> signature:bytes ->
  (bool, string) result
(** Verify a KES signature at the given evolution period.
    Stub: returns [Error] until KES bindings are available. *)
