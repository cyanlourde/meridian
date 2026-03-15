(** CBOR encoder/decoder following RFC 8949 *)

(** CBOR data item type covering all major types 0-7.

    - [Uint]: Major type 0, unsigned integer (0 to 2^63-1 in OCaml)
    - [Nint]: Major type 1, negative integer (-2^63 to -1)
    - [Bytes]: Major type 2, definite-length byte string
    - [Text]: Major type 3, definite-length UTF-8 text string
    - [Array]: Major type 4, definite-length array
    - [Map]: Major type 5, definite-length map
    - [Tag]: Major type 6, semantic tag wrapping a data item
    - [Bool], [Null], [Undefined], [Float], [Simple]: Major type 7 *)
type cbor_value =
  | Uint of int64
  | Nint of int64
  | Bytes of bytes
  | Text of string
  | Array of cbor_value list
  | Map of (cbor_value * cbor_value) list
  | Tag of int64 * cbor_value
  | Bool of bool
  | Null
  | Undefined
  | Float of float
  | Simple of int
  | IndefiniteBytes of bytes list
  | IndefiniteText of string list
  | IndefiniteArray of cbor_value list
  | IndefiniteMap of (cbor_value * cbor_value) list

(** Encode a CBOR value to bytes. *)
val encode : cbor_value -> bytes

(** Decode bytes into a CBOR value. Returns [Error msg] on malformed input. *)
val decode : bytes -> (cbor_value, string) result

(** Encode in canonical/deterministic form per RFC 8949 Section 4.2.
    Indefinite-length items become definite, map keys are sorted by encoded
    byte order, integers use shortest encoding, floats use shortest
    precision that preserves the value. *)
val encode_canonical : cbor_value -> bytes

(** {2 Helper constructors} *)

val encode_uint : int64 -> cbor_value
val encode_bytes : bytes -> cbor_value
val encode_text : string -> cbor_value
val encode_array : cbor_value list -> cbor_value
val encode_map : (cbor_value * cbor_value) list -> cbor_value
val encode_tag : int64 -> cbor_value -> cbor_value
