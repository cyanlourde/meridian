(** Minimal JSON parser. *)

type t =
  | Object of (string * t) list
  | Array of t list
  | String of string
  | Number of float
  | Bool of bool
  | Null

val parse : string -> t

val get : string -> t -> t option
val to_string : t -> string option
val to_float : t -> float option
val to_int : t -> int option
val to_int64 : t -> int64 option
val to_bool : t -> bool option
val to_object : t -> (string * t) list option
