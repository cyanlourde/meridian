(** TCP listener for inbound peer connections. *)

type t
val create : port:int -> t
val accept : t -> (Unix.file_descr * string) option
val close : t -> unit
val port : t -> int
val is_running : t -> bool
