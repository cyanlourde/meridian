(** Unix domain socket listener for local client connections. *)

type t

val create : socket_path:string -> t
val accept : t -> Unix.file_descr option
val close : t -> unit
val socket_path : t -> string
val is_running : t -> bool
