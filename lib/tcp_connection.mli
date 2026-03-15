(** TCP connection management for Ouroboros networking.

    Handles DNS resolution, connect with timeout, send/recv, and clean shutdown. *)

type t

type connection_error =
  | Dns_error of string
  | Connection_refused of string
  | Timeout
  | Network_error of string

val error_to_string : connection_error -> string

val connect : ?timeout_s:float -> host:string -> port:int -> unit ->
  (t, connection_error) result

val send_bytes : t -> bytes -> (unit, connection_error) result

val recv_bytes : t -> int -> (bytes, connection_error) result

val file_descr : t -> Unix.file_descr

val close : t -> unit

val to_string : t -> string
