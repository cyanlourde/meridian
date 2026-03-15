(** Ouroboros network layer.

    Connects the multiplexer to a TCP socket and provides high-level
    operations for handshake and mini-protocol messaging. *)

type t

val connect : ?timeout_s:float -> host:string -> port:int -> unit ->
  (t, string) result

val send_message : t -> protocol_id:Miniprotocol.mini_protocol_id ->
  bytes -> (unit, string) result

val recv_message : t -> (Mux.segment_header * bytes, string) result

val perform_handshake :
  t -> versions:(Handshake.version_number * Handshake.version_params) list ->
  (Handshake.version_number * Handshake.version_params, string) result

val close : t -> unit

val remote_addr : t -> string
