(** Ouroboros network layer.

    Connects the multiplexer to a TCP socket and provides high-level
    operations for handshake, chain-sync, and mini-protocol messaging. *)

type t

val connect : ?timeout_s:float -> host:string -> port:int -> unit ->
  (t, string) result

val send_message : t -> protocol_id:Miniprotocol.mini_protocol_id ->
  bytes -> (unit, string) result

val recv_message : t -> (Mux.segment_header * bytes, string) result

val perform_handshake :
  t -> versions:(Handshake.version_number * Handshake.version_params) list ->
  (Handshake.version_number * Handshake.version_params, string) result

(** {1 Chain-sync} *)

type sync_event =
  | Roll_forward of {
      header : Cbor.cbor_value;
      tip : Chain_sync.tip;
    }
  | Roll_backward of {
      point : Chain_sync.point;
      tip : Chain_sync.tip;
    }
  | Await_reply

val find_intersection :
  t -> points:Chain_sync.point list ->
  (Chain_sync.point option * Chain_sync.tip, string) result

val request_next : t -> (sync_event, string) result

val await_next : t -> (sync_event, string) result

val chain_sync_done : t -> (unit, string) result

(** {1 Connection management} *)

val close : t -> unit

val remote_addr : t -> string
