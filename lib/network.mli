(** Ouroboros network layer.

    Connects the multiplexer to a TCP socket and provides high-level
    operations for handshake, chain-sync, block-fetch, and mini-protocol
    messaging. Handles demultiplexing with per-protocol buffering and
    transparent keep-alive auto-response. *)

type t

val connect : ?timeout_s:float -> host:string -> port:int -> unit ->
  (t, string) result

val send_message : t -> protocol_id:Miniprotocol.mini_protocol_id ->
  bytes -> (unit, string) result

val recv_message : t -> (Mux.segment_header * bytes, string) result

val perform_handshake :
  t -> versions:(Handshake.version_number * Handshake.version_params) list ->
  (Handshake.version_number * Handshake.version_params, string) result

(** {1 Keep-alive} *)

val start_keep_alive_responder : t -> unit
(** Enable automatic keep-alive ping response. When a MsgKeepAlive
    arrives on protocol ID 8 while waiting for any other protocol's
    message, the response is sent immediately and transparently. *)

val stop_keep_alive_responder : t -> unit

val keep_alive_stats : t -> int * int * float
(** [(pings_received, responses_sent, last_ping_time)] *)

(** {1 Chain-sync} *)

type sync_event =
  | Roll_forward of { header : Cbor.cbor_value; tip : Chain_sync.tip }
  | Roll_backward of { point : Chain_sync.point; tip : Chain_sync.tip }
  | Await_reply

val find_intersection :
  t -> points:Chain_sync.point list ->
  (Chain_sync.point option * Chain_sync.tip, string) result

val request_next : t -> (sync_event, string) result
val await_next : t -> (sync_event, string) result

val pipeline_request_next :
  t -> count:int -> (sync_event list * bool, string) result
(** Send [count] MsgRequestNext pipelined, then read all responses.
    Returns [(events, at_tip)]. Eliminates N-1 round-trips per batch. *)

val chain_sync_done : t -> (unit, string) result

(** {1 Header point extraction} *)

val extract_point_from_header :
  Cbor.cbor_value -> (Chain_sync.point, string) result

(** {1 Block-fetch} *)

type fetch_result = Batch_started | No_blocks

val request_range :
  t -> from_point:Chain_sync.point -> to_point:Chain_sync.point ->
  (fetch_result, string) result

val recv_block : t -> (bytes option, string) result
val block_fetch_done : t -> (unit, string) result

(** {1 Connection management} *)

val close : t -> unit
val remote_addr : t -> string
