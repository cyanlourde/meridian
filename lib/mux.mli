(** Ouroboros network multiplexer.

    Frames mini-protocol messages into segments on a single bearer
    (TCP connection). Each segment has an 8-byte header followed by
    a payload of up to 65535 bytes.

    Reference: Ouroboros network specification, Section 4 "Multiplexing" *)

(** {1 Constants} *)

val header_size : int
(** Size of the segment header in bytes (8). *)

val max_payload_size : int
(** Maximum payload size per segment (65535). *)

(** {1 Segment header} *)

type segment_header = {
  timestamp : int32;
  protocol_id : Miniprotocol.mini_protocol_id;
  payload_length : int;
  from_initiator : bool;
}

val encode_segment_header : segment_header -> bytes
(** Encode a segment header to 8 bytes. *)

val decode_segment_header : bytes -> (segment_header, string) result
(** Decode a segment header from at least 8 bytes. *)

(** {1 Multiplexer} *)

type mode =
  | Initiator
  | Responder

type mux
(** An opaque multiplexer instance. *)

val create : fd:Unix.file_descr -> mode:mode -> mux
(** Create a multiplexer over the given file descriptor. *)

val register : mux -> Miniprotocol.handler -> unit
(** Register a mini-protocol handler. *)

val send_segment :
  mux -> protocol_id:Miniprotocol.mini_protocol_id ->
  timestamp:int32 -> bytes -> (unit, string) result
(** Send a framed segment. Direction bit is set from the mux mode. *)

val recv_segment : mux -> (segment_header * bytes, string) result
(** Read the next segment (header + payload) from the bearer. *)

val recv_and_dispatch :
  mux -> (Miniprotocol.mini_protocol_id * bytes list, string) result
(** Read a segment and dispatch it to the registered handler.
    Returns the protocol ID and any response payloads. *)
