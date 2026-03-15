(* Ouroboros network multiplexer.

   Reference: Ouroboros network specification, Section 4 "Multiplexing"

   The multiplexer frames mini-protocol messages into segments on a single
   bearer (TCP connection). Each segment has an 8-byte header:

     Byte 0-3: transmission time (32-bit big-endian, microseconds, monotonic)
     Byte 4-5: mini-protocol ID (16-bit big-endian)
               — bit 15 (MSB) encodes direction: 0 = initiator→responder,
                 1 = responder→initiator
               — bits 14-0 = protocol number
     Byte 6-7: payload length (16-bit big-endian, 0..65535)

   After the header, exactly payload_length bytes of payload follow.

   The initiator sets the direction bit to 0 when sending, and expects
   the direction bit to be 1 on received segments (since the responder
   set it). The responder does the inverse. *)

(* ================================================================ *)
(* Constants                                                         *)
(* ================================================================ *)

let header_size = 8
let max_payload_size = 65535

(* ================================================================ *)
(* Segment header                                                    *)
(* ================================================================ *)

type segment_header = {
  timestamp : int32;
  (** Transmission time in microseconds (monotonic clock) *)

  protocol_id : Miniprotocol.mini_protocol_id;
  (** 15-bit mini-protocol number (without direction bit) *)

  payload_length : int;
  (** Length of the payload that follows this header (0..65535) *)

  from_initiator : bool;
  (** True if this segment was sent by the initiator (direction bit = 0
      in the wire format). False if sent by the responder (bit = 1). *)
}

(** Encode a segment header into an 8-byte buffer.

    The direction bit is set based on [from_initiator]:
    - true  → bit 15 = 0 (initiator→responder)
    - false → bit 15 = 1 (responder→initiator) *)
let encode_segment_header hdr =
  let buf = Bytes.create header_size in
  (* Bytes 0-3: timestamp, big-endian 32-bit *)
  Bytes.set_uint8 buf 0 (Int32.to_int (Int32.shift_right_logical hdr.timestamp 24) land 0xFF);
  Bytes.set_uint8 buf 1 (Int32.to_int (Int32.shift_right_logical hdr.timestamp 16) land 0xFF);
  Bytes.set_uint8 buf 2 (Int32.to_int (Int32.shift_right_logical hdr.timestamp 8) land 0xFF);
  Bytes.set_uint8 buf 3 (Int32.to_int hdr.timestamp land 0xFF);
  (* Bytes 4-5: protocol ID with direction bit *)
  let wire_id =
    if hdr.from_initiator then hdr.protocol_id
    else hdr.protocol_id lor 0x8000
  in
  Bytes.set_uint8 buf 4 ((wire_id lsr 8) land 0xFF);
  Bytes.set_uint8 buf 5 (wire_id land 0xFF);
  (* Bytes 6-7: payload length, big-endian 16-bit *)
  Bytes.set_uint8 buf 6 ((hdr.payload_length lsr 8) land 0xFF);
  Bytes.set_uint8 buf 7 (hdr.payload_length land 0xFF);
  buf

(** Decode a segment header from an 8-byte buffer.
    Returns [Error] if the buffer is too short. *)
let decode_segment_header buf =
  if Bytes.length buf < header_size then
    Error "segment header: need 8 bytes"
  else
    let timestamp =
      let b0 = Int32.of_int (Bytes.get_uint8 buf 0) in
      let b1 = Int32.of_int (Bytes.get_uint8 buf 1) in
      let b2 = Int32.of_int (Bytes.get_uint8 buf 2) in
      let b3 = Int32.of_int (Bytes.get_uint8 buf 3) in
      Int32.logor (Int32.shift_left b0 24)
        (Int32.logor (Int32.shift_left b1 16)
           (Int32.logor (Int32.shift_left b2 8) b3))
    in
    let raw_id =
      (Bytes.get_uint8 buf 4 lsl 8) lor Bytes.get_uint8 buf 5
    in
    let from_initiator = (raw_id land 0x8000) = 0 in
    let protocol_id = raw_id land 0x7FFF in
    let payload_length =
      (Bytes.get_uint8 buf 6 lsl 8) lor Bytes.get_uint8 buf 7
    in
    Ok { timestamp; protocol_id; payload_length; from_initiator }

(* ================================================================ *)
(* Multiplexer                                                       *)
(* ================================================================ *)

(** Role of this endpoint on the bearer. *)
type mode =
  | Initiator  (** This side initiated the connection *)
  | Responder  (** This side accepted the connection *)

type mux = {
  fd : Unix.file_descr;
  (** The underlying TCP socket or pipe *)

  mode : mode;
  (** Whether this endpoint is the initiator or responder *)

  handlers : (Miniprotocol.mini_protocol_id, handler_entry) Hashtbl.t;
  (** Registered mini-protocol handlers, keyed by protocol ID *)
}

and handler_entry = {
  handler : Miniprotocol.handler;
  mutable state : Miniprotocol.protocol_state;
}

let create ~fd ~mode =
  { fd; mode; handlers = Hashtbl.create 8 }

let register mux (handler : Miniprotocol.handler) =
  let pid = handler.initial_state.protocol_id in
  Hashtbl.replace mux.handlers pid
    { handler; state = handler.initial_state }

(* ================================================================ *)
(* I/O helpers                                                       *)
(* ================================================================ *)

(** Read exactly [n] bytes from a file descriptor. *)
let read_exact fd n =
  let buf = Bytes.create n in
  let rec go off remaining =
    if remaining = 0 then Ok buf
    else
      match Unix.read fd buf off remaining with
      | 0 -> Error "connection closed"
      | k -> go (off + k) (remaining - k)
      | exception Unix.Unix_error (e, _, _) ->
        Error (Printf.sprintf "read: %s" (Unix.error_message e))
  in
  go 0 n

(** Write all bytes to a file descriptor. *)
let write_all fd buf =
  let len = Bytes.length buf in
  let rec go off remaining =
    if remaining = 0 then Ok ()
    else
      match Unix.write fd buf off remaining with
      | 0 -> Error "write: zero bytes written"
      | k -> go (off + k) (remaining - k)
      | exception Unix.Unix_error (e, _, _) ->
        Error (Printf.sprintf "write: %s" (Unix.error_message e))
  in
  go 0 len

(* ================================================================ *)
(* Send / receive segments                                           *)
(* ================================================================ *)

let ( let* ) = Result.bind

(** Send a framed segment for the given mini-protocol.
    The direction bit is set automatically based on the mux mode.
    Payloads larger than [max_payload_size] are rejected. *)
let send_segment mux ~protocol_id ~timestamp payload =
  let payload_length = Bytes.length payload in
  if payload_length > max_payload_size then
    Error (Printf.sprintf "payload too large: %d > %d" payload_length max_payload_size)
  else
    let from_initiator = (mux.mode = Initiator) in
    let hdr = { timestamp; protocol_id; payload_length; from_initiator } in
    let hdr_bytes = encode_segment_header hdr in
    let* () = write_all mux.fd hdr_bytes in
    if payload_length > 0 then
      write_all mux.fd payload
    else
      Ok ()

(** Receive the next segment from the bearer.
    Returns the decoded header and the payload bytes. *)
let recv_segment mux =
  let* hdr_bytes = read_exact mux.fd header_size in
  let* hdr = decode_segment_header hdr_bytes in
  if hdr.payload_length > max_payload_size then
    Error (Printf.sprintf "received payload length %d exceeds max %d"
             hdr.payload_length max_payload_size)
  else if hdr.payload_length = 0 then
    Ok (hdr, Bytes.empty)
  else
    let* payload = read_exact mux.fd hdr.payload_length in
    Ok (hdr, payload)

(** Receive a segment and dispatch it to the registered handler.
    Returns the handler's response payloads (if any), which the caller
    should send back via [send_segment]. *)
let recv_and_dispatch mux =
  let* (hdr, payload) = recv_segment mux in
  match Hashtbl.find_opt mux.handlers hdr.protocol_id with
  | None ->
    Error (Printf.sprintf "no handler for protocol %d (%s)"
             hdr.protocol_id
             (Miniprotocol.protocol_name hdr.protocol_id))
  | Some entry ->
    let* (new_state, responses) = entry.handler.on_message entry.state payload in
    entry.state <- new_state;
    Ok (hdr.protocol_id, responses)
