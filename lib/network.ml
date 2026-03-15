(* Ouroboros network layer.

   Wires the multiplexer to a TCP connection and provides high-level
   operations for performing the handshake and sending/receiving
   mini-protocol messages over the network.

   The wire format follows the Ouroboros network specification:
   each segment is an 8-byte header + payload, multiplexed over
   a single TCP connection.

   Segments for the same mini-protocol are concatenated into a
   per-protocol byte buffer. CBOR message boundaries are determined
   by attempting to decode from the buffer — a complete CBOR value
   is one message. *)

let ( let* ) = Result.bind

(** Per-protocol ingress buffer that accumulates segment payloads
    and yields complete CBOR-decoded messages. *)
module Proto_buffer = struct
  type t = {
    mutable data : bytes;
    mutable len : int;  (* valid bytes in data *)
  }

  let create () = { data = Bytes.create 4096; len = 0 }

  let ensure_capacity buf need =
    let cap = Bytes.length buf.data in
    if buf.len + need > cap then begin
      let new_cap = max (cap * 2) (buf.len + need) in
      let new_data = Bytes.create new_cap in
      Bytes.blit buf.data 0 new_data 0 buf.len;
      buf.data <- new_data
    end

  let append buf payload =
    let plen = Bytes.length payload in
    ensure_capacity buf plen;
    Bytes.blit payload 0 buf.data buf.len plen;
    buf.len <- buf.len + plen

  (** Try to decode one CBOR value from the front of the buffer.
      If successful, consume those bytes and return the raw bytes
      of the complete message. *)
  let try_take_message buf =
    if buf.len = 0 then None
    else
      (* Try decoding CBOR from the buffer. We need to figure out
         how many bytes the CBOR value consumes. Our Cbor.decode
         expects the ENTIRE buffer to be one value (trailing data = error).
         Instead, we use a binary search approach: try decoding
         prefixes of increasing length until we succeed. *)
      let rec try_len n =
        if n > buf.len then None
        else
          let sub = Bytes.sub buf.data 0 n in
          match Cbor.decode sub with
          | Ok _cbor ->
            (* Successfully decoded n bytes. Consume them. *)
            let remaining = buf.len - n in
            if remaining > 0 then
              Bytes.blit buf.data n buf.data 0 remaining;
            buf.len <- remaining;
            Some sub
          | Error _ ->
            (* Not enough data or malformed. Try more bytes.
               Increase by 1 for small buffers, larger jumps for big ones. *)
            try_len (n + 1)
      in
      try_len 1
end

(** A live network connection with multiplexer and per-protocol buffers. *)
type t = {
  conn : Tcp_connection.t;
  mux : Mux.mux;
  buffers : (int, Proto_buffer.t) Hashtbl.t;
}

let get_buffer t protocol_id =
  match Hashtbl.find_opt t.buffers protocol_id with
  | Some buf -> buf
  | None ->
    let buf = Proto_buffer.create () in
    Hashtbl.replace t.buffers protocol_id buf;
    buf

(** Connect to a Cardano node and set up the multiplexer. *)
let connect ?(timeout_s = 30.0) ~host ~port () =
  match Tcp_connection.connect ~timeout_s ~host ~port () with
  | Error e -> Error (Tcp_connection.error_to_string e)
  | Ok conn ->
    let mux = Mux.create ~fd:(Tcp_connection.file_descr conn) ~mode:Initiator in
    Ok { conn; mux; buffers = Hashtbl.create 8 }

(** Send a mini-protocol message as a framed mux segment. *)
let send_message t ~protocol_id payload =
  Mux.send_segment t.mux ~protocol_id ~timestamp:0l payload

(** Receive the next mux segment from the connection. *)
let recv_message t =
  Mux.recv_segment t.mux

(** Receive one complete CBOR message for a specific protocol.
    Reads mux segments, appends their payloads to the per-protocol
    buffer, and returns when a complete CBOR value can be decoded. *)
let recv_bytes_for_protocol t protocol_id =
  let buf = get_buffer t protocol_id in
  (* First check if we already have a complete message buffered *)
  match Proto_buffer.try_take_message buf with
  | Some msg_bytes -> Ok msg_bytes
  | None ->
    let rec go () =
      let* (hdr, payload) = recv_message t in
      let target_buf = get_buffer t hdr.Mux.protocol_id in
      Proto_buffer.append target_buf payload;
      (* Check if the protocol we want now has a complete message *)
      let our_buf = get_buffer t protocol_id in
      match Proto_buffer.try_take_message our_buf with
      | Some msg_bytes -> Ok msg_bytes
      | None -> go ()
    in
    go ()

(** Perform the Ouroboros handshake as initiator. *)
let perform_handshake t ~versions =
  let proposal = Handshake.propose_versions versions in
  let payload = Handshake.to_bytes proposal in
  let* () = send_message t ~protocol_id:Miniprotocol.handshake payload in
  let* response_bytes = recv_bytes_for_protocol t Miniprotocol.handshake in
  let* response = Handshake.of_bytes response_bytes in
  Handshake.handle_response ~supported:versions response

(* ================================================================ *)
(* Chain-sync                                                        *)
(* ================================================================ *)

let chain_sync_id = Miniprotocol.chain_sync

let send_chain_sync t msg =
  let payload = Chain_sync.to_bytes msg in
  send_message t ~protocol_id:chain_sync_id payload

let recv_chain_sync t =
  let* msg_bytes = recv_bytes_for_protocol t chain_sync_id in
  Chain_sync.of_bytes msg_bytes

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

let find_intersection t ~points =
  let* () = send_chain_sync t (MsgFindIntersect points) in
  let* response = recv_chain_sync t in
  match response with
  | MsgIntersectFound (point, tip) -> Ok (Some point, tip)
  | MsgIntersectNotFound tip -> Ok (None, tip)
  | _ -> Error "chain_sync: unexpected response to FindIntersect"

let request_next t =
  let* () = send_chain_sync t MsgRequestNext in
  let* response = recv_chain_sync t in
  match response with
  | MsgRollForward (header, tip) ->
    Ok (Roll_forward { header; tip })
  | MsgRollBackward (point, tip) ->
    Ok (Roll_backward { point; tip })
  | MsgAwaitReply ->
    Ok Await_reply
  | _ -> Error "chain_sync: unexpected response to RequestNext"

let await_next t =
  let* response = recv_chain_sync t in
  match response with
  | MsgRollForward (header, tip) ->
    Ok (Roll_forward { header; tip })
  | MsgRollBackward (point, tip) ->
    Ok (Roll_backward { point; tip })
  | _ -> Error "chain_sync: unexpected message while awaiting"

let chain_sync_done t =
  send_chain_sync t MsgDone

(** Extract a block point (slot, hash) from a chain-sync RollForward header.
    The n2n header is [era, #6.24(header_bytes)] where the block hash is
    Blake2b-256 of header_bytes, and the slot is in the header body. *)
let extract_point_from_header header =
  match header with
  | Cbor.Array [Cbor.Uint _era; Cbor.Tag (24L, Cbor.Bytes hdr_bytes)] ->
    let block_hash = Crypto.blake2b_256 hdr_bytes in
    (match Cbor.decode hdr_bytes with
     | Ok (Cbor.Array (Cbor.Array (_ :: Cbor.Uint slot :: _) :: _)) ->
       Ok (Chain_sync.Point (slot, block_hash))
     | Ok _ -> Ok (Chain_sync.Point (0L, block_hash))
     | Error _ -> Ok (Chain_sync.Point (0L, block_hash)))
  | _ -> Error "extract_point: unrecognized header format"

(* ================================================================ *)
(* Block-fetch                                                       *)
(* ================================================================ *)

let block_fetch_id = Miniprotocol.block_fetch

let send_block_fetch t msg =
  let payload = Block_fetch.to_bytes msg in
  send_message t ~protocol_id:block_fetch_id payload

let recv_block_fetch t =
  let* msg_bytes = recv_bytes_for_protocol t block_fetch_id in
  Block_fetch.of_bytes msg_bytes

type fetch_result =
  | Batch_started
  | No_blocks

let request_range t ~from_point ~to_point =
  let* () = send_block_fetch t (MsgRequestRange (from_point, to_point)) in
  let* response = recv_block_fetch t in
  match response with
  | MsgStartBatch -> Ok Batch_started
  | MsgNoBlocks -> Ok No_blocks
  | _ -> Error "block_fetch: unexpected response to RequestRange"

let recv_block t =
  let* response = recv_block_fetch t in
  match response with
  | MsgBlock block_bytes -> Ok (Some block_bytes)
  | MsgBatchDone -> Ok None
  | _ -> Error "block_fetch: unexpected message during streaming"

let block_fetch_done t =
  send_block_fetch t MsgClientDone

(* ================================================================ *)
(* Connection management                                             *)
(* ================================================================ *)

let close t =
  Tcp_connection.close t.conn

let remote_addr t =
  Tcp_connection.to_string t.conn
