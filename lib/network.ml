(* Ouroboros network layer.

   Wires the multiplexer to a TCP connection and provides high-level
   operations for performing the handshake and sending/receiving
   mini-protocol messages over the network.

   Segments for the same mini-protocol are concatenated into a
   per-protocol byte buffer. CBOR message boundaries are determined
   by attempting to decode from the buffer.

   When keep-alive is enabled, incoming MsgKeepAlive messages are
   automatically answered with MsgKeepAliveResponse — transparently
   to all other protocol operations. *)

let ( let* ) = Result.bind

(** Per-protocol ingress buffer. *)
module Proto_buffer = struct
  type t = {
    mutable data : bytes;
    mutable len : int;
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

  let try_take_message buf =
    if buf.len = 0 then None
    else
      let rec try_len n =
        if n > buf.len then None
        else
          let sub = Bytes.sub buf.data 0 n in
          match Cbor.decode sub with
          | Ok _cbor ->
            let remaining = buf.len - n in
            if remaining > 0 then
              Bytes.blit buf.data n buf.data 0 remaining;
            buf.len <- remaining;
            Some sub
          | Error _ -> try_len (n + 1)
      in
      try_len 1
end

(** Keep-alive statistics. *)
type keep_alive_stats = {
  mutable pings_received : int;
  mutable responses_sent : int;
  mutable last_ping_time : float;
}

(** A live network connection with multiplexer, per-protocol buffers,
    and optional keep-alive auto-responder. *)
type t = {
  conn : Tcp_connection.t;
  mux : Mux.mux;
  buffers : (int, Proto_buffer.t) Hashtbl.t;
  mutable keep_alive_active : bool;
  ka_stats : keep_alive_stats;
}

let get_buffer t protocol_id =
  match Hashtbl.find_opt t.buffers protocol_id with
  | Some buf -> buf
  | None ->
    let buf = Proto_buffer.create () in
    Hashtbl.replace t.buffers protocol_id buf;
    buf

let connect ?(timeout_s = 30.0) ~host ~port () =
  match Tcp_connection.connect ~timeout_s ~host ~port () with
  | Error e -> Error (Tcp_connection.error_to_string e)
  | Ok conn ->
    let mux = Mux.create ~fd:(Tcp_connection.file_descr conn) ~mode:Initiator in
    Ok { conn; mux;
         buffers = Hashtbl.create 8;
         keep_alive_active = false;
         ka_stats = { pings_received = 0; responses_sent = 0;
                      last_ping_time = 0.0 } }

let send_message t ~protocol_id payload =
  Mux.send_segment t.mux ~protocol_id ~timestamp:0l payload

let recv_message t =
  Mux.recv_segment t.mux

(* ================================================================ *)
(* Keep-alive auto-responder                                         *)
(* ================================================================ *)

let keep_alive_id = Miniprotocol.keep_alive

(** Process any complete keep-alive messages in the buffer.
    Decodes MsgKeepAlive, sends MsgKeepAliveResponse with the same
    cookie, and updates stats. Runs in a loop until no more complete
    keep-alive messages are buffered. *)
let handle_pending_keep_alive t =
  if not t.keep_alive_active then Ok ()
  else
    let ka_buf = get_buffer t keep_alive_id in
    let rec drain () =
      match Proto_buffer.try_take_message ka_buf with
      | None -> Ok ()
      | Some msg_bytes ->
        (match Keep_alive.of_bytes msg_bytes with
         | Ok (Keep_alive.MsgKeepAlive cookie) ->
           t.ka_stats.pings_received <- t.ka_stats.pings_received + 1;
           t.ka_stats.last_ping_time <- Unix.gettimeofday ();
           let response = Keep_alive.to_bytes (MsgKeepAliveResponse cookie) in
           let* () = send_message t ~protocol_id:keep_alive_id response in
           t.ka_stats.responses_sent <- t.ka_stats.responses_sent + 1;
           drain ()
         | Ok _ -> drain ()  (* Ignore other keep-alive messages *)
         | Error _ -> drain ())
    in
    drain ()

(** Enable the keep-alive auto-responder. *)
let start_keep_alive_responder t =
  t.keep_alive_active <- true

(** Disable the keep-alive auto-responder. *)
let stop_keep_alive_responder t =
  t.keep_alive_active <- false

(** Get keep-alive statistics. *)
let keep_alive_stats t =
  (t.ka_stats.pings_received, t.ka_stats.responses_sent, t.ka_stats.last_ping_time)

(* ================================================================ *)
(* Protocol-aware receive with keep-alive handling                   *)
(* ================================================================ *)

(** Receive one complete CBOR message for a specific protocol.
    Automatically handles keep-alive pings that arrive while waiting. *)
let recv_bytes_for_protocol t protocol_id =
  let buf = get_buffer t protocol_id in
  match Proto_buffer.try_take_message buf with
  | Some msg_bytes -> Ok msg_bytes
  | None ->
    let rec go () =
      let* (hdr, payload) = recv_message t in
      let target_buf = get_buffer t hdr.Mux.protocol_id in
      Proto_buffer.append target_buf payload;
      (* Handle any keep-alive pings that arrived *)
      let* () = handle_pending_keep_alive t in
      (* Check if the protocol we want now has a complete message *)
      let our_buf = get_buffer t protocol_id in
      match Proto_buffer.try_take_message our_buf with
      | Some msg_bytes -> Ok msg_bytes
      | None -> go ()
    in
    go ()

(* ================================================================ *)
(* Handshake                                                         *)
(* ================================================================ *)

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
  | Roll_forward of { header : Cbor.cbor_value; tip : Chain_sync.tip }
  | Roll_backward of { point : Chain_sync.point; tip : Chain_sync.tip }
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
  | MsgRollForward (header, tip) -> Ok (Roll_forward { header; tip })
  | MsgRollBackward (point, tip) -> Ok (Roll_backward { point; tip })
  | MsgAwaitReply -> Ok Await_reply
  | _ -> Error "chain_sync: unexpected response to RequestNext"

let await_next t =
  let* response = recv_chain_sync t in
  match response with
  | MsgRollForward (header, tip) -> Ok (Roll_forward { header; tip })
  | MsgRollBackward (point, tip) -> Ok (Roll_backward { point; tip })
  | _ -> Error "chain_sync: unexpected message while awaiting"

let chain_sync_done t =
  send_chain_sync t MsgDone

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

type fetch_result = Batch_started | No_blocks

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
