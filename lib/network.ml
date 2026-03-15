(* Ouroboros network layer.

   Wires the multiplexer to a TCP connection and provides high-level
   operations for performing the handshake and sending/receiving
   mini-protocol messages over the network.

   The wire format follows the Ouroboros network specification:
   each segment is an 8-byte header + payload, multiplexed over
   a single TCP connection. *)

let ( let* ) = Result.bind

(** A live network connection with multiplexer. *)
type t = {
  conn : Tcp_connection.t;
  mux : Mux.mux;
}

(** Connect to a Cardano node and set up the multiplexer. *)
let connect ?(timeout_s = 30.0) ~host ~port () =
  match Tcp_connection.connect ~timeout_s ~host ~port () with
  | Error e -> Error (Tcp_connection.error_to_string e)
  | Ok conn ->
    let mux = Mux.create ~fd:(Tcp_connection.file_descr conn) ~mode:Initiator in
    Ok { conn; mux }

(** Send a mini-protocol message as a framed mux segment.
    The payload is the CBOR-encoded message bytes. *)
let send_message t ~protocol_id payload =
  Mux.send_segment t.mux ~protocol_id ~timestamp:0l payload

(** Receive the next mux segment from the connection.
    Returns the segment header and payload bytes. *)
let recv_message t =
  Mux.recv_segment t.mux

(** Perform the Ouroboros handshake as initiator.

    Sends ProposeVersions with the given version list, waits for
    AcceptVersion or Refuse, and returns the negotiated version
    and parameters on success.

    The handshake runs on mini-protocol ID 0 (Miniprotocol.handshake). *)
let perform_handshake t ~versions =
  (* Build and send ProposeVersions *)
  let proposal = Handshake.propose_versions versions in
  let payload = Handshake.to_bytes proposal in
  let* () = send_message t ~protocol_id:Miniprotocol.handshake payload in
  (* Receive response *)
  let* (hdr, response_bytes) = recv_message t in
  if hdr.Mux.protocol_id <> Miniprotocol.handshake then
    Error (Printf.sprintf "expected handshake response (protocol 0), got protocol %d"
             hdr.protocol_id)
  else
    let* response = Handshake.of_bytes response_bytes in
    Handshake.handle_response ~supported:versions response

(* ================================================================ *)
(* Chain-sync                                                        *)
(* ================================================================ *)

let chain_sync_id = Miniprotocol.chain_sync

(** Send a chain-sync message over the multiplexer. *)
let send_chain_sync t msg =
  let payload = Chain_sync.to_bytes msg in
  send_message t ~protocol_id:chain_sync_id payload

(** Receive and decode the next chain-sync message.
    Skips segments for other mini-protocols (e.g. keep-alive)
    and only returns chain-sync messages. *)
let recv_chain_sync t =
  let rec go () =
    let* (hdr, payload) = recv_message t in
    if hdr.Mux.protocol_id <> chain_sync_id then
      (* Skip non-chain-sync segments (e.g. keep-alive from server) *)
      go ()
    else
      Chain_sync.of_bytes payload
  in
  go ()

(** Result of a single chain-sync step. *)
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
    (** Server is at tip and needs more time. The next recv will
        deliver the actual roll-forward when a new block arrives. *)

(** Find intersection with the node's chain.
    [points] is a list of known points to check; use [[]] to start
    from genesis. Returns the intersection point and the node's
    current tip, or [None] if no intersection was found (with tip). *)
let find_intersection t ~points =
  let* () = send_chain_sync t (MsgFindIntersect points) in
  let* response = recv_chain_sync t in
  match response with
  | MsgIntersectFound (point, tip) -> Ok (Some point, tip)
  | MsgIntersectNotFound tip -> Ok (None, tip)
  | _ -> Error "chain_sync: unexpected response to FindIntersect"

(** Request the next chain update.
    Returns a [sync_event] describing what happened. *)
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

(** Wait for the next message after MsgAwaitReply.
    The server will send RollForward or RollBackward when ready. *)
let await_next t =
  let* response = recv_chain_sync t in
  match response with
  | MsgRollForward (header, tip) ->
    Ok (Roll_forward { header; tip })
  | MsgRollBackward (point, tip) ->
    Ok (Roll_backward { point; tip })
  | _ -> Error "chain_sync: unexpected message while awaiting"

(** Send MsgDone to cleanly terminate chain-sync. *)
let chain_sync_done t =
  send_chain_sync t MsgDone

(** Close the network connection. *)
let close t =
  Tcp_connection.close t.conn

(** String representation of the remote endpoint. *)
let remote_addr t =
  Tcp_connection.to_string t.conn
