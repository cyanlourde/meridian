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

(** Close the network connection. *)
let close t =
  Tcp_connection.close t.conn

(** String representation of the remote endpoint. *)
let remote_addr t =
  Tcp_connection.to_string t.conn
