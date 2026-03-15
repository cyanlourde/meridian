(* Mini-protocol definitions for the Ouroboros network layer.

   Reference: Ouroboros network specification, Section 4 "Multiplexing"

   Each mini-protocol is identified by a 15-bit unsigned integer.
   The protocol IDs are assigned by the specification:
     0 = handshake (mux-internal)
     2 = chain-sync
     3 = block-fetch
     4 = tx-submission
     5 = local-state-query
     6 = local-tx-submission
     7 = local-tx-monitor
     8 = keep-alive
     9 = peer-sharing *)

(* ================================================================ *)
(* Mini-protocol identifiers                                         *)
(* ================================================================ *)

type mini_protocol_id = int
(** 15-bit protocol number, without direction bit *)

let handshake        = 0
let chain_sync       = 2
let block_fetch      = 3
let tx_submission    = 4
let local_state_query = 5
let local_tx_submission = 6
let local_tx_monitor = 7
let keep_alive       = 8
let peer_sharing     = 9

let protocol_name = function
  | 0 -> "handshake"
  | 2 -> "chain-sync"
  | 3 -> "block-fetch"
  | 4 -> "tx-submission"
  | 5 -> "local-state-query"
  | 6 -> "local-tx-submission"
  | 7 -> "local-tx-monitor"
  | 8 -> "keep-alive"
  | 9 -> "peer-sharing"
  | n -> Printf.sprintf "unknown(%d)" n

(* ================================================================ *)
(* Protocol agency                                                   *)
(* ================================================================ *)

(** In each mini-protocol, exactly one peer has agency (the right to
    send the next message) at any given time. The peer without agency
    must wait. *)
type agency =
  | Client_agency   (** The initiator (client) may send *)
  | Server_agency   (** The responder (server) may send *)
  | Nobody_agency   (** Protocol is in a terminal state *)

(** Protocol state tracks who speaks next. *)
type protocol_state = {
  protocol_id : mini_protocol_id;
  current_agency : agency;
}

let init_state ?(agency = Client_agency) protocol_id =
  { protocol_id; current_agency = agency }

(* ================================================================ *)
(* Handler signature                                                 *)
(* ================================================================ *)

(** A mini-protocol handler processes incoming messages and produces
    outgoing responses. The handler receives raw payload bytes and
    returns a list of response payloads to send back, along with the
    updated protocol state.

    Handlers are registered with the multiplexer for a specific
    protocol ID and direction (initiator or responder). *)
type handler = {
  on_message : protocol_state -> bytes -> (protocol_state * bytes list, string) result;
  (** Called when a segment arrives for this protocol.
      Returns updated state and zero or more response payloads. *)

  initial_state : protocol_state;
  (** The initial protocol state when the handler is registered. *)
}
