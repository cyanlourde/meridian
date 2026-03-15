(** Mini-protocol definitions for the Ouroboros network layer.

    Each mini-protocol is identified by a 15-bit unsigned integer.
    Protocol IDs are assigned by the Ouroboros network specification. *)

(** {1 Protocol identifiers} *)

type mini_protocol_id = int

val handshake : mini_protocol_id
val chain_sync : mini_protocol_id
val block_fetch : mini_protocol_id
val tx_submission : mini_protocol_id
val local_state_query : mini_protocol_id
val local_tx_submission : mini_protocol_id
val local_tx_monitor : mini_protocol_id
val keep_alive : mini_protocol_id
val peer_sharing : mini_protocol_id

val protocol_name : mini_protocol_id -> string

(** {1 Protocol agency} *)

(** In each mini-protocol, exactly one peer has agency at any time. *)
type agency =
  | Client_agency
  | Server_agency
  | Nobody_agency

(** Protocol state tracks who speaks next. *)
type protocol_state = {
  protocol_id : mini_protocol_id;
  current_agency : agency;
}

val init_state : ?agency:agency -> mini_protocol_id -> protocol_state

(** {1 Handler signature} *)

(** A mini-protocol handler processes incoming messages and produces responses. *)
type handler = {
  on_message : protocol_state -> bytes -> (protocol_state * bytes list, string) result;
  initial_state : protocol_state;
}
