(** Ouroboros keep-alive mini-protocol (mini-protocol ID 8).

    A simple ping/pong protocol for connection liveness detection.
    Client sends a cookie; server echoes it back.

    Reference: Ouroboros network specification *)

type keep_alive_state =
  | StClient
  | StServer
  | StDone

type keep_alive_message =
  | MsgKeepAlive of int
  | MsgKeepAliveResponse of int
  | MsgDone

val agency_of : keep_alive_state -> Miniprotocol.agency
val state_name : keep_alive_state -> string
val transition : keep_alive_state -> keep_alive_message -> (keep_alive_state, string) result

val encode_message : keep_alive_message -> Cbor.cbor_value
val decode_message : Cbor.cbor_value -> (keep_alive_message, string) result

val to_bytes : keep_alive_message -> bytes
val of_bytes : bytes -> (keep_alive_message, string) result
