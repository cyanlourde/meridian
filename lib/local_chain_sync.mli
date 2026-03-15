(** Local chain-sync mini-protocol (mini-protocol ID 5).

    Same as node-to-node chain-sync but carries full blocks
    instead of headers. Used for node-to-client communication. *)

type point = Chain_sync.point
type tip = Chain_sync.tip

type local_chain_sync_state = StIdle | StNext | StIntersect | StDone

type local_chain_sync_message =
  | MsgRequestNext
  | MsgAwaitReply
  | MsgRollForward of bytes * tip
  | MsgRollBackward of point * tip
  | MsgFindIntersect of point list
  | MsgIntersectFound of point * tip
  | MsgIntersectNotFound of tip
  | MsgDone

val agency_of : local_chain_sync_state -> Miniprotocol.agency
val state_name : local_chain_sync_state -> string
val transition : local_chain_sync_state -> local_chain_sync_message -> (local_chain_sync_state, string) result
val encode_message : local_chain_sync_message -> Cbor.cbor_value
val decode_message : Cbor.cbor_value -> (local_chain_sync_message, string) result
val to_bytes : local_chain_sync_message -> bytes
val of_bytes : bytes -> (local_chain_sync_message, string) result
