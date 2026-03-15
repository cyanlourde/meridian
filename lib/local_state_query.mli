(** Local state query mini-protocol (mini-protocol ID 6).

    Allows clients to query ledger state at a specific chain point.
    Client acquires a point, issues queries, then releases. *)

type point = Chain_sync.point

type acquire_failure =
  | AcquireFailurePointTooOld
  | AcquireFailurePointNotOnChain

type local_state_query_state =
  | StIdle | StAcquiring | StAcquired | StQuerying | StDone

type local_state_query_message =
  | MsgAcquire of point option
  | MsgAcquired
  | MsgFailure of acquire_failure
  | MsgQuery of Cbor.cbor_value
  | MsgResult of Cbor.cbor_value
  | MsgRelease
  | MsgReAcquire of point option
  | MsgDone

val agency_of : local_state_query_state -> Miniprotocol.agency
val state_name : local_state_query_state -> string
val transition : local_state_query_state -> local_state_query_message -> (local_state_query_state, string) result
val encode_message : local_state_query_message -> Cbor.cbor_value
val decode_message : Cbor.cbor_value -> (local_state_query_message, string) result
val to_bytes : local_state_query_message -> bytes
val of_bytes : bytes -> (local_state_query_message, string) result
