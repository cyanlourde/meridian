(** Ouroboros block-fetch mini-protocol (mini-protocol ID 3).

    Downloads ranges of blocks. Client requests a range of points;
    server streams the corresponding blocks.

    Reference: Ouroboros network specification *)

type point = Chain_sync.point

type block_fetch_state =
  | StIdle
  | StBusy
  | StStreaming
  | StDone

type block_fetch_message =
  | MsgRequestRange of point * point
  | MsgClientDone
  | MsgStartBatch
  | MsgNoBlocks
  | MsgBlock of bytes
  | MsgBatchDone

val agency_of : block_fetch_state -> Miniprotocol.agency
val state_name : block_fetch_state -> string
val transition : block_fetch_state -> block_fetch_message -> (block_fetch_state, string) result

val encode_message : block_fetch_message -> Cbor.cbor_value
val decode_message : Cbor.cbor_value -> (block_fetch_message, string) result

val to_bytes : block_fetch_message -> bytes
val of_bytes : bytes -> (block_fetch_message, string) result
