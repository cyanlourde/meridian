(** Ouroboros tx-submission mini-protocol (mini-protocol ID 4).

    A pull-based protocol for transaction submission. The server
    requests transaction IDs and then specific transactions from
    the client.

    Reference: Ouroboros network specification *)

type tx_id = bytes

type tx_submission_state =
  | StInit
  | StIdle
  | StTxIds
  | StTxs
  | StDone

type tx_submission_message =
  | MsgInit
  | MsgRequestTxIds of { blocking : bool; ack_count : int; req_count : int }
  | MsgReplyTxIds of (tx_id * int) list
  | MsgRequestTxs of tx_id list
  | MsgReplyTxs of bytes list
  | MsgDone

val agency_of : tx_submission_state -> Miniprotocol.agency
val state_name : tx_submission_state -> string
val transition : tx_submission_state -> tx_submission_message -> (tx_submission_state, string) result

val encode_message : tx_submission_message -> Cbor.cbor_value
val decode_message : Cbor.cbor_value -> (tx_submission_message, string) result

val to_bytes : tx_submission_message -> bytes
val of_bytes : bytes -> (tx_submission_message, string) result
