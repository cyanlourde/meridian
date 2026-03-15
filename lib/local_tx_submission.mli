(** Local tx-submission mini-protocol (mini-protocol ID 7).

    Simple submit/accept/reject protocol for local transaction submission. *)

type local_tx_submission_state = StIdle | StBusy | StDone

type local_tx_submission_message =
  | MsgSubmitTx of bytes
  | MsgAcceptTx
  | MsgRejectTx of Cbor.cbor_value
  | MsgDone

val agency_of : local_tx_submission_state -> Miniprotocol.agency
val state_name : local_tx_submission_state -> string
val transition : local_tx_submission_state -> local_tx_submission_message -> (local_tx_submission_state, string) result
val encode_message : local_tx_submission_message -> Cbor.cbor_value
val decode_message : Cbor.cbor_value -> (local_tx_submission_message, string) result
val to_bytes : local_tx_submission_message -> bytes
val of_bytes : bytes -> (local_tx_submission_message, string) result
