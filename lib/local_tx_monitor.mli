(** Local tx-monitor mini-protocol (mini-protocol ID 9).

    Monitor the local mempool: list pending transactions,
    check membership, and query sizes. *)

type local_tx_monitor_state = StIdle | StAcquiring | StAcquired | StDone

type mempool_sizes = {
  capacity : int64;
  size : int64;
  num_txs : int;
}

type local_tx_monitor_message =
  | MsgAcquire
  | MsgAcquired of Cardano_types.slot_number
  | MsgNextTx
  | MsgReplyNextTx of bytes option
  | MsgHasTx of bytes
  | MsgReplyHasTx of bool
  | MsgGetSizes
  | MsgReplyGetSizes of mempool_sizes
  | MsgRelease
  | MsgDone

val agency_of : local_tx_monitor_state -> Miniprotocol.agency
val state_name : local_tx_monitor_state -> string
val transition : local_tx_monitor_state -> local_tx_monitor_message -> (local_tx_monitor_state, string) result
val encode_message : local_tx_monitor_message -> Cbor.cbor_value
val decode_message : Cbor.cbor_value -> (local_tx_monitor_message, string) result
val to_bytes : local_tx_monitor_message -> bytes
val of_bytes : bytes -> (local_tx_monitor_message, string) result
