(* Local tx-monitor mini-protocol (mini-protocol ID 9).

   Reference: Ouroboros network specification, "Local Tx Monitor"

   Allows clients to monitor the local mempool: list pending transactions,
   check membership, and query sizes.

   State machine:
     StIdle      — client has agency (Acquire, Done)
     StAcquiring — server has agency (Acquired)
     StAcquired  — client has agency (NextTx, HasTx, GetSizes, Release)
     StDone      — nobody has agency

   CBOR wire format:
     MsgAcquire        = [0]
     MsgAcquired       = [1, slot_number]
     MsgNextTx         = [2]
     MsgReplyNextTx    = [3, tx_bytes / null]
     MsgHasTx          = [4, tx_id]
     MsgReplyHasTx     = [5, bool]
     MsgGetSizes       = [6]
     MsgReplyGetSizes  = [7, capacity, size, num_txs]
     MsgRelease        = [8]
     MsgDone           = [9] *)

let ( let* ) = Result.bind

type local_tx_monitor_state =
  | StIdle
  | StAcquiring
  | StAcquired
  | StDone

(** Mempool size info returned by MsgReplyGetSizes. *)
type mempool_sizes = {
  capacity : int64;    (** max bytes the mempool can hold *)
  size : int64;        (** current size in bytes *)
  num_txs : int;       (** number of transactions *)
}

type local_tx_monitor_message =
  | MsgAcquire
  | MsgAcquired of Cardano_types.slot_number
  | MsgNextTx
  | MsgReplyNextTx of bytes option
  | MsgHasTx of bytes               (* tx_id *)
  | MsgReplyHasTx of bool
  | MsgGetSizes
  | MsgReplyGetSizes of mempool_sizes
  | MsgRelease
  | MsgDone

let agency_of = function
  | StIdle      -> Miniprotocol.Client_agency
  | StAcquiring -> Miniprotocol.Server_agency
  | StAcquired  -> Miniprotocol.Client_agency
  | StDone      -> Miniprotocol.Nobody_agency

let state_name = function
  | StIdle -> "StIdle" | StAcquiring -> "StAcquiring"
  | StAcquired -> "StAcquired" | StDone -> "StDone"

let transition state msg =
  match state, msg with
  | StIdle, MsgAcquire              -> Ok StAcquiring
  | StIdle, MsgDone                 -> Ok StDone
  | StAcquiring, MsgAcquired _     -> Ok StAcquired
  | StAcquired, MsgNextTx          -> Ok StAcquired  (* server responds, then back *)
  | StAcquired, MsgReplyNextTx _   -> Ok StAcquired
  | StAcquired, MsgHasTx _         -> Ok StAcquired
  | StAcquired, MsgReplyHasTx _    -> Ok StAcquired
  | StAcquired, MsgGetSizes        -> Ok StAcquired
  | StAcquired, MsgReplyGetSizes _ -> Ok StAcquired
  | StAcquired, MsgRelease         -> Ok StIdle
  | _ ->
    let mn = match msg with
      | MsgAcquire -> "MsgAcquire" | MsgAcquired _ -> "MsgAcquired"
      | MsgNextTx -> "MsgNextTx" | MsgReplyNextTx _ -> "MsgReplyNextTx"
      | MsgHasTx _ -> "MsgHasTx" | MsgReplyHasTx _ -> "MsgReplyHasTx"
      | MsgGetSizes -> "MsgGetSizes" | MsgReplyGetSizes _ -> "MsgReplyGetSizes"
      | MsgRelease -> "MsgRelease" | MsgDone -> "MsgDone"
    in
    Error (Printf.sprintf "local_tx_monitor: invalid %s in %s" mn (state_name state))

let encode_message = function
  | MsgAcquire -> Cbor.Array [Cbor.Uint 0L]
  | MsgAcquired slot -> Cbor.Array [Cbor.Uint 1L; Cbor.Uint slot]
  | MsgNextTx -> Cbor.Array [Cbor.Uint 2L]
  | MsgReplyNextTx None -> Cbor.Array [Cbor.Uint 3L; Cbor.Null]
  | MsgReplyNextTx (Some tx) -> Cbor.Array [Cbor.Uint 3L; Cbor.Bytes tx]
  | MsgHasTx txid -> Cbor.Array [Cbor.Uint 4L; Cbor.Bytes txid]
  | MsgReplyHasTx b -> Cbor.Array [Cbor.Uint 5L; Cbor.Bool b]
  | MsgGetSizes -> Cbor.Array [Cbor.Uint 6L]
  | MsgReplyGetSizes s ->
    Cbor.Array [Cbor.Uint 7L; Cbor.Uint s.capacity; Cbor.Uint s.size;
                Cbor.Uint (Int64.of_int s.num_txs)]
  | MsgRelease -> Cbor.Array [Cbor.Uint 8L]
  | MsgDone -> Cbor.Array [Cbor.Uint 9L]

let decode_message = function
  | Cbor.Array [Cbor.Uint 0L] -> Ok MsgAcquire
  | Cbor.Array [Cbor.Uint 1L; Cbor.Uint slot] -> Ok (MsgAcquired slot)
  | Cbor.Array [Cbor.Uint 2L] -> Ok MsgNextTx
  | Cbor.Array [Cbor.Uint 3L; Cbor.Null] -> Ok (MsgReplyNextTx None)
  | Cbor.Array [Cbor.Uint 3L; Cbor.Bytes tx] -> Ok (MsgReplyNextTx (Some tx))
  | Cbor.Array [Cbor.Uint 4L; Cbor.Bytes txid] -> Ok (MsgHasTx txid)
  | Cbor.Array [Cbor.Uint 5L; Cbor.Bool b] -> Ok (MsgReplyHasTx b)
  | Cbor.Array [Cbor.Uint 6L] -> Ok MsgGetSizes
  | Cbor.Array [Cbor.Uint 7L; Cbor.Uint cap; Cbor.Uint sz; Cbor.Uint n] ->
    Ok (MsgReplyGetSizes { capacity = cap; size = sz; num_txs = Int64.to_int n })
  | Cbor.Array [Cbor.Uint 8L] -> Ok MsgRelease
  | Cbor.Array [Cbor.Uint 9L] -> Ok MsgDone
  | _ -> Error "local_tx_monitor: unrecognized message"

let to_bytes msg = Cbor.encode (encode_message msg)

let of_bytes data =
  let* cbor = Cbor.decode data in
  decode_message cbor
