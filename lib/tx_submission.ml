(* Ouroboros tx-submission mini-protocol (mini-protocol ID 4).

   Reference: Ouroboros network specification, "TxSubmission Mini-Protocol"

   A pull-based protocol for transaction submission. The server drives
   the interaction by requesting transaction IDs and then specific
   transactions from the client.

   State machine:
     StInit  — client has agency (sends MsgInit to start)
     StIdle  — server has agency (RequestTxIds, RequestTxs)
     StTxIds — client has agency (ReplyTxIds)
     StTxs   — client has agency (ReplyTxs)
     StDone  — nobody has agency (terminal)

   Note: The client can send MsgDone from StTxIds or StTxs to terminate.

   CBOR wire format:
     MsgRequestTxIds = [0, blocking, ack_count, req_count]
     MsgReplyTxIds   = [1, [[tx_id, tx_size], ...]]
     MsgRequestTxs   = [2, [tx_id, ...]]
     MsgReplyTxs     = [3, [tx_bytes, ...]]
     MsgDone         = [4]
     MsgInit         = [6] *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type tx_id = bytes  (** Transaction identifier (typically 32-byte hash) *)

type tx_submission_state =
  | StInit
  | StIdle
  | StTxIds
  | StTxs
  | StDone

type tx_submission_message =
  | MsgInit
  | MsgRequestTxIds of { blocking : bool; ack_count : int; req_count : int }
  | MsgReplyTxIds of (tx_id * int) list  (** (tx_id, tx_size_in_bytes) *)
  | MsgRequestTxs of tx_id list
  | MsgReplyTxs of bytes list
  | MsgDone

(* ================================================================ *)
(* State machine                                                     *)
(* ================================================================ *)

let agency_of = function
  | StInit  -> Miniprotocol.Client_agency
  | StIdle  -> Miniprotocol.Server_agency
  | StTxIds -> Miniprotocol.Client_agency
  | StTxs   -> Miniprotocol.Client_agency
  | StDone  -> Miniprotocol.Nobody_agency

let state_name = function
  | StInit -> "StInit" | StIdle -> "StIdle"
  | StTxIds -> "StTxIds" | StTxs -> "StTxs"
  | StDone -> "StDone"

let transition state msg =
  match state, msg with
  | StInit, MsgInit               -> Ok StIdle
  | StIdle, MsgRequestTxIds _     -> Ok StTxIds
  | StIdle, MsgRequestTxs _       -> Ok StTxs
  | StTxIds, MsgReplyTxIds _      -> Ok StIdle
  | StTxIds, MsgDone              -> Ok StDone
  | StTxs, MsgReplyTxs _          -> Ok StIdle
  | StTxs, MsgDone                -> Ok StDone
  | _ ->
    let msg_name = match msg with
      | MsgInit -> "MsgInit" | MsgRequestTxIds _ -> "MsgRequestTxIds"
      | MsgReplyTxIds _ -> "MsgReplyTxIds" | MsgRequestTxs _ -> "MsgRequestTxs"
      | MsgReplyTxs _ -> "MsgReplyTxs" | MsgDone -> "MsgDone"
    in
    Error (Printf.sprintf "tx_submission: invalid transition %s in %s"
             msg_name (state_name state))

(* ================================================================ *)
(* CBOR encoding/decoding                                            *)
(* ================================================================ *)

let encode_message = function
  | MsgRequestTxIds { blocking; ack_count; req_count } ->
    Cbor.Array [Cbor.Uint 0L;
                Cbor.Bool blocking;
                Cbor.Uint (Int64.of_int ack_count);
                Cbor.Uint (Int64.of_int req_count)]
  | MsgReplyTxIds entries ->
    Cbor.Array [Cbor.Uint 1L;
                Cbor.Array (List.map (fun (txid, sz) ->
                  Cbor.Array [Cbor.Bytes txid; Cbor.Uint (Int64.of_int sz)]
                ) entries)]
  | MsgRequestTxs txids ->
    Cbor.Array [Cbor.Uint 2L;
                Cbor.Array (List.map (fun txid -> Cbor.Bytes txid) txids)]
  | MsgReplyTxs txs ->
    Cbor.Array [Cbor.Uint 3L;
                Cbor.Array (List.map (fun tx -> Cbor.Bytes tx) txs)]
  | MsgDone ->
    Cbor.Array [Cbor.Uint 4L]
  | MsgInit ->
    Cbor.Array [Cbor.Uint 6L]

let decode_message = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bool blocking;
                Cbor.Uint ack; Cbor.Uint req] ->
    Ok (MsgRequestTxIds { blocking;
                          ack_count = Int64.to_int ack;
                          req_count = Int64.to_int req })
  | Cbor.Array [Cbor.Uint 1L; Cbor.Array entries] ->
    let* items = List.fold_left (fun acc e ->
      let* acc = acc in
      match e with
      | Cbor.Array [Cbor.Bytes txid; Cbor.Uint sz] ->
        Ok ((txid, Int64.to_int sz) :: acc)
      | _ -> Error "tx_submission: invalid tx_id entry"
    ) (Ok []) entries in
    Ok (MsgReplyTxIds (List.rev items))
  | Cbor.Array [Cbor.Uint 2L; Cbor.Array txids] ->
    let* ids = List.fold_left (fun acc e ->
      let* acc = acc in
      match e with
      | Cbor.Bytes txid -> Ok (txid :: acc)
      | _ -> Error "tx_submission: invalid tx_id"
    ) (Ok []) txids in
    Ok (MsgRequestTxs (List.rev ids))
  | Cbor.Array [Cbor.Uint 3L; Cbor.Array txs] ->
    let* items = List.fold_left (fun acc e ->
      let* acc = acc in
      match e with
      | Cbor.Bytes tx -> Ok (tx :: acc)
      | _ -> Error "tx_submission: invalid tx bytes"
    ) (Ok []) txs in
    Ok (MsgReplyTxs (List.rev items))
  | Cbor.Array [Cbor.Uint 4L] -> Ok MsgDone
  | Cbor.Array [Cbor.Uint 6L] -> Ok MsgInit
  | _ -> Error "tx_submission: unrecognized message"

let to_bytes msg = Cbor.encode (encode_message msg)

let of_bytes data =
  let* cbor = Cbor.decode data in
  decode_message cbor
