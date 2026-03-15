(* Local tx-submission mini-protocol (mini-protocol ID 7).

   Reference: Ouroboros network specification, "Local Tx Submission"

   Simple request/response protocol for submitting a single transaction
   to the local node via Unix socket.

   State machine:
     StIdle — client has agency (SubmitTx, Done)
     StBusy — server has agency (AcceptTx, RejectTx)
     StDone — nobody has agency

   CBOR wire format:
     MsgSubmitTx = [0, tx_bytes]
     MsgAcceptTx = [1]
     MsgRejectTx = [2, reason]
     MsgDone     = [3] *)

let ( let* ) = Result.bind

type local_tx_submission_state =
  | StIdle
  | StBusy
  | StDone

type local_tx_submission_message =
  | MsgSubmitTx of bytes
  | MsgAcceptTx
  | MsgRejectTx of Cbor.cbor_value   (* rejection reason is era-specific *)
  | MsgDone

let agency_of = function
  | StIdle -> Miniprotocol.Client_agency
  | StBusy -> Miniprotocol.Server_agency
  | StDone -> Miniprotocol.Nobody_agency

let state_name = function
  | StIdle -> "StIdle" | StBusy -> "StBusy" | StDone -> "StDone"

let transition state msg =
  match state, msg with
  | StIdle, MsgSubmitTx _ -> Ok StBusy
  | StIdle, MsgDone       -> Ok StDone
  | StBusy, MsgAcceptTx   -> Ok StIdle
  | StBusy, MsgRejectTx _ -> Ok StIdle
  | _ ->
    let mn = match msg with
      | MsgSubmitTx _ -> "MsgSubmitTx" | MsgAcceptTx -> "MsgAcceptTx"
      | MsgRejectTx _ -> "MsgRejectTx" | MsgDone -> "MsgDone"
    in
    Error (Printf.sprintf "local_tx_submission: invalid %s in %s" mn (state_name state))

let encode_message = function
  | MsgSubmitTx tx -> Cbor.Array [Cbor.Uint 0L; Cbor.Bytes tx]
  | MsgAcceptTx    -> Cbor.Array [Cbor.Uint 1L]
  | MsgRejectTx r  -> Cbor.Array [Cbor.Uint 2L; r]
  | MsgDone        -> Cbor.Array [Cbor.Uint 3L]

let decode_message = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bytes tx] -> Ok (MsgSubmitTx tx)
  | Cbor.Array [Cbor.Uint 1L] -> Ok MsgAcceptTx
  | Cbor.Array [Cbor.Uint 2L; r] -> Ok (MsgRejectTx r)
  | Cbor.Array [Cbor.Uint 3L] -> Ok MsgDone
  | _ -> Error "local_tx_submission: unrecognized message"

let to_bytes msg = Cbor.encode (encode_message msg)

let of_bytes data =
  let* cbor = Cbor.decode data in
  decode_message cbor
