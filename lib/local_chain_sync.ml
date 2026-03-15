(* Local chain-sync mini-protocol (mini-protocol ID 5).

   Reference: Ouroboros network specification, "Local Chain-Sync"

   Same state machine and message structure as node-to-node chain-sync,
   but carries full blocks instead of headers in MsgRollForward. Used
   for node-to-client (local socket) communication.

   CBOR wire format is identical to chain-sync (tags 0-7). *)

let ( let* ) = Result.bind

(* Reuse types from chain_sync *)
type point = Chain_sync.point
type tip = Chain_sync.tip

type local_chain_sync_state =
  | StIdle
  | StNext
  | StIntersect
  | StDone

type local_chain_sync_message =
  | MsgRequestNext
  | MsgAwaitReply
  | MsgRollForward of bytes * tip   (* full block bytes, not just header *)
  | MsgRollBackward of point * tip
  | MsgFindIntersect of point list
  | MsgIntersectFound of point * tip
  | MsgIntersectNotFound of tip
  | MsgDone

let agency_of = function
  | StIdle      -> Miniprotocol.Client_agency
  | StNext      -> Miniprotocol.Server_agency
  | StIntersect -> Miniprotocol.Server_agency
  | StDone      -> Miniprotocol.Nobody_agency

let state_name = function
  | StIdle -> "StIdle" | StNext -> "StNext"
  | StIntersect -> "StIntersect" | StDone -> "StDone"

let transition state msg =
  match state, msg with
  | StIdle, MsgRequestNext       -> Ok StNext
  | StIdle, MsgFindIntersect _   -> Ok StIntersect
  | StIdle, MsgDone              -> Ok StDone
  | StNext, MsgAwaitReply        -> Ok StNext
  | StNext, MsgRollForward _     -> Ok StIdle
  | StNext, MsgRollBackward _    -> Ok StIdle
  | StIntersect, MsgIntersectFound _    -> Ok StIdle
  | StIntersect, MsgIntersectNotFound _ -> Ok StIdle
  | _ ->
    let mn = match msg with
      | MsgRequestNext -> "MsgRequestNext" | MsgAwaitReply -> "MsgAwaitReply"
      | MsgRollForward _ -> "MsgRollForward" | MsgRollBackward _ -> "MsgRollBackward"
      | MsgFindIntersect _ -> "MsgFindIntersect"
      | MsgIntersectFound _ -> "MsgIntersectFound"
      | MsgIntersectNotFound _ -> "MsgIntersectNotFound" | MsgDone -> "MsgDone"
    in
    Error (Printf.sprintf "local_chain_sync: invalid %s in %s" mn (state_name state))

let encode_message = function
  | MsgRequestNext -> Cbor.Array [Cbor.Uint 0L]
  | MsgAwaitReply -> Cbor.Array [Cbor.Uint 1L]
  | MsgRollForward (block, t) ->
    Cbor.Array [Cbor.Uint 2L; Cbor.Bytes block; Chain_sync.encode_tip t]
  | MsgRollBackward (p, t) ->
    Cbor.Array [Cbor.Uint 3L; Chain_sync.encode_point p; Chain_sync.encode_tip t]
  | MsgFindIntersect points ->
    Cbor.Array [Cbor.Uint 4L; Cbor.Array (List.map Chain_sync.encode_point points)]
  | MsgIntersectFound (p, t) ->
    Cbor.Array [Cbor.Uint 5L; Chain_sync.encode_point p; Chain_sync.encode_tip t]
  | MsgIntersectNotFound t ->
    Cbor.Array [Cbor.Uint 6L; Chain_sync.encode_tip t]
  | MsgDone -> Cbor.Array [Cbor.Uint 7L]

let decode_message = function
  | Cbor.Array [Cbor.Uint 0L] -> Ok MsgRequestNext
  | Cbor.Array [Cbor.Uint 1L] -> Ok MsgAwaitReply
  | Cbor.Array [Cbor.Uint 2L; Cbor.Bytes block; tip_cbor] ->
    let* t = Chain_sync.decode_tip tip_cbor in
    Ok (MsgRollForward (block, t))
  | Cbor.Array [Cbor.Uint 3L; pt_cbor; tip_cbor] ->
    let* p = Chain_sync.decode_point pt_cbor in
    let* t = Chain_sync.decode_tip tip_cbor in
    Ok (MsgRollBackward (p, t))
  | Cbor.Array [Cbor.Uint 4L; Cbor.Array pts] ->
    let* points = List.fold_left (fun acc pc ->
      let* acc = acc in
      let* p = Chain_sync.decode_point pc in Ok (p :: acc)
    ) (Ok []) pts in
    Ok (MsgFindIntersect (List.rev points))
  | Cbor.Array [Cbor.Uint 5L; pt_cbor; tip_cbor] ->
    let* p = Chain_sync.decode_point pt_cbor in
    let* t = Chain_sync.decode_tip tip_cbor in
    Ok (MsgIntersectFound (p, t))
  | Cbor.Array [Cbor.Uint 6L; tip_cbor] ->
    let* t = Chain_sync.decode_tip tip_cbor in
    Ok (MsgIntersectNotFound t)
  | Cbor.Array [Cbor.Uint 7L] -> Ok MsgDone
  | _ -> Error "local_chain_sync: unrecognized message"

let to_bytes msg = Cbor.encode (encode_message msg)

let of_bytes data =
  let* cbor = Cbor.decode data in
  decode_message cbor
