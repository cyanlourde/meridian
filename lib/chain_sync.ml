(* Ouroboros chain-sync mini-protocol (mini-protocol ID 2).

   Reference: Ouroboros network specification, "Chain-Sync Mini-Protocol"

   A pull-based protocol for synchronizing the chain. The client drives
   the interaction from StIdle; the server responds with new headers/blocks
   or rollback instructions.

   State machine:
     StIdle      — client has agency (can send RequestNext, FindIntersect, Done)
     StNext      — server has agency (responds with RollForward, RollBackward, AwaitReply)
     StIntersect — server has agency (responds with IntersectFound, IntersectNotFound)
     StDone      — nobody has agency (terminal)

   CBOR wire format — each message is [tag, ...args]:
     MsgRequestNext      = [0]
     MsgAwaitReply        = [1]
     MsgRollForward       = [2, header_or_block, tip]
     MsgRollBackward      = [3, point, tip]
     MsgFindIntersect     = [4, [point, ...]]
     MsgIntersectFound    = [5, point, tip]
     MsgIntersectNotFound = [6, tip]
     MsgDone              = [7] *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

(** A point on the chain: either the origin (genesis) or a specific
    slot + block hash pair. *)
type point =
  | Origin
  | Point of Cardano_types.slot_number * Cardano_types.block_hash

(** The tip of the chain: a point plus the block number at that point. *)
type tip = {
  tip_point : point;
  tip_block_number : Cardano_types.block_number;
}

(** Protocol states with agency annotations. *)
type chain_sync_state =
  | StIdle      (** Client has agency *)
  | StNext      (** Server has agency *)
  | StIntersect (** Server has agency *)
  | StDone      (** Nobody has agency — terminal *)

(** Chain-sync messages.
    The [header] field in RollForward is left as opaque CBOR because
    its structure depends on whether we are doing header-only or
    block-body chain-sync, and which era the block belongs to. *)
type chain_sync_message =
  | MsgRequestNext
  | MsgAwaitReply
  | MsgRollForward of Cbor.cbor_value * tip
  | MsgRollBackward of point * tip
  | MsgFindIntersect of point list
  | MsgIntersectFound of point * tip
  | MsgIntersectNotFound of tip
  | MsgDone

(* ================================================================ *)
(* State machine                                                     *)
(* ================================================================ *)

let agency_of = function
  | StIdle      -> Miniprotocol.Client_agency
  | StNext      -> Miniprotocol.Server_agency
  | StIntersect -> Miniprotocol.Server_agency
  | StDone      -> Miniprotocol.Nobody_agency

let state_name = function
  | StIdle -> "StIdle" | StNext -> "StNext"
  | StIntersect -> "StIntersect" | StDone -> "StDone"

(** Compute the next state after a message, or [Error] if the
    transition is invalid from the current state. *)
let transition state msg =
  match state, msg with
  (* Client sends from StIdle *)
  | StIdle, MsgRequestNext   -> Ok StNext
  | StIdle, MsgFindIntersect _ -> Ok StIntersect
  | StIdle, MsgDone          -> Ok StDone
  (* Server responds from StNext *)
  | StNext, MsgAwaitReply      -> Ok StNext  (* stay in StNext, client will re-wait *)
  | StNext, MsgRollForward _   -> Ok StIdle
  | StNext, MsgRollBackward _  -> Ok StIdle
  (* Server responds from StIntersect *)
  | StIntersect, MsgIntersectFound _    -> Ok StIdle
  | StIntersect, MsgIntersectNotFound _ -> Ok StIdle
  (* Everything else is invalid *)
  | _ ->
    let msg_name = match msg with
      | MsgRequestNext -> "MsgRequestNext"
      | MsgAwaitReply -> "MsgAwaitReply"
      | MsgRollForward _ -> "MsgRollForward"
      | MsgRollBackward _ -> "MsgRollBackward"
      | MsgFindIntersect _ -> "MsgFindIntersect"
      | MsgIntersectFound _ -> "MsgIntersectFound"
      | MsgIntersectNotFound _ -> "MsgIntersectNotFound"
      | MsgDone -> "MsgDone"
    in
    Error (Printf.sprintf "invalid transition: %s in state %s"
             msg_name (state_name state))

(* ================================================================ *)
(* CBOR encoding helpers                                             *)
(* ================================================================ *)

let encode_point = function
  | Origin ->
    Cbor.Array []
  | Point (slot, hash) ->
    Cbor.Array [Cbor.Uint slot; Cbor.Bytes hash]

let decode_point = function
  | Cbor.Array [] ->
    Ok Origin
  | Cbor.Array [Cbor.Uint slot; Cbor.Bytes hash] when Bytes.length hash = 32 ->
    Ok (Point (slot, hash))
  | _ -> Error "point: expected [] or [uint, hash32]"

let encode_tip t =
  Cbor.Array [encode_point t.tip_point; Cbor.Uint t.tip_block_number]

let decode_tip = function
  | Cbor.Array [point_cbor; Cbor.Uint block_no] ->
    let* p = decode_point point_cbor in
    Ok { tip_point = p; tip_block_number = block_no }
  | _ -> Error "tip: expected [point, uint]"

(* ================================================================ *)
(* Message CBOR encoding/decoding                                    *)
(* ================================================================ *)

let encode_message = function
  | MsgRequestNext ->
    Cbor.Array [Cbor.Uint 0L]
  | MsgAwaitReply ->
    Cbor.Array [Cbor.Uint 1L]
  | MsgRollForward (header, t) ->
    Cbor.Array [Cbor.Uint 2L; header; encode_tip t]
  | MsgRollBackward (point, t) ->
    Cbor.Array [Cbor.Uint 3L; encode_point point; encode_tip t]
  | MsgFindIntersect points ->
    Cbor.Array [Cbor.Uint 4L; Cbor.Array (List.map encode_point points)]
  | MsgIntersectFound (point, t) ->
    Cbor.Array [Cbor.Uint 5L; encode_point point; encode_tip t]
  | MsgIntersectNotFound t ->
    Cbor.Array [Cbor.Uint 6L; encode_tip t]
  | MsgDone ->
    Cbor.Array [Cbor.Uint 7L]

let decode_message = function
  | Cbor.Array [Cbor.Uint 0L] ->
    Ok MsgRequestNext
  | Cbor.Array [Cbor.Uint 1L] ->
    Ok MsgAwaitReply
  | Cbor.Array [Cbor.Uint 2L; header; tip_cbor] ->
    let* t = decode_tip tip_cbor in
    Ok (MsgRollForward (header, t))
  | Cbor.Array [Cbor.Uint 3L; point_cbor; tip_cbor] ->
    let* p = decode_point point_cbor in
    let* t = decode_tip tip_cbor in
    Ok (MsgRollBackward (p, t))
  | Cbor.Array [Cbor.Uint 4L; Cbor.Array points_cbor] ->
    let* points =
      List.fold_left (fun acc pc ->
        let* acc = acc in
        let* p = decode_point pc in
        Ok (p :: acc)
      ) (Ok []) points_cbor
    in
    Ok (MsgFindIntersect (List.rev points))
  | Cbor.Array [Cbor.Uint 5L; point_cbor; tip_cbor] ->
    let* p = decode_point point_cbor in
    let* t = decode_tip tip_cbor in
    Ok (MsgIntersectFound (p, t))
  | Cbor.Array [Cbor.Uint 6L; tip_cbor] ->
    let* t = decode_tip tip_cbor in
    Ok (MsgIntersectNotFound t)
  | Cbor.Array [Cbor.Uint 7L] ->
    Ok MsgDone
  | _ -> Error "chain_sync: unrecognized message"

(* ================================================================ *)
(* Byte serialization via CBOR                                       *)
(* ================================================================ *)

let to_bytes msg =
  Cbor.encode (encode_message msg)

let of_bytes data =
  let* cbor = Cbor.decode data in
  decode_message cbor
