(* Ouroboros block-fetch mini-protocol (mini-protocol ID 3).

   Reference: Ouroboros network specification, "Block-Fetch Mini-Protocol"

   A protocol for downloading ranges of blocks. The client requests a
   range (from, to) of chain points; the server streams the blocks back.

   State machine:
     StIdle      — client has agency (RequestRange or ClientDone)
     StBusy      — server has agency (StartBatch or NoBlocks)
     StStreaming  — server has agency (Block or BatchDone)
     StDone      — nobody has agency (terminal)

   CBOR wire format:
     MsgRequestRange = [0, point_from, point_to]
     MsgClientDone   = [1]
     MsgStartBatch   = [2]
     MsgNoBlocks     = [3]
     MsgBlock        = [4, block_bytes]
     MsgBatchDone    = [5] *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

(** Reuse the point type from chain-sync. *)
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

(* ================================================================ *)
(* State machine                                                     *)
(* ================================================================ *)

let agency_of = function
  | StIdle      -> Miniprotocol.Client_agency
  | StBusy      -> Miniprotocol.Server_agency
  | StStreaming  -> Miniprotocol.Server_agency
  | StDone      -> Miniprotocol.Nobody_agency

let state_name = function
  | StIdle -> "StIdle" | StBusy -> "StBusy"
  | StStreaming -> "StStreaming" | StDone -> "StDone"

let transition state msg =
  match state, msg with
  | StIdle, MsgRequestRange _  -> Ok StBusy
  | StIdle, MsgClientDone      -> Ok StDone
  | StBusy, MsgStartBatch      -> Ok StStreaming
  | StBusy, MsgNoBlocks        -> Ok StIdle
  | StStreaming, MsgBlock _    -> Ok StStreaming
  | StStreaming, MsgBatchDone  -> Ok StIdle
  | _ ->
    let msg_name = match msg with
      | MsgRequestRange _ -> "MsgRequestRange"
      | MsgClientDone -> "MsgClientDone"
      | MsgStartBatch -> "MsgStartBatch"
      | MsgNoBlocks -> "MsgNoBlocks"
      | MsgBlock _ -> "MsgBlock"
      | MsgBatchDone -> "MsgBatchDone"
    in
    Error (Printf.sprintf "block_fetch: invalid transition %s in %s"
             msg_name (state_name state))

(* ================================================================ *)
(* CBOR encoding/decoding                                            *)
(* ================================================================ *)

let encode_message = function
  | MsgRequestRange (from_pt, to_pt) ->
    Cbor.Array [Cbor.Uint 0L;
                Chain_sync.encode_point from_pt;
                Chain_sync.encode_point to_pt]
  | MsgClientDone ->
    Cbor.Array [Cbor.Uint 1L]
  | MsgStartBatch ->
    Cbor.Array [Cbor.Uint 2L]
  | MsgNoBlocks ->
    Cbor.Array [Cbor.Uint 3L]
  | MsgBlock block_bytes ->
    Cbor.Array [Cbor.Uint 4L; Cbor.Bytes block_bytes]
  | MsgBatchDone ->
    Cbor.Array [Cbor.Uint 5L]

let decode_message = function
  | Cbor.Array [Cbor.Uint 0L; from_cbor; to_cbor] ->
    let* from_pt = Chain_sync.decode_point from_cbor in
    let* to_pt = Chain_sync.decode_point to_cbor in
    Ok (MsgRequestRange (from_pt, to_pt))
  | Cbor.Array [Cbor.Uint 1L] -> Ok MsgClientDone
  | Cbor.Array [Cbor.Uint 2L] -> Ok MsgStartBatch
  | Cbor.Array [Cbor.Uint 3L] -> Ok MsgNoBlocks
  | Cbor.Array [Cbor.Uint 4L; Cbor.Bytes b] -> Ok (MsgBlock b)
  | Cbor.Array [Cbor.Uint 4L; Cbor.Tag (24L, Cbor.Bytes b)] -> Ok (MsgBlock b)
  | Cbor.Array [Cbor.Uint 5L] -> Ok MsgBatchDone
  | _ -> Error "block_fetch: unrecognized message"

let to_bytes msg = Cbor.encode (encode_message msg)

let of_bytes data =
  let* cbor = Cbor.decode data in
  decode_message cbor
