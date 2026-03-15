(* Ouroboros keep-alive mini-protocol (mini-protocol ID 8).

   Reference: Ouroboros network specification, "KeepAlive Mini-Protocol"

   A simple ping/pong protocol to detect dead connections. The client
   periodically sends a cookie; the server echoes it back.

   State machine:
     StClient — client has agency (KeepAlive or Done)
     StServer — server has agency (KeepAliveResponse)
     StDone   — nobody has agency (terminal)

   CBOR wire format:
     MsgKeepAlive         = [0, cookie]
     MsgKeepAliveResponse = [1, cookie]
     MsgDone              = [2] *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type keep_alive_state =
  | StClient
  | StServer
  | StDone

type keep_alive_message =
  | MsgKeepAlive of int          (** cookie: 16-bit unsigned *)
  | MsgKeepAliveResponse of int  (** echoed cookie *)
  | MsgDone

(* ================================================================ *)
(* State machine                                                     *)
(* ================================================================ *)

let agency_of = function
  | StClient -> Miniprotocol.Client_agency
  | StServer -> Miniprotocol.Server_agency
  | StDone   -> Miniprotocol.Nobody_agency

let state_name = function
  | StClient -> "StClient" | StServer -> "StServer" | StDone -> "StDone"

let transition state msg =
  match state, msg with
  | StClient, MsgKeepAlive _          -> Ok StServer
  | StClient, MsgDone                 -> Ok StDone
  | StServer, MsgKeepAliveResponse _  -> Ok StClient
  | _ ->
    let msg_name = match msg with
      | MsgKeepAlive _ -> "MsgKeepAlive"
      | MsgKeepAliveResponse _ -> "MsgKeepAliveResponse"
      | MsgDone -> "MsgDone"
    in
    Error (Printf.sprintf "keep_alive: invalid transition %s in %s"
             msg_name (state_name state))

(* ================================================================ *)
(* CBOR encoding/decoding                                            *)
(* ================================================================ *)

let encode_message = function
  | MsgKeepAlive cookie ->
    Cbor.Array [Cbor.Uint 0L; Cbor.Uint (Int64.of_int cookie)]
  | MsgKeepAliveResponse cookie ->
    Cbor.Array [Cbor.Uint 1L; Cbor.Uint (Int64.of_int cookie)]
  | MsgDone ->
    Cbor.Array [Cbor.Uint 2L]

let decode_message = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Uint cookie] ->
    Ok (MsgKeepAlive (Int64.to_int cookie))
  | Cbor.Array [Cbor.Uint 1L; Cbor.Uint cookie] ->
    Ok (MsgKeepAliveResponse (Int64.to_int cookie))
  | Cbor.Array [Cbor.Uint 2L] ->
    Ok MsgDone
  | _ -> Error "keep_alive: unrecognized message"

let to_bytes msg = Cbor.encode (encode_message msg)

let of_bytes data =
  let* cbor = Cbor.decode data in
  decode_message cbor
