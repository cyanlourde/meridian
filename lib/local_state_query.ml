(* Local state query mini-protocol (mini-protocol ID 6).

   Reference: Ouroboros network specification, "Local State Query"

   Allows clients to query the ledger state at a specific point.
   The client first acquires a point, then issues queries against it.

   State machine:
     StIdle      — client has agency (Acquire, Done)
     StAcquiring — server has agency (Acquired, Failure)
     StAcquired  — client has agency (Query, ReAcquire, Release)
     StQuerying  — server has agency (Result)
     StDone      — nobody has agency

   CBOR wire format:
     MsgAcquire    = [0, point / null]
     MsgAcquired   = [1]
     MsgFailure    = [2, failure_reason]
     MsgQuery      = [3, query]
     MsgResult     = [4, result]
     MsgRelease    = [5]
     MsgReAcquire  = [6, point / null]
     MsgDone       = [7] *)

let ( let* ) = Result.bind

type point = Chain_sync.point

type acquire_failure =
  | AcquireFailurePointTooOld
  | AcquireFailurePointNotOnChain

type local_state_query_state =
  | StIdle
  | StAcquiring
  | StAcquired
  | StQuerying
  | StDone

type local_state_query_message =
  | MsgAcquire of point option       (* None = current tip *)
  | MsgAcquired
  | MsgFailure of acquire_failure
  | MsgQuery of Cbor.cbor_value      (* query is era-specific, kept opaque *)
  | MsgResult of Cbor.cbor_value     (* result is era-specific, kept opaque *)
  | MsgRelease
  | MsgReAcquire of point option
  | MsgDone

let agency_of = function
  | StIdle      -> Miniprotocol.Client_agency
  | StAcquiring -> Miniprotocol.Server_agency
  | StAcquired  -> Miniprotocol.Client_agency
  | StQuerying  -> Miniprotocol.Server_agency
  | StDone      -> Miniprotocol.Nobody_agency

let state_name = function
  | StIdle -> "StIdle" | StAcquiring -> "StAcquiring"
  | StAcquired -> "StAcquired" | StQuerying -> "StQuerying"
  | StDone -> "StDone"

let transition state msg =
  match state, msg with
  | StIdle, MsgAcquire _         -> Ok StAcquiring
  | StIdle, MsgDone              -> Ok StDone
  | StAcquiring, MsgAcquired     -> Ok StAcquired
  | StAcquiring, MsgFailure _    -> Ok StIdle
  | StAcquired, MsgQuery _       -> Ok StQuerying
  | StAcquired, MsgReAcquire _   -> Ok StAcquiring
  | StAcquired, MsgRelease       -> Ok StIdle
  | StQuerying, MsgResult _      -> Ok StAcquired
  | _ ->
    let mn = match msg with
      | MsgAcquire _ -> "MsgAcquire" | MsgAcquired -> "MsgAcquired"
      | MsgFailure _ -> "MsgFailure" | MsgQuery _ -> "MsgQuery"
      | MsgResult _ -> "MsgResult" | MsgRelease -> "MsgRelease"
      | MsgReAcquire _ -> "MsgReAcquire" | MsgDone -> "MsgDone"
    in
    Error (Printf.sprintf "local_state_query: invalid %s in %s" mn (state_name state))

(* ---- CBOR ---- *)

let encode_opt_point = function
  | None -> Cbor.Null
  | Some p -> Chain_sync.encode_point p

let decode_opt_point = function
  | Cbor.Null -> Ok None
  | cbor -> let* p = Chain_sync.decode_point cbor in Ok (Some p)

let encode_failure = function
  | AcquireFailurePointTooOld -> Cbor.Uint 0L
  | AcquireFailurePointNotOnChain -> Cbor.Uint 1L

let decode_failure = function
  | Cbor.Uint 0L -> Ok AcquireFailurePointTooOld
  | Cbor.Uint 1L -> Ok AcquireFailurePointNotOnChain
  | _ -> Error "acquire_failure: expected 0 or 1"

let encode_message = function
  | MsgAcquire pt    -> Cbor.Array [Cbor.Uint 0L; encode_opt_point pt]
  | MsgAcquired      -> Cbor.Array [Cbor.Uint 1L]
  | MsgFailure f     -> Cbor.Array [Cbor.Uint 2L; encode_failure f]
  | MsgQuery q       -> Cbor.Array [Cbor.Uint 3L; q]
  | MsgResult r      -> Cbor.Array [Cbor.Uint 4L; r]
  | MsgRelease       -> Cbor.Array [Cbor.Uint 5L]
  | MsgReAcquire pt  -> Cbor.Array [Cbor.Uint 6L; encode_opt_point pt]
  | MsgDone          -> Cbor.Array [Cbor.Uint 7L]

let decode_message = function
  | Cbor.Array [Cbor.Uint 0L; pt_cbor] ->
    let* p = decode_opt_point pt_cbor in Ok (MsgAcquire p)
  | Cbor.Array [Cbor.Uint 1L] -> Ok MsgAcquired
  | Cbor.Array [Cbor.Uint 2L; f_cbor] ->
    let* f = decode_failure f_cbor in Ok (MsgFailure f)
  | Cbor.Array [Cbor.Uint 3L; q] -> Ok (MsgQuery q)
  | Cbor.Array [Cbor.Uint 4L; r] -> Ok (MsgResult r)
  | Cbor.Array [Cbor.Uint 5L] -> Ok MsgRelease
  | Cbor.Array [Cbor.Uint 6L; pt_cbor] ->
    let* p = decode_opt_point pt_cbor in Ok (MsgReAcquire p)
  | Cbor.Array [Cbor.Uint 7L] -> Ok MsgDone
  | _ -> Error "local_state_query: unrecognized message"

let to_bytes msg = Cbor.encode (encode_message msg)

let of_bytes data =
  let* cbor = Cbor.decode data in
  decode_message cbor
