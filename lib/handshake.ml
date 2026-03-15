(* Ouroboros handshake mini-protocol.

   Reference: Ouroboros network specification, "Handshake Mini-Protocol"

   The handshake is a one-shot protocol on mini-protocol ID 0:
   1. Client sends MsgProposeVersions: a map of version numbers to params
   2. Server responds with MsgAcceptVersion or MsgRefuse
   3. Connection is then established (data-bearing mini-protocols start)
      or torn down.

   CBOR wire format:
     MsgProposeVersions = [0, { versionNumber => versionParams, ... }]
     MsgAcceptVersion   = [1, versionNumber, versionParams]
     MsgRefuse          = [2, refuseReason]

   refuseReason:
     VersionMismatch      = [0, [versionNumber, ...]]
     HandshakeDecodeError = [1, versionNumber, text]
     Refused              = [2, versionNumber, text] *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Network magic constants                                           *)
(* ================================================================ *)

let mainnet_magic = 764824073L
let preprod_magic = 1L
let preview_magic = 2L

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type version_number = int64

(** Version parameters exchanged during handshake.
    The wire encoding varies by version number:
    - Versions <= 6: just network_magic (uint)
    - Versions 7-10: [network_magic, initiator_only_diffusion_mode]
    - Versions 11-12: [network_magic, initiator_only_diffusion_mode, peer_sharing]
    - Versions 13+: [network_magic, initiator_only_diffusion_mode, peer_sharing, query] *)
type version_params = {
  network_magic : int64;
  initiator_only_diffusion_mode : bool;
  peer_sharing : int;  (** 0 = NoPeerSharing, 1 = PeerSharingPrivate, 2 = PeerSharingPublic *)
  query : bool;
}

type refuse_reason =
  | VersionMismatch of version_number list
  | HandshakeDecodeError of version_number * string
  | Refused of version_number * string

type handshake_message =
  | ProposeVersions of (version_number * version_params) list
  | AcceptVersion of version_number * version_params
  | Refuse of refuse_reason

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let default_params ~network_magic = {
  network_magic;
  initiator_only_diffusion_mode = false;
  peer_sharing = 0;
  query = false;
}

let refuse_reason_to_string = function
  | VersionMismatch vs ->
    Printf.sprintf "version mismatch: [%s]"
      (String.concat ", " (List.map Int64.to_string vs))
  | HandshakeDecodeError (v, msg) ->
    Printf.sprintf "decode error (version %Ld): %s" v msg
  | Refused (v, msg) ->
    Printf.sprintf "refused (version %Ld): %s" v msg

(* ================================================================ *)
(* Version params CBOR encoding                                      *)
(* ================================================================ *)

(** Encode version params in the format expected for the given version number. *)
let encode_version_params version params =
  if version <= 6L then
    Cbor.Uint params.network_magic
  else if version <= 10L then
    Cbor.Array [
      Cbor.Uint params.network_magic;
      Cbor.Bool params.initiator_only_diffusion_mode;
    ]
  else if version <= 12L then
    Cbor.Array [
      Cbor.Uint params.network_magic;
      Cbor.Bool params.initiator_only_diffusion_mode;
      Cbor.Uint (Int64.of_int params.peer_sharing);
    ]
  else
    Cbor.Array [
      Cbor.Uint params.network_magic;
      Cbor.Bool params.initiator_only_diffusion_mode;
      Cbor.Uint (Int64.of_int params.peer_sharing);
      Cbor.Bool params.query;
    ]

(** Decode version params from CBOR. The format is inferred from structure. *)
let decode_version_params cbor =
  let base = { network_magic = 0L; initiator_only_diffusion_mode = false;
               peer_sharing = 0; query = false } in
  match cbor with
  | Cbor.Uint magic ->
    Ok { base with network_magic = magic }
  | Cbor.Array (Cbor.Uint magic :: rest) ->
    let initiator_only = match rest with
      | Cbor.Bool b :: _ -> b | _ -> false
    in
    let peer_sharing = match rest with
      | _ :: Cbor.Uint n :: _ -> Int64.to_int n | _ -> 0
    in
    let query = match rest with
      | _ :: _ :: Cbor.Bool b :: _ -> b | _ -> false
    in
    Ok { network_magic = magic;
         initiator_only_diffusion_mode = initiator_only;
         peer_sharing; query }
  | _ -> Error "version_params: expected uint or array"

(* ================================================================ *)
(* Refuse reason CBOR encoding                                       *)
(* ================================================================ *)

let encode_refuse_reason = function
  | VersionMismatch versions ->
    Cbor.Array [
      Cbor.Uint 0L;
      Cbor.Array (List.map (fun v -> Cbor.Uint v) versions);
    ]
  | HandshakeDecodeError (version, msg) ->
    Cbor.Array [Cbor.Uint 1L; Cbor.Uint version; Cbor.Text msg]
  | Refused (version, msg) ->
    Cbor.Array [Cbor.Uint 2L; Cbor.Uint version; Cbor.Text msg]

let decode_refuse_reason = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Array versions] ->
    let* vs = List.fold_left (fun acc v ->
      let* acc = acc in
      match v with
      | Cbor.Uint n -> Ok (n :: acc)
      | _ -> Error "refuse_reason: version must be uint"
    ) (Ok []) versions in
    Ok (VersionMismatch (List.rev vs))
  | Cbor.Array [Cbor.Uint 1L; Cbor.Uint version; Cbor.Text msg] ->
    Ok (HandshakeDecodeError (version, msg))
  | Cbor.Array [Cbor.Uint 2L; Cbor.Uint version; Cbor.Text msg] ->
    Ok (Refused (version, msg))
  | _ -> Error "refuse_reason: unrecognized structure"

(* ================================================================ *)
(* Handshake message CBOR encoding                                   *)
(* ================================================================ *)

let encode_handshake_message = function
  | ProposeVersions versions ->
    let entries = List.map (fun (v, p) ->
      (Cbor.Uint v, encode_version_params v p)
    ) versions in
    Cbor.Array [Cbor.Uint 0L; Cbor.Map entries]
  | AcceptVersion (version, params) ->
    Cbor.Array [Cbor.Uint 1L; Cbor.Uint version;
                encode_version_params version params]
  | Refuse reason ->
    Cbor.Array [Cbor.Uint 2L; encode_refuse_reason reason]

let decode_handshake_message cbor =
  match cbor with
  | Cbor.Array [Cbor.Uint 0L; Cbor.Map entries] ->
    let* versions = List.fold_left (fun acc (k, v) ->
      let* acc = acc in
      match k with
      | Cbor.Uint version ->
        let* params = decode_version_params v in
        Ok ((version, params) :: acc)
      | _ -> Error "ProposeVersions: version key must be uint"
    ) (Ok []) entries in
    Ok (ProposeVersions (List.rev versions))
  | Cbor.Array [Cbor.Uint 1L; Cbor.Uint version; params_cbor] ->
    let* params = decode_version_params params_cbor in
    Ok (AcceptVersion (version, params))
  | Cbor.Array [Cbor.Uint 2L; reason_cbor] ->
    let* reason = decode_refuse_reason reason_cbor in
    Ok (Refuse reason)
  | _ -> Error "handshake_message: expected [tag, ...] with tag 0, 1, or 2"

(* ================================================================ *)
(* Serialization to/from bytes (via CBOR)                            *)
(* ================================================================ *)

let to_bytes msg =
  Cbor.encode (encode_handshake_message msg)

let of_bytes data =
  let* cbor = Cbor.decode data in
  decode_handshake_message cbor

(* ================================================================ *)
(* Client logic                                                      *)
(* ================================================================ *)

(** Build a ProposeVersions message from a list of supported versions. *)
let propose_versions versions =
  ProposeVersions versions

(** Validate a server's response against our supported versions.
    Returns the negotiated version and params on success. *)
let handle_response ~supported msg =
  match msg with
  | AcceptVersion (v, params) ->
    if List.exists (fun (sv, _) -> Int64.equal sv v) supported then
      Ok (v, params)
    else
      Error (Printf.sprintf "server accepted version %Ld which we did not propose" v)
  | Refuse reason ->
    Error (refuse_reason_to_string reason)
  | ProposeVersions _ ->
    Error "client received ProposeVersions (expected AcceptVersion or Refuse)"

(* ================================================================ *)
(* Server logic                                                      *)
(* ================================================================ *)

(** Negotiate a version from the client's proposals against our supported
    versions. Selects the highest mutually supported version.
    Returns AcceptVersion with the server's params, or Refuse. *)
let negotiate ~supported msg =
  match msg with
  | ProposeVersions client_versions ->
    let client_vs = List.map fst client_versions in
    (* Find versions we both support, using server's params *)
    let mutual = List.filter (fun (sv, _) ->
      List.exists (fun cv -> Int64.equal sv cv) client_vs
    ) supported in
    (* Sort descending by version number to pick highest *)
    let sorted = List.sort (fun (a, _) (b, _) ->
      Int64.compare b a
    ) mutual in
    (match sorted with
     | (v, p) :: _ -> AcceptVersion (v, p)
     | [] -> Refuse (VersionMismatch client_vs))
  | AcceptVersion _ | Refuse _ ->
    Refuse (Refused (0L, "server received non-ProposeVersions message"))
