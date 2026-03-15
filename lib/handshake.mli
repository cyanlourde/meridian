(** Ouroboros handshake mini-protocol.

    The handshake is a one-shot protocol (mini-protocol ID 0):
    1. Client sends MsgProposeVersions
    2. Server responds with MsgAcceptVersion or MsgRefuse
    3. Connection is established or torn down

    Reference: Ouroboros network specification *)

(** {1 Network magic constants} *)

val mainnet_magic : int64
val preprod_magic : int64
val preview_magic : int64

(** {1 Types} *)

type version_number = int64

(** Version parameters exchanged during handshake. *)
type version_params = {
  network_magic : int64;
  initiator_only_diffusion_mode : bool;
  peer_sharing : int;
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

(** {1 Helpers} *)

val default_params : network_magic:int64 -> version_params
val refuse_reason_to_string : refuse_reason -> string

(** {1 CBOR encoding/decoding} *)

val encode_version_params : version_number -> version_params -> Cbor.cbor_value
val decode_version_params : Cbor.cbor_value -> (version_params, string) result

val encode_refuse_reason : refuse_reason -> Cbor.cbor_value
val decode_refuse_reason : Cbor.cbor_value -> (refuse_reason, string) result

val encode_handshake_message : handshake_message -> Cbor.cbor_value
val decode_handshake_message : Cbor.cbor_value -> (handshake_message, string) result

(** {1 Byte serialization} *)

val to_bytes : handshake_message -> bytes
val of_bytes : bytes -> (handshake_message, string) result

(** {1 Client logic} *)

val propose_versions :
  (version_number * version_params) list -> handshake_message
(** Build a ProposeVersions message. *)

val handle_response :
  supported:(version_number * version_params) list ->
  handshake_message -> (version_number * version_params, string) result
(** Validate a server's response against supported versions. *)

(** {1 Server logic} *)

val negotiate :
  supported:(version_number * version_params) list ->
  handshake_message -> handshake_message
(** Negotiate against client proposals. Returns AcceptVersion or Refuse. *)
