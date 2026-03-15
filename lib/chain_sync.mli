(** Ouroboros chain-sync mini-protocol (mini-protocol ID 2).

    A pull-based protocol for chain synchronization.
    The client requests the next update; the server responds with
    roll-forward (new header/block) or roll-backward instructions.

    Reference: Ouroboros network specification *)

(** {1 Types} *)

(** A point on the chain. *)
type point =
  | Origin
  | Point of Cardano_types.slot_number * Cardano_types.block_hash

(** Tip of the chain: a point plus the block number at that tip. *)
type tip = {
  tip_point : point;
  tip_block_number : Cardano_types.block_number;
}

(** Protocol states. *)
type chain_sync_state =
  | StIdle      (** Client has agency *)
  | StNext      (** Server has agency *)
  | StIntersect (** Server has agency *)
  | StDone      (** Terminal — nobody has agency *)

(** Chain-sync messages. The header/block in [MsgRollForward] is
    left as opaque [Cbor.cbor_value] because its structure is
    era-dependent. *)
type chain_sync_message =
  | MsgRequestNext
  | MsgAwaitReply
  | MsgRollForward of Cbor.cbor_value * tip
  | MsgRollBackward of point * tip
  | MsgFindIntersect of point list
  | MsgIntersectFound of point * tip
  | MsgIntersectNotFound of tip
  | MsgDone

(** {1 State machine} *)

val agency_of : chain_sync_state -> Miniprotocol.agency
val state_name : chain_sync_state -> string
val transition : chain_sync_state -> chain_sync_message -> (chain_sync_state, string) result

(** {1 CBOR encoding/decoding} *)

val encode_point : point -> Cbor.cbor_value
val decode_point : Cbor.cbor_value -> (point, string) result

val encode_tip : tip -> Cbor.cbor_value
val decode_tip : Cbor.cbor_value -> (tip, string) result

val encode_message : chain_sync_message -> Cbor.cbor_value
val decode_message : Cbor.cbor_value -> (chain_sync_message, string) result

(** {1 Byte serialization} *)

val to_bytes : chain_sync_message -> bytes
val of_bytes : bytes -> (chain_sync_message, string) result
