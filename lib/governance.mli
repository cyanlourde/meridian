(** Conway governance state tracking per CIP-1694. *)

type drep_credential = Key_drep of bytes | Script_drep of bytes

type drep_state = {
  ds_deposit : int64;
  ds_anchor : (string * bytes) option;
  ds_expiry_epoch : int64;
}

type governance_action_id = { ga_tx_hash : bytes; ga_index : int }

type vote = Vote_yes | Vote_no | Vote_abstain

type voter =
  | Voter_cc of drep_credential
  | Voter_drep of drep_credential
  | Voter_spo of bytes

type governance_action =
  | GA_ParameterChange | GA_HardForkInitiation | GA_TreasuryWithdrawals
  | GA_NoConfidence | GA_UpdateCommittee | GA_NewConstitution | GA_InfoAction

type t

val create : unit -> t
val drep_count : t -> int
val proposal_count : t -> int
val vote_count : t -> int
val treasury : t -> int64

val register_drep :
  t -> credential:drep_credential -> deposit:int64 ->
  anchor:(string * bytes) option -> expiry_epoch:int64 -> unit

val deregister_drep : t -> credential:drep_credential -> int64
val update_drep : t -> credential:drep_credential -> anchor:(string * bytes) option -> unit

val submit_proposal :
  t -> action_id:governance_action_id -> action:governance_action ->
  deposit:int64 -> return_addr:bytes -> epoch:int64 -> unit

val cast_vote : t -> voter:voter -> action_id:governance_action_id -> vote:vote -> unit

val add_treasury : t -> amount:int64 -> unit

val expire_proposals : t -> current_epoch:int64 -> max_lifetime:int -> int
val expire_dreps : t -> current_epoch:int64 -> int

val is_drep_registered : t -> credential:drep_credential -> bool
val get_drep : t -> credential:drep_credential -> drep_state option
