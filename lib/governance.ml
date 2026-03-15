(* Conway governance state tracking per CIP-1694.

   Tracks DRep registry, constitutional committee, proposals, votes,
   and treasury. Implements governance transitions for Conway-era
   blocks. *)

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type drep_credential = Key_drep of bytes | Script_drep of bytes

type drep_state = {
  ds_deposit : int64;
  ds_anchor : (string * bytes) option;  (* url, hash *)
  ds_expiry_epoch : int64;
}

type _committee_member = {
  cm_credential : drep_credential;
  cm_expiry_epoch : int64;
}

type governance_action_id = {
  ga_tx_hash : bytes;
  ga_index : int;
}

type vote = Vote_yes | Vote_no | Vote_abstain

type voter =
  | Voter_cc of drep_credential
  | Voter_drep of drep_credential
  | Voter_spo of bytes  (* pool_id *)

type governance_action =
  | GA_ParameterChange
  | GA_HardForkInitiation
  | GA_TreasuryWithdrawals
  | GA_NoConfidence
  | GA_UpdateCommittee
  | GA_NewConstitution
  | GA_InfoAction

type proposal = {
  pr_action : governance_action;
  pr_deposit : int64;
  pr_return_addr : bytes;
  pr_epoch_proposed : int64;
}

type t = {
  drep_registry : (string, drep_state) Hashtbl.t;
  mutable committee_quorum : int64 * int64;  (* numerator, denominator *)
  proposals : (string, proposal) Hashtbl.t;
  votes : (string, (voter * vote) list) Hashtbl.t;
  mutable treasury : int64;
  mutable dreps_registered : int;
  mutable proposals_active : int;
  mutable votes_cast : int;
}

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let ga_id_key ga =
  Printf.sprintf "%s#%d" (hex_of_bytes ga.ga_tx_hash) ga.ga_index

let cred_key = function
  | Key_drep h -> "k:" ^ hex_of_bytes h
  | Script_drep h -> "s:" ^ hex_of_bytes h

(* ================================================================ *)
(* Public API                                                        *)
(* ================================================================ *)

let create () = {
  drep_registry = Hashtbl.create 64;
  committee_quorum = (2L, 3L);
  proposals = Hashtbl.create 16;
  votes = Hashtbl.create 64;
  treasury = 0L;
  dreps_registered = 0;
  proposals_active = 0;
  votes_cast = 0;
}

let drep_count t = t.dreps_registered
let proposal_count t = t.proposals_active
let vote_count t = t.votes_cast
let treasury t = t.treasury

let register_drep t ~credential ~deposit ~anchor ~expiry_epoch =
  let key = cred_key credential in
  if not (Hashtbl.mem t.drep_registry key) then
    t.dreps_registered <- t.dreps_registered + 1;
  Hashtbl.replace t.drep_registry key
    { ds_deposit = deposit; ds_anchor = anchor; ds_expiry_epoch = expiry_epoch }

let deregister_drep t ~credential =
  let key = cred_key credential in
  let refund = match Hashtbl.find_opt t.drep_registry key with
    | Some ds -> ds.ds_deposit | None -> 0L in
  Hashtbl.remove t.drep_registry key;
  if refund > 0L then t.dreps_registered <- max 0 (t.dreps_registered - 1);
  refund

let update_drep t ~credential ~anchor =
  let key = cred_key credential in
  match Hashtbl.find_opt t.drep_registry key with
  | Some ds -> Hashtbl.replace t.drep_registry key { ds with ds_anchor = anchor }
  | None -> ()

let submit_proposal t ~action_id ~action ~deposit ~return_addr ~epoch =
  let key = ga_id_key action_id in
  Hashtbl.replace t.proposals key
    { pr_action = action; pr_deposit = deposit;
      pr_return_addr = return_addr; pr_epoch_proposed = epoch };
  t.proposals_active <- t.proposals_active + 1

let cast_vote t ~voter ~action_id ~vote =
  let key = ga_id_key action_id in
  let existing = match Hashtbl.find_opt t.votes key with
    | Some vs -> vs | None -> [] in
  Hashtbl.replace t.votes key ((voter, vote) :: existing);
  t.votes_cast <- t.votes_cast + 1

let add_treasury t ~amount =
  t.treasury <- Int64.add t.treasury amount

let expire_proposals t ~current_epoch ~max_lifetime =
  let to_remove = ref [] in
  Hashtbl.iter (fun key pr ->
    if Int64.sub current_epoch pr.pr_epoch_proposed > Int64.of_int max_lifetime then
      to_remove := key :: !to_remove
  ) t.proposals;
  List.iter (fun key ->
    Hashtbl.remove t.proposals key;
    Hashtbl.remove t.votes key;
    t.proposals_active <- max 0 (t.proposals_active - 1)
  ) !to_remove;
  List.length !to_remove

let expire_dreps t ~current_epoch =
  let to_expire = ref [] in
  Hashtbl.iter (fun key ds ->
    if Int64.compare current_epoch ds.ds_expiry_epoch > 0 then
      to_expire := key :: !to_expire
  ) t.drep_registry;
  List.iter (fun key ->
    Hashtbl.remove t.drep_registry key;
    t.dreps_registered <- max 0 (t.dreps_registered - 1)
  ) !to_expire;
  List.length !to_expire

let is_drep_registered t ~credential =
  Hashtbl.mem t.drep_registry (cred_key credential)

let get_drep t ~credential =
  Hashtbl.find_opt t.drep_registry (cred_key credential)
