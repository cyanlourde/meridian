(* Transaction deserialization per era.

   Decodes raw CBOR transaction bodies into structured types.
   Shelley+ txs follow: {0: inputs, 1: outputs, 2: fee, ...}
   Byron txs follow: [inputs, outputs, attributes]

   Outputs carry full Multi_asset.value for proper conservation checks. *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type tx_input = {
  ti_tx_hash : bytes;
  ti_index : int64;
}

type tx_output = {
  to_address : bytes;
  to_value : Multi_asset.value;
  to_has_datum : bool;
  to_has_script_ref : bool;
}

type cert_action =
  | Cert_stake_registration
  | Cert_stake_deregistration
  | Cert_pool_registration
  | Cert_pool_retirement
  | Cert_drep_registration of int64    (* deposit amount *)
  | Cert_drep_deregistration of int64  (* refund amount *)
  | Cert_drep_update
  | Cert_vote_delegation
  | Cert_committee_auth
  | Cert_committee_resign
  | Cert_other

type decoded_tx = {
  dt_inputs : tx_input list;
  dt_outputs : tx_output list;
  dt_fee : int64;
  dt_ttl : int64 option;
  dt_validity_start : int64 option;
  dt_certs : cert_action list;
  dt_withdrawal_total : int64;
  dt_mint : Multi_asset.value;
  dt_collateral_inputs : tx_input list;
  dt_collateral_return : tx_output option;
  dt_total_collateral : int64 option;
  dt_is_valid : bool;
  dt_era : Block_decoder.era;
  dt_voting_procedures : int;     (* count of votes in this tx *)
  dt_proposal_count : int;        (* count of governance proposals *)
  dt_treasury_donation : int64;   (* key 22: lovelace donated to treasury *)
}

(* ================================================================ *)
(* CBOR helpers                                                      *)
(* ================================================================ *)

let find_map_uint_key key pairs =
  List.find_map (fun (k, v) ->
    match k with
    | Cbor.Uint n when Int64.equal n (Int64.of_int key) -> Some v
    | _ -> None
  ) pairs

let decode_input = function
  | Cbor.Array [Cbor.Bytes tx_hash; Cbor.Uint idx]
    when Bytes.length tx_hash = 32 ->
    Ok { ti_tx_hash = tx_hash; ti_index = idx }
  | _ -> Error "tx_input: expected [hash32, uint]"

(* ================================================================ *)
(* Output decoders — all use Multi_asset.value                       *)
(* ================================================================ *)

let decode_shelley_output = function
  | Cbor.Array [Cbor.Bytes addr; Cbor.Uint amount] ->
    Ok { to_address = addr; to_value = Multi_asset.of_lovelace amount;
         to_has_datum = false; to_has_script_ref = false }
  | _ -> Error "shelley output: expected [addr, coin]"

let decode_mary_output = function
  | Cbor.Array [Cbor.Bytes addr; val_cbor] ->
    Ok { to_address = addr; to_value = Multi_asset.of_cbor val_cbor;
         to_has_datum = false; to_has_script_ref = false }
  | _ -> Error "mary output: expected [addr, value]"

let decode_alonzo_output = function
  | Cbor.Array [Cbor.Bytes addr; val_cbor] ->
    Ok { to_address = addr; to_value = Multi_asset.of_cbor val_cbor;
         to_has_datum = false; to_has_script_ref = false }
  | Cbor.Array [Cbor.Bytes addr; val_cbor; _datum_hash] ->
    Ok { to_address = addr; to_value = Multi_asset.of_cbor val_cbor;
         to_has_datum = true; to_has_script_ref = false }
  | _ -> Error "alonzo output: unexpected format"

let decode_babbage_output = function
  | Cbor.Map pairs ->
    let addr = match find_map_uint_key 0 pairs with
      | Some (Cbor.Bytes a) -> a | _ -> Bytes.empty in
    let value = match find_map_uint_key 1 pairs with
      | Some v -> Multi_asset.of_cbor v | None -> Multi_asset.zero in
    let has_datum = find_map_uint_key 2 pairs <> None in
    let has_script = find_map_uint_key 3 pairs <> None in
    Ok { to_address = addr; to_value = value;
         to_has_datum = has_datum; to_has_script_ref = has_script }
  | cbor -> decode_alonzo_output cbor

(* ================================================================ *)
(* Certificate parsing                                               *)
(* ================================================================ *)

let decode_cert_action = function
  | Cbor.Array (Cbor.Uint 0L :: _) -> Cert_stake_registration
  | Cbor.Array (Cbor.Uint 1L :: _) -> Cert_stake_deregistration
  | Cbor.Array (Cbor.Uint 3L :: _) -> Cert_pool_registration
  | Cbor.Array (Cbor.Uint 4L :: _) -> Cert_pool_retirement
  (* Conway certs with explicit deposits *)
  | Cbor.Array [Cbor.Uint 7L; _cred; Cbor.Uint _deposit] ->
    Cert_stake_registration  (* reg with deposit — same effect *)
  | Cbor.Array [Cbor.Uint 8L; _cred; Cbor.Uint _refund] ->
    Cert_stake_deregistration
  | Cbor.Array (Cbor.Uint 9L :: _) -> Cert_vote_delegation
  | Cbor.Array (Cbor.Uint 10L :: _) -> Cert_vote_delegation
  | Cbor.Array (Cbor.Uint 11L :: _) -> Cert_stake_registration
  | Cbor.Array (Cbor.Uint 12L :: _) -> Cert_vote_delegation
  | Cbor.Array (Cbor.Uint 13L :: _) -> Cert_stake_registration
  | Cbor.Array (Cbor.Uint 14L :: _) -> Cert_committee_auth
  | Cbor.Array (Cbor.Uint 15L :: _) -> Cert_committee_resign
  (* DRep certs *)
  | Cbor.Array [Cbor.Uint 16L; _cred; Cbor.Uint deposit; _anchor] ->
    Cert_drep_registration deposit
  | Cbor.Array [Cbor.Uint 16L; _cred; Cbor.Uint deposit] ->
    Cert_drep_registration deposit
  | Cbor.Array [Cbor.Uint 17L; _cred; Cbor.Uint refund] ->
    Cert_drep_deregistration refund
  | Cbor.Array (Cbor.Uint 18L :: _) -> Cert_drep_update
  | _ -> Cert_other

let sum_withdrawals = function
  | Cbor.Map pairs ->
    List.fold_left (fun acc (_, v) ->
      match v with Cbor.Uint n -> Int64.add acc n | _ -> acc
    ) 0L pairs
  | _ -> 0L

(* ================================================================ *)
(* Transaction body decoder                                          *)
(* ================================================================ *)

let list_map_ok f items =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs -> match f x with Ok v -> go (v :: acc) xs | Error _ as e -> e
  in go [] items

let decode_map_tx_body era pairs =
  let find = find_map_uint_key in
  let* inputs = match find 0 pairs with
    | Some (Cbor.Array items) -> list_map_ok decode_input items
    | _ -> Error "tx_body: missing inputs (key 0)" in
  let decode_output = match era with
    | Block_decoder.Byron | Shelley | Allegra -> decode_shelley_output
    | Mary -> decode_mary_output
    | Alonzo -> decode_alonzo_output
    | Babbage | Conway -> decode_babbage_output in
  let* outputs = match find 1 pairs with
    | Some (Cbor.Array items) -> list_map_ok decode_output items
    | _ -> Error "tx_body: missing outputs (key 1)" in
  let fee = match find 2 pairs with
    | Some (Cbor.Uint n) -> n | _ -> 0L in
  let ttl = match find 3 pairs with
    | Some (Cbor.Uint n) -> Some n | _ -> None in
  let validity_start = match find 8 pairs with
    | Some (Cbor.Uint n) -> Some n | _ -> None in
  let certs = match find 4 pairs with
    | Some (Cbor.Array items) -> List.map decode_cert_action items
    | _ -> [] in
  let withdrawal_total = match find 5 pairs with
    | Some wds -> sum_withdrawals wds | None -> 0L in
  let mint = match find 9 pairs with
    | Some v -> Multi_asset.mint_of_cbor v | None -> Multi_asset.zero in
  let collateral_inputs = match find 13 pairs with
    | Some (Cbor.Array items) ->
      List.filter_map (fun c -> match decode_input c with Ok i -> Some i | _ -> None) items
    | _ -> [] in
  let collateral_return = match find 16 pairs with
    | Some cbor -> (match decode_babbage_output cbor with Ok o -> Some o | _ -> None)
    | None -> None in
  let total_collateral = match find 17 pairs with
    | Some (Cbor.Uint n) -> Some n | _ -> None in
  Ok { dt_inputs = inputs; dt_outputs = outputs; dt_fee = fee;
       dt_ttl = ttl; dt_validity_start = validity_start;
       dt_certs = certs; dt_withdrawal_total = withdrawal_total;
       dt_mint = mint;
       dt_collateral_inputs = collateral_inputs;
       dt_collateral_return = collateral_return;
       dt_total_collateral = total_collateral;
       dt_is_valid = true;
       dt_era = era;
       dt_voting_procedures = (match find 19 pairs with
         | Some (Cbor.Map voters) ->
           List.fold_left (fun acc (_, v) ->
             match v with Cbor.Map actions -> acc + List.length actions | _ -> acc
           ) 0 voters
         | _ -> 0);
       dt_proposal_count = (match find 20 pairs with
         | Some (Cbor.Array items) -> List.length items | _ -> 0);
       dt_treasury_donation = (match find 22 pairs with
         | Some (Cbor.Uint n) -> n | _ -> 0L) }

let decode_transaction ~era cbor =
  match era, cbor with
  | Block_decoder.Byron, Cbor.Array [Cbor.Array inputs; Cbor.Array outputs; _attrs] ->
    let* dt_inputs = list_map_ok (function
      | Cbor.Array [Cbor.Uint 0L; Cbor.Tag (24L, Cbor.Bytes inner)] ->
        (match Cbor.decode inner with
         | Ok (Cbor.Array [Cbor.Bytes tx_hash; Cbor.Uint idx]) ->
           Ok { ti_tx_hash = tx_hash; ti_index = idx }
         | _ -> Error "byron input: invalid inner")
      | _ -> Error "byron input: expected [0, #6.24(bytes)]"
    ) inputs in
    let* dt_outputs = list_map_ok (function
      | Cbor.Array [_addr; Cbor.Uint amount] ->
        Ok { to_address = Bytes.empty; to_value = Multi_asset.of_lovelace amount;
             to_has_datum = false; to_has_script_ref = false }
      | _ -> Error "byron output: expected [addr, coin]"
    ) outputs in
    Ok { dt_inputs; dt_outputs; dt_fee = 0L;
         dt_ttl = None; dt_validity_start = None;
         dt_certs = []; dt_withdrawal_total = 0L;
         dt_mint = Multi_asset.zero;
         dt_collateral_inputs = []; dt_collateral_return = None;
         dt_total_collateral = None; dt_is_valid = true;
         dt_era = era;
         dt_voting_procedures = 0; dt_proposal_count = 0;
         dt_treasury_donation = 0L }
  | _, Cbor.Map pairs ->
    decode_map_tx_body era pairs
  | _ -> Error "transaction: expected map (Shelley+) or array (Byron)"
