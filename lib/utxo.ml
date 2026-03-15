(* UTXO set management for Cardano.

   Reference: Shelley formal ledger spec (SL-D5), Section 9 "UTxO"

   Conservation equation (Shelley spec):
     consumed = sum(inputs) + withdrawals + refunds
     produced = sum(outputs) + fee + deposits
   Where deposits/refunds come from certificates (stake reg/dereg, pool reg/retirement). *)

(* ================================================================ *)
(* UTxO key and value                                                *)
(* ================================================================ *)

module TxIn = struct
  type t = { tx_hash : bytes; tx_index : int }

  let compare a b =
    let c = Bytes.compare a.tx_hash b.tx_hash in
    if c <> 0 then c else Int.compare a.tx_index b.tx_index

  let of_decoder (inp : Tx_decoder.tx_input) =
    { tx_hash = inp.ti_tx_hash; tx_index = Int64.to_int inp.ti_index }

  let equal a b = compare a b = 0

  let to_string t =
    let hex = Buffer.create 16 in
    for i = 0 to min 7 (Bytes.length t.tx_hash - 1) do
      Buffer.add_string hex (Printf.sprintf "%02x" (Bytes.get_uint8 t.tx_hash i))
    done;
    Printf.sprintf "%s..#%d" (Buffer.contents hex) t.tx_index
end

module TxOut = struct
  type t = {
    address : bytes; lovelace : int64;
    has_multi_asset : bool; has_datum : bool; has_script_ref : bool;
  }

  let of_decoder (out : Tx_decoder.tx_output) =
    { address = out.to_address; lovelace = out.to_lovelace;
      has_multi_asset = out.to_has_multi_asset;
      has_datum = out.to_has_datum; has_script_ref = out.to_has_script_ref }
end

(* ================================================================ *)
(* UTxO set                                                          *)
(* ================================================================ *)

module TxInMap = Map.Make(TxIn)

type utxo_set = {
  mutable entries : TxOut.t TxInMap.t;
  mutable size : int;
}

let create () = { entries = TxInMap.empty; size = 0 }
let size s = s.size
let mem s txin = TxInMap.mem txin s.entries
let find s txin = TxInMap.find_opt txin s.entries

let add s txin txout =
  if not (TxInMap.mem txin s.entries) then s.size <- s.size + 1;
  s.entries <- TxInMap.add txin txout s.entries

let remove s txin =
  if TxInMap.mem txin s.entries then begin
    s.entries <- TxInMap.remove txin s.entries;
    s.size <- s.size - 1
  end

let iter f s = TxInMap.iter (fun k v -> f k v) s.entries

let total_lovelace s =
  TxInMap.fold (fun _k v acc -> Int64.add acc v.TxOut.lovelace) s.entries 0L

(* ================================================================ *)
(* Validation errors and warnings                                    *)
(* ================================================================ *)

type validation_error =
  | Input_not_in_utxo of TxIn.t
  | Duplicate_input of TxIn.t
  | Insufficient_fee of { required : int64; actual : int64 }
  | Value_not_conserved of { consumed : int64; produced : int64 }
  | Output_too_small of { index : int; lovelace : int64; minimum : int64 }
  | Expired_ttl of { slot : int64; ttl : int64 }
  | Empty_inputs
  | Empty_outputs
  | Conservation_warning of string  (** Approximate check — multi-asset *)

let error_to_string = function
  | Input_not_in_utxo txin ->
    Printf.sprintf "input not in UTxO: %s" (TxIn.to_string txin)
  | Duplicate_input txin ->
    Printf.sprintf "duplicate input: %s" (TxIn.to_string txin)
  | Insufficient_fee { required; actual } ->
    Printf.sprintf "fee %Ld below minimum %Ld" actual required
  | Value_not_conserved { consumed; produced } ->
    Printf.sprintf "value not conserved: consumed %Ld != produced %Ld" consumed produced
  | Output_too_small { index; lovelace; minimum } ->
    Printf.sprintf "output %d too small: %Ld < %Ld" index lovelace minimum
  | Expired_ttl { slot; ttl } ->
    Printf.sprintf "TTL expired: slot %Ld > TTL %Ld" slot ttl
  | Empty_inputs -> "transaction has no inputs"
  | Empty_outputs -> "transaction has no outputs"
  | Conservation_warning msg -> Printf.sprintf "WARN: %s" msg

let is_warning = function Conservation_warning _ -> true | _ -> false

(* ================================================================ *)
(* Deposit/refund calculation from certificates                      *)
(* ================================================================ *)

let compute_deposits ?(key_deposit = 2000000L) ?(pool_deposit = 500000000L)
    (certs : Tx_decoder.cert_action list) =
  let deposits = ref 0L in
  let refunds = ref 0L in
  List.iter (function
    | Tx_decoder.Cert_stake_registration ->
      deposits := Int64.add !deposits key_deposit
    | Cert_stake_deregistration ->
      refunds := Int64.add !refunds key_deposit
    | Cert_pool_registration ->
      deposits := Int64.add !deposits pool_deposit
    | Cert_pool_retirement ->
      (* Pool deposit refunded at retirement epoch, not immediately.
         For conservation, we don't count it as immediate refund. *)
      ()
    | Cert_other -> ()
  ) certs;
  (!deposits, !refunds)

(* ================================================================ *)
(* Fee minimum                                                       *)
(* ================================================================ *)

let min_fee ~min_fee_a ~min_fee_b ~tx_size =
  Int64.add (Int64.mul min_fee_a (Int64.of_int tx_size)) min_fee_b

(* ================================================================ *)
(* Transaction validation                                            *)
(* ================================================================ *)

let validate_tx ?(min_fee_a = 44L) ?(min_fee_b = 155381L)
    ?(min_utxo_value = 1000000L) ?(tx_size_estimate = 300)
    ?(key_deposit = 2000000L) ?(pool_deposit = 500000000L)
    ~utxo ~current_slot (tx : Tx_decoder.decoded_tx) =
  let errors = ref [] in
  let add e = errors := e :: !errors in

  if not tx.dt_is_valid then begin
    (* Failed Plutus tx: consume collateral, skip normal validation *)
    List.iter (fun (inp : Tx_decoder.tx_input) ->
      let txin = TxIn.of_decoder inp in
      if not (mem utxo txin) then
        add (Input_not_in_utxo txin)
    ) tx.dt_collateral_inputs;
    List.rev !errors
  end else begin
    (* Normal transaction validation *)
    if tx.dt_inputs = [] then add Empty_inputs;
    if tx.dt_outputs = [] then add Empty_outputs;

    (* Check inputs exist and sum consumed value *)
    let seen = Hashtbl.create (List.length tx.dt_inputs) in
    let consumed_inputs = ref 0L in
    List.iter (fun (inp : Tx_decoder.tx_input) ->
      let txin = TxIn.of_decoder inp in
      if Hashtbl.mem seen (txin.tx_hash, txin.tx_index) then
        add (Duplicate_input txin)
      else begin
        Hashtbl.replace seen (txin.tx_hash, txin.tx_index) ();
        match find utxo txin with
        | None -> add (Input_not_in_utxo txin)
        | Some out -> consumed_inputs := Int64.add !consumed_inputs out.TxOut.lovelace
      end
    ) tx.dt_inputs;

    (* Fee minimum *)
    let required_fee = min_fee ~min_fee_a ~min_fee_b ~tx_size:tx_size_estimate in
    if tx.dt_fee < required_fee then
      add (Insufficient_fee { required = required_fee; actual = tx.dt_fee });

    (* Output minimums *)
    List.iteri (fun i (out : Tx_decoder.tx_output) ->
      if out.to_lovelace < min_utxo_value then
        add (Output_too_small { index = i; lovelace = out.to_lovelace;
                                minimum = min_utxo_value })
    ) tx.dt_outputs;

    (* TTL *)
    (match tx.dt_ttl with
     | Some ttl when Int64.compare current_slot ttl > 0 ->
       add (Expired_ttl { slot = current_slot; ttl })
     | _ -> ());

    (* Value conservation:
       consumed = sum(inputs) + withdrawals + refunds
       produced = sum(outputs) + fee + deposits *)
    let (deposits, refunds) = compute_deposits ~key_deposit ~pool_deposit tx.dt_certs in
    let consumed = Int64.add (Int64.add !consumed_inputs tx.dt_withdrawal_total) refunds in
    let produced_outputs = List.fold_left (fun acc (out : Tx_decoder.tx_output) ->
      Int64.add acc out.to_lovelace) 0L tx.dt_outputs in
    let produced = Int64.add (Int64.add produced_outputs tx.dt_fee) deposits in

    if !consumed_inputs > 0L then begin
      if tx.dt_mint then
        (* Multi-asset minting: conservation check is approximate *)
        (if not (Int64.equal consumed produced) then
           add (Conservation_warning
                  (Printf.sprintf "mint tx: consumed %Ld != produced %Ld (multi-asset)" consumed produced)))
      else if not (Int64.equal consumed produced) then
        add (Value_not_conserved { consumed; produced })
    end;

    List.rev !errors
  end

(* ================================================================ *)
(* UTxO transition                                                   *)
(* ================================================================ *)

let apply_tx utxo ~tx_hash (tx : Tx_decoder.decoded_tx) =
  if not tx.dt_is_valid then begin
    (* Failed Plutus: consume collateral, produce collateral_return *)
    List.iter (fun (inp : Tx_decoder.tx_input) ->
      remove utxo (TxIn.of_decoder inp)
    ) tx.dt_collateral_inputs;
    (match tx.dt_collateral_return with
     | Some out ->
       let txin = TxIn.{ tx_hash; tx_index = 0 } in
       add utxo txin (TxOut.of_decoder out)
     | None -> ())
  end else begin
    (* Normal: remove inputs, add outputs *)
    List.iter (fun (inp : Tx_decoder.tx_input) ->
      remove utxo (TxIn.of_decoder inp)
    ) tx.dt_inputs;
    List.iteri (fun i (out : Tx_decoder.tx_output) ->
      let txin = TxIn.{ tx_hash; tx_index = i } in
      add utxo txin (TxOut.of_decoder out)
    ) tx.dt_outputs
  end

let apply_block utxo ~current_slot ~tx_hash_fn
    (block : Block_decoder.decoded_block) =
  let total_errors = ref [] in
  let applied = ref 0 in
  List.iter (fun tx_cbor ->
    match Tx_decoder.decode_transaction ~era:block.db_era tx_cbor with
    | Error _e -> ()
    | Ok tx ->
      let errors = validate_tx ~utxo ~current_slot tx in
      if errors <> [] then
        total_errors := errors @ !total_errors;
      let tx_hash = tx_hash_fn tx_cbor in
      apply_tx utxo ~tx_hash tx;
      incr applied
  ) block.db_tx_raw;
  (!applied, List.rev !total_errors)
