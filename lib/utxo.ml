(* UTXO set management for Cardano.

   Reference: Shelley formal ledger spec (SL-D5), Section 9 "UTxO"

   Conservation equation (multi-asset):
     consumed + mint = produced
   Where:
     consumed = sum(input_values) + withdrawals_value + refund_value
     produced = sum(output_values) + fee_value + deposit_value

   Performance: Hashtbl-based for O(1) amortized lookups.
   Incremental lovelace tracking on add/remove. *)

module TxIn = struct
  type t = { tx_hash : bytes; tx_index : int }

  let compare a b =
    let c = Bytes.compare a.tx_hash b.tx_hash in
    if c <> 0 then c else Int.compare a.tx_index b.tx_index

  let of_decoder (inp : Tx_decoder.tx_input) =
    { tx_hash = inp.ti_tx_hash; tx_index = Int64.to_int inp.ti_index }

  let equal a b = Bytes.equal a.tx_hash b.tx_hash && a.tx_index = b.tx_index

  let hash t =
    (* Fast hash: combine first 8 bytes of tx_hash with tx_index *)
    let h = ref 0 in
    for i = 0 to min 7 (Bytes.length t.tx_hash - 1) do
      h := !h * 31 + Bytes.get_uint8 t.tx_hash i
    done;
    !h lxor (t.tx_index * 65537)

  let to_string t =
    let hex = Buffer.create 16 in
    for i = 0 to min 7 (Bytes.length t.tx_hash - 1) do
      Buffer.add_string hex (Printf.sprintf "%02x" (Bytes.get_uint8 t.tx_hash i))
    done;
    Printf.sprintf "%s..#%d" (Buffer.contents hex) t.tx_index
end

module TxOut = struct
  type t = {
    address : bytes;
    value : Multi_asset.value;
    has_datum : bool;
    has_script_ref : bool;
  }

  let of_decoder (out : Tx_decoder.tx_output) =
    { address = out.to_address; value = out.to_value;
      has_datum = out.to_has_datum; has_script_ref = out.to_has_script_ref }
end

(* Hashtbl with custom hash/equal for TxIn *)
module TxInHash = Hashtbl.Make(struct
  type t = TxIn.t
  let equal = TxIn.equal
  let hash = TxIn.hash
end)

type utxo_set = {
  entries : TxOut.t TxInHash.t;
  mutable size : int;
  mutable total_lovelace_cache : int64;
}

let create () = {
  entries = TxInHash.create 500_000;
  size = 0;
  total_lovelace_cache = 0L;
}

let size s = s.size

let mem s txin = TxInHash.mem s.entries txin

let find s txin = TxInHash.find_opt s.entries txin

let add s txin txout =
  (match TxInHash.find_opt s.entries txin with
   | Some old ->
     s.total_lovelace_cache <- Int64.sub s.total_lovelace_cache
       (Multi_asset.lovelace_of old.TxOut.value)
   | None -> s.size <- s.size + 1);
  TxInHash.replace s.entries txin txout;
  s.total_lovelace_cache <- Int64.add s.total_lovelace_cache
    (Multi_asset.lovelace_of txout.value)

let remove s txin =
  match TxInHash.find_opt s.entries txin with
  | Some old ->
    TxInHash.remove s.entries txin;
    s.size <- s.size - 1;
    s.total_lovelace_cache <- Int64.sub s.total_lovelace_cache
      (Multi_asset.lovelace_of old.TxOut.value)
  | None -> ()

let iter f s = TxInHash.iter (fun k v -> f k v) s.entries

let find_by_address s ~address =
  TxInHash.fold (fun k v acc ->
    if Bytes.equal v.TxOut.address address then (k, v) :: acc else acc
  ) s.entries []

let find_by_txins s txins =
  List.map (fun txin -> (txin, TxInHash.find_opt s.entries txin)) txins

let total_lovelace s = s.total_lovelace_cache

let total_assets s =
  TxInHash.fold (fun _k v acc ->
    acc + Multi_asset.asset_count v.TxOut.value) s.entries 0

(* ================================================================ *)
(* Validation errors                                                 *)
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

(* ================================================================ *)
(* Deposit/refund                                                    *)
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
    | Cert_drep_registration deposit ->
      deposits := Int64.add !deposits deposit
    | Cert_drep_deregistration refund ->
      refunds := Int64.add !refunds refund
    | Cert_pool_retirement | Cert_drep_update | Cert_vote_delegation
    | Cert_committee_auth | Cert_committee_resign | Cert_other -> ()
  ) certs;
  (!deposits, !refunds)

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
    List.iter (fun (inp : Tx_decoder.tx_input) ->
      let txin = TxIn.of_decoder inp in
      if not (mem utxo txin) then add (Input_not_in_utxo txin)
    ) tx.dt_collateral_inputs;
    List.rev !errors
  end else begin
    if tx.dt_inputs = [] then add Empty_inputs;
    if tx.dt_outputs = [] then add Empty_outputs;

    let seen = Hashtbl.create (List.length tx.dt_inputs) in
    let consumed_value = ref Multi_asset.zero in
    List.iter (fun (inp : Tx_decoder.tx_input) ->
      let txin = TxIn.of_decoder inp in
      if Hashtbl.mem seen (txin.tx_hash, txin.tx_index) then
        add (Duplicate_input txin)
      else begin
        Hashtbl.replace seen (txin.tx_hash, txin.tx_index) ();
        match find utxo txin with
        | None -> add (Input_not_in_utxo txin)
        | Some out -> consumed_value := Multi_asset.add !consumed_value out.value
      end
    ) tx.dt_inputs;

    let required_fee = min_fee ~min_fee_a ~min_fee_b ~tx_size:tx_size_estimate in
    if tx.dt_fee < required_fee then
      add (Insufficient_fee { required = required_fee; actual = tx.dt_fee });

    List.iteri (fun i (out : Tx_decoder.tx_output) ->
      if Multi_asset.lovelace_of out.to_value < min_utxo_value then
        add (Output_too_small { index = i;
               lovelace = Multi_asset.lovelace_of out.to_value;
               minimum = min_utxo_value })
    ) tx.dt_outputs;

    (match tx.dt_ttl with
     | Some ttl when Int64.compare current_slot ttl > 0 ->
       add (Expired_ttl { slot = current_slot; ttl })
     | _ -> ());

    let (deposits, refunds) = compute_deposits ~key_deposit ~pool_deposit tx.dt_certs in
    let consumed_with_extras = Multi_asset.add !consumed_value
      (Multi_asset.of_lovelace (Int64.add tx.dt_withdrawal_total refunds)) in
    let consumed_plus_mint = Multi_asset.add consumed_with_extras tx.dt_mint in

    let produced_outputs = List.fold_left (fun acc (out : Tx_decoder.tx_output) ->
      Multi_asset.add acc out.to_value) Multi_asset.zero tx.dt_outputs in
    let produced = Multi_asset.add produced_outputs
      (Multi_asset.of_lovelace
         (Int64.add (Int64.add tx.dt_fee deposits) tx.dt_treasury_donation)) in

    let consumed_lovelace = Multi_asset.lovelace_of !consumed_value in
    if consumed_lovelace > 0L && not (Multi_asset.equal
         (Multi_asset.filter_zero consumed_plus_mint)
         (Multi_asset.filter_zero produced)) then
      add (Value_not_conserved {
        consumed = Multi_asset.lovelace_of consumed_plus_mint;
        produced = Multi_asset.lovelace_of produced });

    List.rev !errors
  end

(* ================================================================ *)
(* UTxO transition                                                   *)
(* ================================================================ *)

let apply_tx utxo ~tx_hash (tx : Tx_decoder.decoded_tx) =
  if not tx.dt_is_valid then begin
    List.iter (fun (inp : Tx_decoder.tx_input) ->
      remove utxo (TxIn.of_decoder inp)
    ) tx.dt_collateral_inputs;
    (match tx.dt_collateral_return with
     | Some out ->
       add utxo TxIn.{ tx_hash; tx_index = 0 } (TxOut.of_decoder out)
     | None -> ())
  end else begin
    List.iter (fun (inp : Tx_decoder.tx_input) ->
      remove utxo (TxIn.of_decoder inp)
    ) tx.dt_inputs;
    List.iteri (fun i (out : Tx_decoder.tx_output) ->
      add utxo TxIn.{ tx_hash; tx_index = i } (TxOut.of_decoder out)
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
