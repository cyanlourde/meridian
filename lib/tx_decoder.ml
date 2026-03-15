(* Transaction deserialization per era.

   Decodes raw CBOR transaction bodies into structured types.
   Shelley+ txs follow: {0: inputs, 1: outputs, 2: fee, ...}
   Byron txs follow: [inputs, outputs, attributes] *)

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
  to_lovelace : int64;
  to_has_multi_asset : bool;
  to_has_datum : bool;
  to_has_script_ref : bool;
}

type decoded_tx = {
  dt_inputs : tx_input list;
  dt_outputs : tx_output list;
  dt_fee : int64;
  dt_ttl : int64 option;
  dt_validity_start : int64 option;
  dt_cert_count : int;
  dt_withdrawal_count : int;
  dt_mint : bool;
  dt_collateral_count : int;
  dt_era : Block_decoder.era;
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

let decode_shelley_output = function
  | Cbor.Array [Cbor.Bytes addr; Cbor.Uint amount] ->
    Ok { to_address = addr; to_lovelace = amount;
         to_has_multi_asset = false; to_has_datum = false;
         to_has_script_ref = false }
  | _ -> Error "shelley output: expected [addr, coin]"

let decode_mary_output = function
  | Cbor.Array [Cbor.Bytes addr; Cbor.Uint amount] ->
    Ok { to_address = addr; to_lovelace = amount;
         to_has_multi_asset = false; to_has_datum = false;
         to_has_script_ref = false }
  | Cbor.Array [Cbor.Bytes addr; Cbor.Array [Cbor.Uint amount; _multi_asset]] ->
    Ok { to_address = addr; to_lovelace = amount;
         to_has_multi_asset = true; to_has_datum = false;
         to_has_script_ref = false }
  | _ -> Error "mary output: expected [addr, value]"

let decode_alonzo_output = function
  | Cbor.Array [Cbor.Bytes addr; val_cbor] ->
    let lovelace = match val_cbor with
      | Cbor.Uint n -> n
      | Cbor.Array (Cbor.Uint n :: _) -> n
      | _ -> 0L in
    let multi_asset = match val_cbor with
      | Cbor.Array (_ :: _ :: _) -> true | _ -> false in
    Ok { to_address = addr; to_lovelace = lovelace;
         to_has_multi_asset = multi_asset; to_has_datum = false;
         to_has_script_ref = false }
  | Cbor.Array [Cbor.Bytes addr; val_cbor; _datum_hash] ->
    let lovelace = match val_cbor with
      | Cbor.Uint n -> n
      | Cbor.Array (Cbor.Uint n :: _) -> n
      | _ -> 0L in
    Ok { to_address = addr; to_lovelace = lovelace;
         to_has_multi_asset = false; to_has_datum = true;
         to_has_script_ref = false }
  | _ -> Error "alonzo output: unexpected format"

let decode_babbage_output = function
  | Cbor.Map pairs ->
    let addr = match find_map_uint_key 0 pairs with
      | Some (Cbor.Bytes a) -> a | _ -> Bytes.empty in
    let lovelace = match find_map_uint_key 1 pairs with
      | Some (Cbor.Uint n) -> n
      | Some (Cbor.Array (Cbor.Uint n :: _)) -> n
      | _ -> 0L in
    let multi_asset = match find_map_uint_key 1 pairs with
      | Some (Cbor.Array (_ :: _ :: _)) -> true | _ -> false in
    let has_datum = find_map_uint_key 2 pairs <> None in
    let has_script = find_map_uint_key 3 pairs <> None in
    Ok { to_address = addr; to_lovelace = lovelace;
         to_has_multi_asset = multi_asset;
         to_has_datum = has_datum;
         to_has_script_ref = has_script }
  (* Legacy array format also accepted in Babbage *)
  | cbor -> decode_alonzo_output cbor

(* ================================================================ *)
(* Transaction body decoder (CBOR map for Shelley+)                  *)
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
  let cert_count = match find 4 pairs with
    | Some (Cbor.Array items) -> List.length items | _ -> 0 in
  let withdrawal_count = match find 5 pairs with
    | Some (Cbor.Map items) -> List.length items | _ -> 0 in
  let mint = find 9 pairs <> None in
  let collateral_count = match find 13 pairs with
    | Some (Cbor.Array items) -> List.length items | _ -> 0 in
  Ok { dt_inputs = inputs; dt_outputs = outputs; dt_fee = fee;
       dt_ttl = ttl; dt_validity_start = validity_start;
       dt_cert_count = cert_count; dt_withdrawal_count = withdrawal_count;
       dt_mint = mint; dt_collateral_count = collateral_count;
       dt_era = era }

(* ================================================================ *)
(* Public API                                                        *)
(* ================================================================ *)

(** Decode a transaction body from CBOR.
    For Shelley+ eras, the tx_body is a CBOR map with integer keys.
    For Byron, the tx_body is a [inputs, outputs, attributes] array. *)
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
        Ok { to_address = Bytes.empty; to_lovelace = amount;
             to_has_multi_asset = false; to_has_datum = false;
             to_has_script_ref = false }
      | _ -> Error "byron output: expected [addr, coin]"
    ) outputs in
    let fee = 0L in  (* Byron fee is computed, not explicit *)
    Ok { dt_inputs; dt_outputs; dt_fee = fee;
         dt_ttl = None; dt_validity_start = None;
         dt_cert_count = 0; dt_withdrawal_count = 0;
         dt_mint = false; dt_collateral_count = 0;
         dt_era = era }
  | _, Cbor.Map pairs ->
    decode_map_tx_body era pairs
  | _ -> Error "transaction: expected map (Shelley+) or array (Byron)"
