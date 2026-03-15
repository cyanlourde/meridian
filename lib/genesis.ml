(* Genesis configuration parser for Cardano.

   Parses the Shelley genesis JSON file to extract:
   - Network magic and epoch parameters
   - Protocol parameters (fees, min UTXO, etc.)
   - Initial fund distribution (address -> lovelace)

   For preview testnet, initialFunds is empty — the initial ADA
   distribution happens through bootstrap delegation in the first
   blocks. We embed known preview defaults as fallback. *)

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type genesis_config = {
  network_magic : int64;
  epoch_length : int64;
  slot_length : int;             (** milliseconds *)
  active_slots_coeff : float;
  max_kes_evolutions : int;
  max_lovelace_supply : int64;
  protocol_params : Ledger_state.protocol_params;
  initial_funds : (bytes * int64) list;  (** (address_bytes, lovelace) *)
}

(* ================================================================ *)
(* Hex decoding                                                      *)
(* ================================================================ *)

let bytes_of_hex s =
  let n = String.length s / 2 in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set_uint8 b i (int_of_string ("0x" ^ String.sub s (i * 2) 2))
  done;
  b

(* ================================================================ *)
(* Shelley genesis parser                                            *)
(* ================================================================ *)

let parse_shelley_genesis ~path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let st = Unix.fstat fd in
  let buf = Bytes.create st.Unix.st_size in
  let rec go off remaining =
    if remaining = 0 then ()
    else let n = Unix.read fd buf off remaining in go (off + n) (remaining - n)
  in
  go 0 st.st_size;
  Unix.close fd;
  let json = Json.parse (Bytes.to_string buf) in
  let get_int key default =
    match Json.get key json with
    | Some v -> (match Json.to_int v with Some n -> n | None -> default)
    | None -> default in
  let get_int64 key default =
    match Json.get key json with
    | Some v -> (match Json.to_int64 v with Some n -> n | None -> default)
    | None -> default in
  let get_float key default =
    match Json.get key json with
    | Some v -> (match Json.to_float v with Some f -> f | None -> default)
    | None -> default in
  (* Protocol params *)
  let pp = match Json.get "protocolParams" json with
    | Some pp_json ->
      let pp_int key default = match Json.get key pp_json with
        | Some v -> (match Json.to_int v with Some n -> n | None -> default)
        | None -> default in
      let pp_int64 key default = match Json.get key pp_json with
        | Some v -> (match Json.to_int64 v with Some n -> n | None -> default)
        | None -> default in
      Ledger_state.{
        min_fee_a = pp_int64 "minFeeA" 44L;
        min_fee_b = pp_int64 "minFeeB" 155381L;
        min_utxo_value = pp_int64 "minUTxOValue" 1000000L;
        max_tx_size = pp_int "maxTxSize" 16384;
        max_block_size = pp_int "maxBlockBodySize" 65536;
        key_deposit = pp_int64 "keyDeposit" 2000000L;
        pool_deposit = pp_int64 "poolDeposit" 500000000L;
      }
    | None -> Ledger_state.shelley_params in
  (* Initial funds *)
  let initial_funds = match Json.get "initialFunds" json with
    | Some (Json.Object pairs) ->
      List.filter_map (fun (addr_hex, amount_json) ->
        match Json.to_int64 amount_json with
        | Some lovelace when String.length addr_hex >= 2 ->
          Some (bytes_of_hex addr_hex, lovelace)
        | _ -> None
      ) pairs
    | _ -> [] in
  Ok {
    network_magic = get_int64 "networkMagic" 2L;
    epoch_length = get_int64 "epochLength" 432000L;
    slot_length = get_int "slotLength" 1;
    active_slots_coeff = get_float "activeSlotsCoeff" 0.05;
    max_kes_evolutions = get_int "maxKESEvolutions" 62;
    max_lovelace_supply = get_int64 "maxLovelaceSupply" 45000000000000000L;
    protocol_params = pp;
    initial_funds;
  }

(* ================================================================ *)
(* Genesis UTXO derivation                                           *)
(* ================================================================ *)

(** Convert initial funds into UTXO entries.
    Convention: tx_hash = Blake2b-256(address_bytes), tx_index = 0. *)
let genesis_utxos genesis =
  List.map (fun (addr_bytes, lovelace) ->
    let tx_hash = Crypto.blake2b_256 addr_bytes in
    let txin = Utxo.TxIn.{ tx_hash; tx_index = 0 } in
    let txout = Utxo.TxOut.{
      address = addr_bytes; lovelace;
      has_multi_asset = false; has_datum = false;
      has_script_ref = false
    } in
    (txin, txout)
  ) genesis.initial_funds

(* ================================================================ *)
(* Preview testnet defaults                                          *)
(* ================================================================ *)

(** Embedded preview testnet genesis config.
    Preview has empty initialFunds — ADA is distributed via first blocks. *)
let preview_genesis = {
  network_magic = 2L;
  epoch_length = 86400L;
  slot_length = 1;
  active_slots_coeff = 0.05;
  max_kes_evolutions = 62;
  max_lovelace_supply = 45000000000000000L;
  protocol_params = Ledger_state.{
    min_fee_a = 44L;
    min_fee_b = 155381L;
    min_utxo_value = 1000000L;
    max_tx_size = 16384;
    max_block_size = 65536;
    key_deposit = 2000000L;
    pool_deposit = 500000000L;
  };
  initial_funds = [];
}

(** Mainnet genesis placeholder — not yet fully parsed. *)
let mainnet_genesis = {
  network_magic = 764824073L;
  epoch_length = 432000L;
  slot_length = 1;
  active_slots_coeff = 0.05;
  max_kes_evolutions = 62;
  max_lovelace_supply = 45000000000000000L;
  protocol_params = Ledger_state.shelley_params;
  initial_funds = [];
}

(* ================================================================ *)
(* Ledger initialization from genesis                                *)
(* ================================================================ *)

(** Create a ledger state pre-loaded with genesis UTXOs. *)
let init_ledger genesis =
  let ls = Ledger_state.create ~params:genesis.protocol_params () in
  let utxos = genesis_utxos genesis in
  List.iter (fun (txin, txout) ->
    Utxo.add (Ledger_state.utxo ls) txin txout
  ) utxos;
  ls
