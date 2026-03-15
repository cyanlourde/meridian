open Meridian

let () = Crypto.init ()

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-gen-test-%d-%d" (Unix.getpid ()) (Random.int 100000))

let rm_rf dir =
  let rec go path =
    if Sys.is_directory path then begin
      Array.iter (fun f -> go (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
    end else Unix.unlink path
  in
  if Sys.file_exists dir then go dir

let write_file path content =
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let b = Bytes.of_string content in
  ignore (Unix.write fd b 0 (Bytes.length b));
  Unix.close fd

(* ================================================================ *)
(* JSON parser                                                       *)
(* ================================================================ *)

let test_json_basic () =
  let j = Json.parse {|{"a": 1, "b": "hello", "c": true, "d": null}|} in
  (match Json.get "a" j with
   | Some v ->
     (match Json.to_float v with
      | Some f -> Alcotest.(check (float 0.001)) "a=1" 1.0 f
      | None -> Alcotest.fail "a not a number")
   | None -> Alcotest.fail "missing a");
  (match Json.get "b" j with
   | Some v ->
     (match Json.to_string v with
      | Some s -> Alcotest.(check string) "b=hello" "hello" s
      | None -> Alcotest.fail "b not a string")
   | None -> Alcotest.fail "missing b");
  (match Json.get "c" j with
   | Some v ->
     (match Json.to_bool v with
      | Some b -> Alcotest.(check bool) "c=true" true b
      | None -> Alcotest.fail "c not a bool")
   | None -> Alcotest.fail "missing c");
  Alcotest.(check bool) "d is null" true
    (Json.get "d" j = Some Json.Null)

let test_json_nested () =
  let j = Json.parse {|{"x": {"y": 42}}|} in
  match Json.get "x" j with
  | Some inner ->
    (match Json.get "y" inner with
     | Some v ->
       (match Json.to_float v with
        | Some f -> Alcotest.(check (float 0.001)) "y=42" 42.0 f
        | None -> Alcotest.fail "y not number")
     | None -> Alcotest.fail "missing y")
  | None -> Alcotest.fail "missing x"

let test_json_array () =
  let j = Json.parse {|[1, 2, 3]|} in
  match j with
  | Json.Array items -> Alcotest.(check int) "3 items" 3 (List.length items)
  | _ -> Alcotest.fail "expected array"

(* ================================================================ *)
(* Shelley genesis parsing                                           *)
(* ================================================================ *)

let test_parse_genesis_file () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "shelley-genesis.json" in
  write_file path {|{
    "networkMagic": 42,
    "epochLength": 100000,
    "slotLength": 1,
    "activeSlotsCoeff": 0.1,
    "maxKESEvolutions": 62,
    "maxLovelaceSupply": 45000000000000000,
    "protocolParams": {
      "minFeeA": 44,
      "minFeeB": 155381,
      "minUTxOValue": 1000000,
      "maxTxSize": 16384,
      "maxBlockBodySize": 65536
    },
    "initialFunds": {
      "60ab0123456789abcdef0123456789abcdef0123456789abcdef01234567": 5000000000,
      "60fedcba9876543210fedcba9876543210fedcba9876543210fedcba987654": 3000000000,
      "6000112233445566778899aabbccddeeff00112233445566778899aabbccddee": 2000000000
    }
  }|};
  match Genesis.parse_shelley_genesis ~path with
  | Error e -> Alcotest.fail e
  | Ok g ->
    Alcotest.(check int64) "magic" 42L g.network_magic;
    Alcotest.(check int64) "epoch_length" 100000L g.epoch_length;
    Alcotest.(check int64) "minFeeA" 44L g.protocol_params.min_fee_a;
    Alcotest.(check int64) "minFeeB" 155381L g.protocol_params.min_fee_b;
    Alcotest.(check int64) "minUTxO" 1000000L g.protocol_params.min_utxo_value;
    Alcotest.(check int) "3 initial funds" 3 (List.length g.initial_funds);
    let total = List.fold_left (fun acc (_, v) -> Int64.add acc v) 0L g.initial_funds in
    Alcotest.(check int64) "total 10B" 10000000000L total;
    rm_rf dir

(* ================================================================ *)
(* Genesis UTXO derivation                                           *)
(* ================================================================ *)

let test_genesis_utxos () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "shelley-genesis.json" in
  write_file path {|{
    "networkMagic": 2,
    "protocolParams": {"minFeeA": 44, "minFeeB": 155381, "minUTxOValue": 1000000,
                       "maxTxSize": 16384, "maxBlockBodySize": 65536},
    "initialFunds": {
      "60ab0123456789abcdef0123456789abcdef0123456789abcdef01234567": 5000000
    }
  }|};
  match Genesis.parse_shelley_genesis ~path with
  | Error e -> Alcotest.fail e
  | Ok g ->
    let utxos = Genesis.genesis_utxos g in
    Alcotest.(check int) "1 utxo" 1 (List.length utxos);
    let (txin, txout) = List.hd utxos in
    Alcotest.(check int) "tx_hash 32 bytes" 32 (Bytes.length txin.Utxo.TxIn.tx_hash);
    Alcotest.(check int) "tx_index 0" 0 txin.tx_index;
    Alcotest.(check int64) "lovelace" 5000000L txout.Utxo.TxOut.lovelace;
    rm_rf dir

let test_genesis_tx_hash_deterministic () =
  let addr = Bytes.of_string "test_address" in
  let h1 = Crypto.blake2b_256 addr in
  let h2 = Crypto.blake2b_256 addr in
  Alcotest.(check bool) "deterministic" true (Bytes.equal h1 h2);
  Alcotest.(check int) "32 bytes" 32 (Bytes.length h1)

(* ================================================================ *)
(* Ledger initialization from genesis                                *)
(* ================================================================ *)

let test_init_ledger () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "shelley-genesis.json" in
  write_file path {|{
    "networkMagic": 2,
    "protocolParams": {"minFeeA": 44, "minFeeB": 155381, "minUTxOValue": 1000000,
                       "maxTxSize": 16384, "maxBlockBodySize": 65536},
    "initialFunds": {
      "60aaaa": 3000000,
      "60bbbb": 7000000
    }
  }|};
  match Genesis.parse_shelley_genesis ~path with
  | Error e -> Alcotest.fail e
  | Ok g ->
    let ls = Genesis.init_ledger g in
    Alcotest.(check int) "2 utxos" 2 (Ledger_state.utxo_count ls);
    Alcotest.(check int64) "total" 10000000L (Ledger_state.total_lovelace ls);
    rm_rf dir

let test_init_ledger_empty () =
  let g = Genesis.{ preview_genesis with initial_funds = [] } in
  let ls = Genesis.init_ledger g in
  Alcotest.(check int) "0 utxos" 0 (Ledger_state.utxo_count ls)

(* ================================================================ *)
(* Apply block after genesis                                         *)
(* ================================================================ *)

let test_apply_after_genesis () =
  (* Create genesis with one fund, then apply a block consuming it *)
  let addr = Bytes.make 29 '\x61' in
  let g = Genesis.{
    preview_genesis with
    initial_funds = [(addr, 5000000L)];
  } in
  let ls = Genesis.init_ledger g in
  Alcotest.(check int) "1 genesis utxo" 1 (Ledger_state.utxo_count ls);
  (* The genesis tx_hash = blake2b_256(addr) *)
  let genesis_tx_hash = Crypto.blake2b_256 addr in
  (* Build a tx that spends the genesis output *)
  let tx_cbor = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [
      Cbor.Array [Cbor.Bytes genesis_tx_hash; Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [
      Cbor.Array [Cbor.Bytes addr; Cbor.Uint 4800000L]]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
  ] in
  let block = Block_decoder.{
    db_era = Shelley;
    db_header = {
      bh_slot = 100L; bh_block_number = 1L;
      bh_prev_hash = None; bh_issuer_vkey = Bytes.empty;
      bh_body_hash = Bytes.empty;
      bh_protocol_version = (2L, 0L); bh_era = Shelley;
      bh_vrf_vkey = Bytes.empty; bh_block_signature = Bytes.empty;
      bh_opcert = None; bh_header_body_cbor = Bytes.empty;
    };
    db_tx_count = 1; db_tx_raw = [tx_cbor];
    db_raw_cbor = Cbor.Null;
  } in
  let errors = Ledger_state.apply_block ls block in
  (* Should have no "input not in utxo" errors *)
  let has_missing = List.exists (fun (e : Ledger_state.block_error) ->
    List.exists (function
      | Utxo.Input_not_in_utxo _ -> true | _ -> false) e.be_errors
  ) errors in
  Alcotest.(check bool) "genesis input found" false has_missing;
  (* UTXO should have the new output *)
  Alcotest.(check int) "1 utxo after" 1 (Ledger_state.utxo_count ls)

(* ================================================================ *)
(* Preview defaults                                                  *)
(* ================================================================ *)

let test_preview_defaults () =
  let g = Genesis.preview_genesis in
  Alcotest.(check int64) "magic 2" 2L g.network_magic;
  Alcotest.(check int64) "epoch 86400" 86400L g.epoch_length;
  Alcotest.(check int) "empty funds" 0 (List.length g.initial_funds);
  Alcotest.(check int64) "minFeeA" 44L g.protocol_params.min_fee_a

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Genesis"
    [ ( "JSON parser",
        [ Alcotest.test_case "basic" `Quick test_json_basic;
          Alcotest.test_case "nested" `Quick test_json_nested;
          Alcotest.test_case "array" `Quick test_json_array ] );
      ( "Shelley genesis",
        [ Alcotest.test_case "parse file" `Quick test_parse_genesis_file ] );
      ( "Genesis UTXOs",
        [ Alcotest.test_case "derivation" `Quick test_genesis_utxos;
          Alcotest.test_case "hash deterministic" `Quick test_genesis_tx_hash_deterministic ] );
      ( "Ledger init",
        [ Alcotest.test_case "from genesis" `Quick test_init_ledger;
          Alcotest.test_case "empty genesis" `Quick test_init_ledger_empty;
          Alcotest.test_case "apply after genesis" `Quick test_apply_after_genesis ] );
      ( "Preview defaults",
        [ Alcotest.test_case "embedded values" `Quick test_preview_defaults ] );
    ]
