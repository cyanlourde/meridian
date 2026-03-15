open Meridian

let pid1 = Bytes.make 28 '\xaa'
let pid2 = Bytes.make 28 '\xbb'
let name1 = Bytes.of_string "Token1"
let name2 = Bytes.of_string "Token2"

(* ================================================================ *)
(* Value arithmetic                                                  *)
(* ================================================================ *)

let test_add_values () =
  let a = Multi_asset.{ lovelace = 5000000L;
    assets = [(pid1, [(name1, 100L)])] } in
  let b = Multi_asset.{ lovelace = 3000000L;
    assets = [(pid1, [(name1, 50L); (name2, 200L)])] } in
  let c = Multi_asset.add a b in
  Alcotest.(check int64) "lovelace" 8000000L c.lovelace;
  (* Token1 = 100 + 50 = 150 *)
  let t1_qty = List.assoc pid1 c.assets |> List.assoc name1 in
  Alcotest.(check int64) "token1" 150L t1_qty;
  let t2_qty = List.assoc pid1 c.assets |> List.assoc name2 in
  Alcotest.(check int64) "token2" 200L t2_qty

let test_add_different_policies () =
  let a = Multi_asset.{ lovelace = 1000000L;
    assets = [(pid1, [(name1, 10L)])] } in
  let b = Multi_asset.{ lovelace = 2000000L;
    assets = [(pid2, [(name2, 20L)])] } in
  let c = Multi_asset.add a b in
  Alcotest.(check int64) "lovelace" 3000000L c.lovelace;
  Alcotest.(check int) "2 policies" 2 (List.length c.assets)

let test_subtract () =
  let a = Multi_asset.{ lovelace = 5000000L;
    assets = [(pid1, [(name1, 100L)])] } in
  let b = Multi_asset.{ lovelace = 2000000L;
    assets = [(pid1, [(name1, 30L)])] } in
  let c = Multi_asset.subtract a b in
  Alcotest.(check int64) "lovelace" 3000000L c.lovelace;
  let qty = List.assoc pid1 c.assets |> List.assoc name1 in
  Alcotest.(check int64) "token" 70L qty

(* ================================================================ *)
(* Lovelace-only                                                     *)
(* ================================================================ *)

let test_lovelace_only () =
  let v = Multi_asset.of_lovelace 5000000L in
  Alcotest.(check int64) "lovelace" 5000000L v.lovelace;
  Alcotest.(check int) "no assets" 0 (Multi_asset.asset_count v);
  Alcotest.(check bool) "lovelace only" true (Multi_asset.is_lovelace_only v)

let test_not_lovelace_only () =
  let v = Multi_asset.{ lovelace = 1000000L;
    assets = [(pid1, [(name1, 1L)])] } in
  Alcotest.(check bool) "not lovelace only" false (Multi_asset.is_lovelace_only v)

(* ================================================================ *)
(* Equality                                                          *)
(* ================================================================ *)

let test_equal_same () =
  let v = Multi_asset.{ lovelace = 5000000L;
    assets = [(pid1, [(name1, 100L); (name2, 200L)])] } in
  Alcotest.(check bool) "equal" true (Multi_asset.equal v v)

let test_equal_different_order () =
  let a = Multi_asset.{ lovelace = 5000000L;
    assets = [(pid1, [(name1, 100L)]); (pid2, [(name2, 50L)])] } in
  let b = Multi_asset.{ lovelace = 5000000L;
    assets = [(pid2, [(name2, 50L)]); (pid1, [(name1, 100L)])] } in
  Alcotest.(check bool) "equal different order" true (Multi_asset.equal a b)

let test_not_equal () =
  let a = Multi_asset.of_lovelace 5000000L in
  let b = Multi_asset.of_lovelace 3000000L in
  Alcotest.(check bool) "not equal" false (Multi_asset.equal a b)

(* ================================================================ *)
(* Filter zero                                                       *)
(* ================================================================ *)

let test_filter_zero () =
  let v = Multi_asset.{ lovelace = 5000000L;
    assets = [(pid1, [(name1, 100L); (name2, 0L)])] } in
  let filtered = Multi_asset.filter_zero v in
  let names = List.assoc pid1 filtered.assets in
  Alcotest.(check int) "1 non-zero" 1 (List.length names)

let test_filter_all_zero () =
  let v = Multi_asset.{ lovelace = 0L;
    assets = [(pid1, [(name1, 0L)])] } in
  let filtered = Multi_asset.filter_zero v in
  Alcotest.(check int) "no policies" 0 (List.length filtered.assets)

(* ================================================================ *)
(* is_positive                                                       *)
(* ================================================================ *)

let test_positive () =
  let v = Multi_asset.{ lovelace = 1000000L;
    assets = [(pid1, [(name1, 100L)])] } in
  Alcotest.(check bool) "positive" true (Multi_asset.is_positive v)

let test_negative_asset () =
  let v = Multi_asset.{ lovelace = 1000000L;
    assets = [(pid1, [(name1, -50L)])] } in
  Alcotest.(check bool) "not positive" false (Multi_asset.is_positive v)

(* ================================================================ *)
(* CBOR parsing                                                      *)
(* ================================================================ *)

let test_cbor_lovelace_only () =
  let v = Multi_asset.of_cbor (Cbor.Uint 5000000L) in
  Alcotest.(check int64) "lovelace" 5000000L v.lovelace;
  Alcotest.(check bool) "no assets" true (Multi_asset.is_lovelace_only v)

let test_cbor_multi_asset () =
  let cbor = Cbor.Array [
    Cbor.Uint 2000000L;
    Cbor.Map [
      (Cbor.Bytes pid1, Cbor.Map [
        (Cbor.Bytes name1, Cbor.Uint 100L);
        (Cbor.Bytes name2, Cbor.Uint 200L)]);
    ]] in
  let v = Multi_asset.of_cbor cbor in
  Alcotest.(check int64) "lovelace" 2000000L v.lovelace;
  Alcotest.(check int) "1 policy" 1 (List.length v.assets);
  Alcotest.(check int) "2 assets" 2 (Multi_asset.asset_count v)

let test_cbor_mint () =
  let cbor = Cbor.Map [
    (Cbor.Bytes pid1, Cbor.Map [
      (Cbor.Bytes name1, Cbor.Uint 100L);
      (Cbor.Bytes name2, Cbor.Nint (-50L))])] in
  let v = Multi_asset.mint_of_cbor cbor in
  Alcotest.(check int64) "lovelace 0" 0L v.lovelace;
  let names = List.assoc pid1 v.assets in
  let qty1 = List.assoc name1 names in
  let qty2 = List.assoc name2 names in
  Alcotest.(check int64) "mint 100" 100L qty1;
  Alcotest.(check int64) "burn -50" (-50L) qty2

(* ================================================================ *)
(* Conservation with mint                                            *)
(* ================================================================ *)

let test_conservation_with_mint () =
  let utxo = Utxo.create () in
  let input_value = Multi_asset.of_lovelace 5000000L in
  Utxo.add utxo Utxo.TxIn.{ tx_hash = Bytes.make 32 '\x01'; tx_index = 0 }
    Utxo.TxOut.{ address = Bytes.make 29 '\x61'; value = input_value;
                 has_datum = false; has_script_ref = false };
  (* Mint 100 tokens, output has 4.8 ADA + 100 tokens *)
  let mint_val = Multi_asset.{ lovelace = 0L;
    assets = [(pid1, [(name1, 100L)])] } in
  let output_value = Multi_asset.{ lovelace = 4800000L;
    assets = [(pid1, [(name1, 100L)])] } in
  let tx : Tx_decoder.decoded_tx = {
    dt_inputs = [{ ti_tx_hash = Bytes.make 32 '\x01'; ti_index = 0L }];
    dt_outputs = [{ to_address = Bytes.make 29 '\x61'; to_value = output_value;
                    to_has_datum = false; to_has_script_ref = false }];
    dt_fee = 200000L; dt_ttl = None; dt_validity_start = None;
    dt_certs = []; dt_withdrawal_total = 0L;
    dt_mint = mint_val;
    dt_collateral_inputs = []; dt_collateral_return = None;
    dt_total_collateral = None; dt_is_valid = true;
    dt_voting_procedures = 0; dt_proposal_count = 0;
    dt_treasury_donation = 0L;
    dt_era = Shelley } in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  let has_cons = List.exists (function
    | Utxo.Value_not_conserved _ -> true | _ -> false) errors in
  Alcotest.(check bool) "mint conservation passes" false has_cons

let test_conservation_with_burn () =
  let utxo = Utxo.create () in
  let input_value = Multi_asset.{ lovelace = 5000000L;
    assets = [(pid1, [(name1, 50L)])] } in
  Utxo.add utxo Utxo.TxIn.{ tx_hash = Bytes.make 32 '\x01'; tx_index = 0 }
    Utxo.TxOut.{ address = Bytes.make 29 '\x61'; value = input_value;
                 has_datum = false; has_script_ref = false };
  (* Burn 50 tokens, output has only lovelace *)
  let burn_val = Multi_asset.{ lovelace = 0L;
    assets = [(pid1, [(name1, -50L)])] } in
  let tx : Tx_decoder.decoded_tx = {
    dt_inputs = [{ ti_tx_hash = Bytes.make 32 '\x01'; ti_index = 0L }];
    dt_outputs = [{ to_address = Bytes.make 29 '\x61';
                    to_value = Multi_asset.of_lovelace 4800000L;
                    to_has_datum = false; to_has_script_ref = false }];
    dt_fee = 200000L; dt_ttl = None; dt_validity_start = None;
    dt_certs = []; dt_withdrawal_total = 0L;
    dt_mint = burn_val;
    dt_collateral_inputs = []; dt_collateral_return = None;
    dt_total_collateral = None; dt_is_valid = true;
    dt_voting_procedures = 0; dt_proposal_count = 0;
    dt_treasury_donation = 0L;
    dt_era = Shelley } in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  let has_cons = List.exists (function
    | Utxo.Value_not_conserved _ -> true | _ -> false) errors in
  Alcotest.(check bool) "burn conservation passes" false has_cons

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Multi-Asset"
    [ ( "Value arithmetic",
        [ Alcotest.test_case "add overlapping" `Quick test_add_values;
          Alcotest.test_case "add different policies" `Quick test_add_different_policies;
          Alcotest.test_case "subtract" `Quick test_subtract ] );
      ( "Lovelace-only",
        [ Alcotest.test_case "lovelace only" `Quick test_lovelace_only;
          Alcotest.test_case "not lovelace only" `Quick test_not_lovelace_only ] );
      ( "Equality",
        [ Alcotest.test_case "same" `Quick test_equal_same;
          Alcotest.test_case "different order" `Quick test_equal_different_order;
          Alcotest.test_case "not equal" `Quick test_not_equal ] );
      ( "Filter zero",
        [ Alcotest.test_case "partial zero" `Quick test_filter_zero;
          Alcotest.test_case "all zero" `Quick test_filter_all_zero ] );
      ( "Positive",
        [ Alcotest.test_case "positive" `Quick test_positive;
          Alcotest.test_case "negative asset" `Quick test_negative_asset ] );
      ( "CBOR parsing",
        [ Alcotest.test_case "lovelace only" `Quick test_cbor_lovelace_only;
          Alcotest.test_case "multi-asset" `Quick test_cbor_multi_asset;
          Alcotest.test_case "mint with burn" `Quick test_cbor_mint ] );
      ( "Conservation",
        [ Alcotest.test_case "mint passes" `Quick test_conservation_with_mint;
          Alcotest.test_case "burn passes" `Quick test_conservation_with_burn ] );
    ]
