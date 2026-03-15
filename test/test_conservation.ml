open Meridian

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF);
  Bytes.set_uint8 b 1 ((n lsr 8) land 0xFF); b

let make_addr () = Bytes.make 29 '\x61'

let make_input tx_n idx : Tx_decoder.tx_input =
  { ti_tx_hash = make_hash tx_n; ti_index = Int64.of_int idx }

let make_output ?(lovelace = 2000000L) () : Tx_decoder.tx_output =
  { to_address = make_addr (); to_lovelace = lovelace;
    to_has_multi_asset = false; to_has_datum = false;
    to_has_script_ref = false }

let make_txin tx_n idx = Utxo.TxIn.{ tx_hash = make_hash tx_n; tx_index = idx }

let make_txout ?(lovelace = 2000000L) () =
  Utxo.TxOut.{ address = make_addr (); lovelace;
               has_multi_asset = false; has_datum = false;
               has_script_ref = false }

let make_tx ?(fee = 200000L) ?(withdrawal = 0L) ?(certs = [])
    ?(is_valid = true) ?(collateral = []) ?(collateral_return = None)
    ?(total_collateral = None) ?(mint = false)
    inputs outputs : Tx_decoder.decoded_tx =
  { dt_inputs = inputs; dt_outputs = outputs; dt_fee = fee;
    dt_ttl = None; dt_validity_start = None;
    dt_certs = certs; dt_withdrawal_total = withdrawal;
    dt_mint = mint;
    dt_collateral_inputs = collateral;
    dt_collateral_return = collateral_return;
    dt_total_collateral = total_collateral;
    dt_is_valid = is_valid;
    dt_era = Block_decoder.Shelley }

let has_conservation_error errors =
  List.exists (function Utxo.Value_not_conserved _ -> true | _ -> false) errors

let has_conservation_warning errors =
  List.exists (function Utxo.Conservation_warning _ -> true | _ -> false) errors

(* ================================================================ *)
(* Withdrawal tests                                                  *)
(* ================================================================ *)

let test_withdrawal_passes () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  (* consumed = 5000000 + 1000000 (withdrawal) = 6000000
     produced = 5800000 + 200000 (fee) = 6000000 *)
  let tx = make_tx ~fee:200000L ~withdrawal:1000000L
    [make_input 1 0] [make_output ~lovelace:5800000L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  Alcotest.(check bool) "no conservation error" false (has_conservation_error errors)

let test_withdrawal_missing_fails () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  (* Without withdrawal: consumed=5000000, produced=5800000+200000=6000000 → mismatch *)
  let tx = make_tx ~fee:200000L ~withdrawal:0L
    [make_input 1 0] [make_output ~lovelace:5800000L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  Alcotest.(check bool) "conservation error" true (has_conservation_error errors)

(* ================================================================ *)
(* Stake registration deposit                                        *)
(* ================================================================ *)

let test_stake_reg_deposit () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:10000000L ());
  (* consumed = 10000000, produced = 7800000 + 200000 + 2000000 (deposit) = 10000000 *)
  let tx = make_tx ~fee:200000L
    ~certs:[Cert_stake_registration]
    [make_input 1 0] [make_output ~lovelace:7800000L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~key_deposit:2000000L ~utxo ~current_slot:100L tx in
  Alcotest.(check bool) "deposit passes" false (has_conservation_error errors)

(* ================================================================ *)
(* Stake deregistration refund                                       *)
(* ================================================================ *)

let test_stake_dereg_refund () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  (* consumed = 5000000 + 2000000 (refund) = 7000000
     produced = 6800000 + 200000 = 7000000 *)
  let tx = make_tx ~fee:200000L
    ~certs:[Cert_stake_deregistration]
    [make_input 1 0] [make_output ~lovelace:6800000L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~key_deposit:2000000L ~utxo ~current_slot:100L tx in
  Alcotest.(check bool) "refund passes" false (has_conservation_error errors)

(* ================================================================ *)
(* Pool registration deposit                                         *)
(* ================================================================ *)

let test_pool_reg_deposit () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:510000000L ());
  (* consumed = 510000000, produced = 9800000 + 200000 + 500000000 = 510000000 *)
  let tx = make_tx ~fee:200000L
    ~certs:[Cert_pool_registration]
    [make_input 1 0] [make_output ~lovelace:9800000L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~pool_deposit:500000000L ~utxo ~current_slot:100L tx in
  Alcotest.(check bool) "pool deposit passes" false (has_conservation_error errors)

(* ================================================================ *)
(* is_valid=false (failed Plutus)                                    *)
(* ================================================================ *)

let test_invalid_tx_collateral () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());  (* regular *)
  Utxo.add utxo (make_txin 2 0) (make_txout ~lovelace:10000000L ()); (* collateral *)
  let tx = make_tx ~is_valid:false ~fee:300000L
    ~collateral:[make_input 2 0]
    [make_input 1 0] [make_output ~lovelace:4700000L ()] in
  let errors = Utxo.validate_tx ~utxo ~current_slot:100L tx in
  (* Should NOT have "input not in utxo" for the collateral input *)
  let missing = List.exists (function Utxo.Input_not_in_utxo _ -> true | _ -> false) errors in
  Alcotest.(check bool) "collateral exists" false missing

let test_invalid_tx_apply () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  Utxo.add utxo (make_txin 2 0) (make_txout ~lovelace:10000000L ());
  let tx = make_tx ~is_valid:false
    ~collateral:[make_input 2 0]
    [make_input 1 0] [make_output ~lovelace:4000000L ()] in
  Utxo.apply_tx utxo ~tx_hash:(make_hash 3) tx;
  (* Regular input NOT consumed, collateral IS consumed *)
  Alcotest.(check bool) "regular still there" true (Utxo.mem utxo (make_txin 1 0));
  Alcotest.(check bool) "collateral consumed" false (Utxo.mem utxo (make_txin 2 0))

let test_invalid_tx_collateral_return () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 2 0) (make_txout ~lovelace:10000000L ());
  let ret = make_output ~lovelace:9700000L () in
  let tx = make_tx ~is_valid:false
    ~collateral:[make_input 2 0]
    ~collateral_return:(Some ret)
    ~total_collateral:(Some 300000L)
    [] [] in
  Utxo.apply_tx utxo ~tx_hash:(make_hash 3) tx;
  Alcotest.(check bool) "collateral consumed" false (Utxo.mem utxo (make_txin 2 0));
  Alcotest.(check bool) "return produced" true (Utxo.mem utxo (make_txin 3 0))

(* ================================================================ *)
(* Multi-asset warning (not error)                                   *)
(* ================================================================ *)

let test_mint_warning () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  (* With mint: conservation is approximate *)
  let tx = make_tx ~fee:200000L ~mint:true
    [make_input 1 0] [make_output ~lovelace:3000000L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  let has_warn = has_conservation_warning errors in
  let has_err = has_conservation_error errors in
  Alcotest.(check bool) "warning not error" true has_warn;
  Alcotest.(check bool) "no conservation error" false has_err

(* ================================================================ *)
(* Combined: withdrawal + deposit                                    *)
(* ================================================================ *)

let test_combined_withdrawal_deposit () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:10000000L ());
  (* consumed = 10000000 + 3000000 (withdrawal) + 0 (no refund) = 13000000
     produced = 10800000 + 200000 + 2000000 (deposit) = 13000000 *)
  let tx = make_tx ~fee:200000L ~withdrawal:3000000L
    ~certs:[Cert_stake_registration]
    [make_input 1 0] [make_output ~lovelace:10800000L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~key_deposit:2000000L ~utxo ~current_slot:100L tx in
  Alcotest.(check bool) "combined passes" false (has_conservation_error errors)

(* ================================================================ *)
(* is_warning function                                               *)
(* ================================================================ *)

let test_is_warning () =
  Alcotest.(check bool) "conservation warning" true
    (Utxo.is_warning (Conservation_warning "test"));
  Alcotest.(check bool) "not warning" false
    (Utxo.is_warning (Value_not_conserved { consumed = 0L; produced = 0L }));
  Alcotest.(check bool) "not warning" false
    (Utxo.is_warning Empty_inputs)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Conservation"
    [ ( "Withdrawals",
        [ Alcotest.test_case "passes" `Quick test_withdrawal_passes;
          Alcotest.test_case "missing fails" `Quick test_withdrawal_missing_fails ] );
      ( "Stake deposits",
        [ Alcotest.test_case "registration" `Quick test_stake_reg_deposit;
          Alcotest.test_case "deregistration refund" `Quick test_stake_dereg_refund ] );
      ( "Pool deposits",
        [ Alcotest.test_case "registration" `Quick test_pool_reg_deposit ] );
      ( "Failed Plutus (is_valid=false)",
        [ Alcotest.test_case "collateral exists" `Quick test_invalid_tx_collateral;
          Alcotest.test_case "apply collateral" `Quick test_invalid_tx_apply;
          Alcotest.test_case "collateral return" `Quick test_invalid_tx_collateral_return ] );
      ( "Multi-asset",
        [ Alcotest.test_case "mint warning" `Quick test_mint_warning ] );
      ( "Combined",
        [ Alcotest.test_case "withdrawal + deposit" `Quick test_combined_withdrawal_deposit ] );
      ( "Warning predicate",
        [ Alcotest.test_case "is_warning" `Quick test_is_warning ] );
    ]
