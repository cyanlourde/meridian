open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF);
  Bytes.set_uint8 b 1 ((n lsr 8) land 0xFF); b

let make_addr () = Bytes.make 29 '\x61'

let make_txin tx_n idx =
  Utxo.TxIn.{ tx_hash = make_hash tx_n; tx_index = idx }

let make_txout ?(lovelace = 2000000L) () =
  Utxo.TxOut.{ address = make_addr (); lovelace;
               has_multi_asset = false; has_datum = false;
               has_script_ref = false }

let make_input tx_n idx : Tx_decoder.tx_input =
  { ti_tx_hash = make_hash tx_n; ti_index = Int64.of_int idx }

let make_output ?(lovelace = 2000000L) () : Tx_decoder.tx_output =
  { to_address = make_addr (); to_lovelace = lovelace;
    to_has_multi_asset = false; to_has_datum = false;
    to_has_script_ref = false }

let make_tx ?(fee = 200000L) ?(ttl = None) inputs outputs : Tx_decoder.decoded_tx =
  { dt_inputs = inputs; dt_outputs = outputs; dt_fee = fee;
    dt_ttl = ttl; dt_validity_start = None;
    dt_certs = []; dt_withdrawal_total = 0L;
    dt_mint = false; dt_collateral_inputs = [];
    dt_collateral_return = None; dt_total_collateral = None;
    dt_is_valid = true;
    dt_era = Block_decoder.Shelley }

(* ================================================================ *)
(* UTxO set basic operations                                         *)
(* ================================================================ *)

let test_empty_set () =
  let utxo = Utxo.create () in
  Alcotest.(check int) "empty size" 0 (Utxo.size utxo);
  Alcotest.(check int64) "empty lovelace" 0L (Utxo.total_lovelace utxo);
  Alcotest.(check bool) "not found" false
    (Utxo.mem utxo (make_txin 1 0))

let test_add_and_find () =
  let utxo = Utxo.create () in
  let txin = make_txin 1 0 in
  let txout = make_txout ~lovelace:5000000L () in
  Utxo.add utxo txin txout;
  Alcotest.(check int) "size 1" 1 (Utxo.size utxo);
  Alcotest.(check bool) "found" true (Utxo.mem utxo txin);
  (match Utxo.find utxo txin with
   | Some out -> Alcotest.(check int64) "lovelace" 5000000L out.lovelace
   | None -> Alcotest.fail "expected to find output")

let test_remove () =
  let utxo = Utxo.create () in
  let txin = make_txin 1 0 in
  Utxo.add utxo txin (make_txout ());
  Alcotest.(check int) "size 1" 1 (Utxo.size utxo);
  Utxo.remove utxo txin;
  Alcotest.(check int) "size 0" 0 (Utxo.size utxo);
  Alcotest.(check bool) "gone" false (Utxo.mem utxo txin)

let test_total_lovelace () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:3000000L ());
  Utxo.add utxo (make_txin 1 1) (make_txout ~lovelace:2000000L ());
  Utxo.add utxo (make_txin 2 0) (make_txout ~lovelace:5000000L ());
  Alcotest.(check int64) "total" 10000000L (Utxo.total_lovelace utxo)

let test_multiple_outputs_same_tx () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:1000000L ());
  Utxo.add utxo (make_txin 1 1) (make_txout ~lovelace:2000000L ());
  Utxo.add utxo (make_txin 1 2) (make_txout ~lovelace:3000000L ());
  Alcotest.(check int) "3 entries" 3 (Utxo.size utxo);
  Alcotest.(check int64) "total" 6000000L (Utxo.total_lovelace utxo)

(* ================================================================ *)
(* Transaction validation                                            *)
(* ================================================================ *)

let test_valid_tx () =
  let utxo = Utxo.create () in
  (* Seed UTxO with one output worth 5 ADA *)
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  (* Transaction: consume that output, produce 2 outputs + fee *)
  let tx = make_tx ~fee:200000L
    [make_input 1 0]
    [make_output ~lovelace:3000000L ();
     make_output ~lovelace:1800000L ()] in
  (* consumed=5000000, produced=3000000+1800000+200000=5000000 *)
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  Alcotest.(check int) "no errors" 0 (List.length errors)

let test_input_not_in_utxo () =
  let utxo = Utxo.create () in
  let tx = make_tx [make_input 99 0] [make_output ()] in
  let errors = Utxo.validate_tx ~utxo ~current_slot:100L tx in
  let has_missing = List.exists (function
    | Utxo.Input_not_in_utxo _ -> true | _ -> false) errors in
  Alcotest.(check bool) "missing input" true has_missing

let test_duplicate_input () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  let tx = make_tx
    [make_input 1 0; make_input 1 0]
    [make_output ~lovelace:4800000L ()] in
  let errors = Utxo.validate_tx ~utxo ~current_slot:100L tx in
  let has_dup = List.exists (function
    | Utxo.Duplicate_input _ -> true | _ -> false) errors in
  Alcotest.(check bool) "duplicate" true has_dup

let test_insufficient_fee () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  let tx = make_tx ~fee:100L
    [make_input 1 0] [make_output ~lovelace:4999900L ()] in
  let errors = Utxo.validate_tx ~min_fee_a:44L ~min_fee_b:155381L
    ~utxo ~current_slot:100L tx in
  let has_fee = List.exists (function
    | Utxo.Insufficient_fee _ -> true | _ -> false) errors in
  Alcotest.(check bool) "insufficient fee" true has_fee

let test_output_too_small () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  let tx = make_tx ~fee:200000L
    [make_input 1 0]
    [make_output ~lovelace:500L (); make_output ~lovelace:4799500L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  let has_small = List.exists (function
    | Utxo.Output_too_small _ -> true | _ -> false) errors in
  Alcotest.(check bool) "output too small" true has_small

let test_expired_ttl () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  let tx = make_tx ~fee:200000L ~ttl:(Some 50L)
    [make_input 1 0] [make_output ~lovelace:4800000L ()] in
  let errors = Utxo.validate_tx ~utxo ~current_slot:100L tx in
  let has_ttl = List.exists (function
    | Utxo.Expired_ttl _ -> true | _ -> false) errors in
  Alcotest.(check bool) "TTL expired" true has_ttl

let test_ttl_not_expired () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  let tx = make_tx ~fee:200000L ~ttl:(Some 200L)
    [make_input 1 0] [make_output ~lovelace:4800000L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  let has_ttl = List.exists (function
    | Utxo.Expired_ttl _ -> true | _ -> false) errors in
  Alcotest.(check bool) "TTL ok" false has_ttl

let test_value_not_conserved () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  (* Outputs + fee = 3000000 + 200000 = 3200000, but input = 5000000 *)
  let tx = make_tx ~fee:200000L
    [make_input 1 0] [make_output ~lovelace:3000000L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  let has_conservation = List.exists (function
    | Utxo.Value_not_conserved _ -> true | _ -> false) errors in
  Alcotest.(check bool) "value not conserved" true has_conservation

let test_empty_inputs () =
  let utxo = Utxo.create () in
  let tx = make_tx [] [make_output ()] in
  let errors = Utxo.validate_tx ~utxo ~current_slot:100L tx in
  let has_empty = List.exists (function
    | Utxo.Empty_inputs -> true | _ -> false) errors in
  Alcotest.(check bool) "empty inputs" true has_empty

let test_empty_outputs () =
  let utxo = Utxo.create () in
  Utxo.add utxo (make_txin 1 0) (make_txout ());
  let tx = make_tx [make_input 1 0] [] in
  let errors = Utxo.validate_tx ~utxo ~current_slot:100L tx in
  let has_empty = List.exists (function
    | Utxo.Empty_outputs -> true | _ -> false) errors in
  Alcotest.(check bool) "empty outputs" true has_empty

(* ================================================================ *)
(* UTxO transition (apply_tx)                                        *)
(* ================================================================ *)

let test_apply_tx () =
  let utxo = Utxo.create () in
  (* Seed: tx1#0 = 5 ADA *)
  Utxo.add utxo (make_txin 1 0) (make_txout ~lovelace:5000000L ());
  Alcotest.(check int) "initial size" 1 (Utxo.size utxo);
  (* Apply tx: consume tx1#0, produce tx2#0 and tx2#1 *)
  let tx = make_tx ~fee:200000L
    [make_input 1 0]
    [make_output ~lovelace:3000000L (); make_output ~lovelace:1800000L ()] in
  Utxo.apply_tx utxo ~tx_hash:(make_hash 2) tx;
  (* tx1#0 should be gone, tx2#0 and tx2#1 should exist *)
  Alcotest.(check bool) "tx1#0 consumed" false (Utxo.mem utxo (make_txin 1 0));
  Alcotest.(check bool) "tx2#0 exists" true (Utxo.mem utxo (make_txin 2 0));
  Alcotest.(check bool) "tx2#1 exists" true (Utxo.mem utxo (make_txin 2 1));
  Alcotest.(check int) "size 2" 2 (Utxo.size utxo);
  Alcotest.(check int64) "total" 4800000L (Utxo.total_lovelace utxo)

let test_apply_chain () =
  (* Simulate a chain of 3 transactions *)
  let utxo = Utxo.create () in
  (* Genesis output: tx0#0 = 10 ADA *)
  Utxo.add utxo (make_txin 0 0) (make_txout ~lovelace:10000000L ());
  (* Tx 1: spend tx0#0, produce tx1#0=6ADA, tx1#1=3.8ADA, fee=0.2ADA *)
  let tx1 = make_tx ~fee:200000L
    [make_input 0 0]
    [make_output ~lovelace:6000000L (); make_output ~lovelace:3800000L ()] in
  Utxo.apply_tx utxo ~tx_hash:(make_hash 1) tx1;
  Alcotest.(check int) "after tx1" 2 (Utxo.size utxo);
  (* Tx 2: spend tx1#0, produce tx2#0=5.8ADA, fee=0.2ADA *)
  let tx2 = make_tx ~fee:200000L
    [make_input 1 0]
    [make_output ~lovelace:5800000L ()] in
  Utxo.apply_tx utxo ~tx_hash:(make_hash 2) tx2;
  Alcotest.(check int) "after tx2" 2 (Utxo.size utxo);  (* tx1#1 + tx2#0 *)
  (* Tx 3: spend both remaining *)
  let tx3 = make_tx ~fee:200000L
    [make_input 1 1; make_input 2 0]
    [make_output ~lovelace:9400000L ()] in
  Utxo.apply_tx utxo ~tx_hash:(make_hash 3) tx3;
  Alcotest.(check int) "after tx3" 1 (Utxo.size utxo);
  Alcotest.(check int64) "final" 9400000L (Utxo.total_lovelace utxo)

(* ================================================================ *)
(* Multiple errors collected                                         *)
(* ================================================================ *)

let test_multiple_errors () =
  let utxo = Utxo.create () in
  (* Tx with missing input, tiny output, expired TTL, tiny fee *)
  let tx = make_tx ~fee:10L ~ttl:(Some 5L)
    [make_input 99 0]
    [make_output ~lovelace:100L ()] in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  Alcotest.(check bool) "multiple errors" true (List.length errors >= 3)

(* ================================================================ *)
(* TxIn comparison and equality                                      *)
(* ================================================================ *)

let test_txin_compare () =
  let a = make_txin 1 0 in
  let b = make_txin 1 1 in
  let c = make_txin 2 0 in
  Alcotest.(check bool) "a < b" true (Utxo.TxIn.compare a b < 0);
  Alcotest.(check bool) "a < c" true (Utxo.TxIn.compare a c < 0);
  Alcotest.(check bool) "a = a" true (Utxo.TxIn.equal a a);
  Alcotest.(check bool) "a != b" false (Utxo.TxIn.equal a b)

let test_error_to_string () =
  let s = Utxo.error_to_string (Input_not_in_utxo (make_txin 1 0)) in
  Alcotest.(check bool) "non-empty" true (String.length s > 0);
  let s = Utxo.error_to_string (Insufficient_fee { required = 200000L; actual = 100L }) in
  Alcotest.(check bool) "non-empty" true (String.length s > 0)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "UTxO"
    [ ( "UTxO set",
        [ Alcotest.test_case "empty" `Quick test_empty_set;
          Alcotest.test_case "add and find" `Quick test_add_and_find;
          Alcotest.test_case "remove" `Quick test_remove;
          Alcotest.test_case "total lovelace" `Quick test_total_lovelace;
          Alcotest.test_case "multi-output tx" `Quick test_multiple_outputs_same_tx ] );
      ( "Validation",
        [ Alcotest.test_case "valid tx" `Quick test_valid_tx;
          Alcotest.test_case "missing input" `Quick test_input_not_in_utxo;
          Alcotest.test_case "duplicate input" `Quick test_duplicate_input;
          Alcotest.test_case "insufficient fee" `Quick test_insufficient_fee;
          Alcotest.test_case "output too small" `Quick test_output_too_small;
          Alcotest.test_case "expired TTL" `Quick test_expired_ttl;
          Alcotest.test_case "TTL ok" `Quick test_ttl_not_expired;
          Alcotest.test_case "value not conserved" `Quick test_value_not_conserved;
          Alcotest.test_case "empty inputs" `Quick test_empty_inputs;
          Alcotest.test_case "empty outputs" `Quick test_empty_outputs;
          Alcotest.test_case "multiple errors" `Quick test_multiple_errors ] );
      ( "UTxO transition",
        [ Alcotest.test_case "apply single tx" `Quick test_apply_tx;
          Alcotest.test_case "apply chain" `Quick test_apply_chain ] );
      ( "TxIn",
        [ Alcotest.test_case "compare/equal" `Quick test_txin_compare;
          Alcotest.test_case "error strings" `Quick test_error_to_string ] );
    ]
