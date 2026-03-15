open Meridian

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF); b

let make_addr () = Bytes.make 29 '\x61'

let cred1 = Governance.Key_drep (Bytes.make 28 '\x01')
let cred2 = Governance.Key_drep (Bytes.make 28 '\x02')

(* ================================================================ *)
(* DRep lifecycle                                                    *)
(* ================================================================ *)

let test_drep_register () =
  let gov = Governance.create () in
  Governance.register_drep gov ~credential:cred1 ~deposit:500000000L
    ~anchor:None ~expiry_epoch:100L;
  Alcotest.(check int) "1 drep" 1 (Governance.drep_count gov);
  Alcotest.(check bool) "registered" true (Governance.is_drep_registered gov ~credential:cred1)

let test_drep_deregister () =
  let gov = Governance.create () in
  Governance.register_drep gov ~credential:cred1 ~deposit:500000000L
    ~anchor:None ~expiry_epoch:100L;
  let refund = Governance.deregister_drep gov ~credential:cred1 in
  Alcotest.(check int64) "refund" 500000000L refund;
  Alcotest.(check int) "0 dreps" 0 (Governance.drep_count gov);
  Alcotest.(check bool) "gone" false (Governance.is_drep_registered gov ~credential:cred1)

let test_drep_update () =
  let gov = Governance.create () in
  Governance.register_drep gov ~credential:cred1 ~deposit:500000000L
    ~anchor:None ~expiry_epoch:100L;
  Governance.update_drep gov ~credential:cred1
    ~anchor:(Some ("https://example.com", make_hash 42));
  match Governance.get_drep gov ~credential:cred1 with
  | Some ds -> Alcotest.(check bool) "has anchor" true (ds.ds_anchor <> None)
  | None -> Alcotest.fail "drep not found"

(* ================================================================ *)
(* Proposals and voting                                              *)
(* ================================================================ *)

let test_submit_proposal () =
  let gov = Governance.create () in
  let ga_id = Governance.{ ga_tx_hash = make_hash 1; ga_index = 0 } in
  Governance.submit_proposal gov ~action_id:ga_id
    ~action:GA_InfoAction ~deposit:100000000000L
    ~return_addr:(make_addr ()) ~epoch:10L;
  Alcotest.(check int) "1 proposal" 1 (Governance.proposal_count gov)

let test_cast_votes () =
  let gov = Governance.create () in
  let ga_id = Governance.{ ga_tx_hash = make_hash 1; ga_index = 0 } in
  Governance.submit_proposal gov ~action_id:ga_id
    ~action:GA_NoConfidence ~deposit:100000000000L
    ~return_addr:(make_addr ()) ~epoch:10L;
  Governance.cast_vote gov ~voter:(Voter_cc cred1)
    ~action_id:ga_id ~vote:Vote_yes;
  Governance.cast_vote gov ~voter:(Voter_drep cred2)
    ~action_id:ga_id ~vote:Vote_yes;
  Governance.cast_vote gov ~voter:(Voter_spo (Bytes.make 28 '\x03'))
    ~action_id:ga_id ~vote:Vote_no;
  Alcotest.(check int) "3 votes" 3 (Governance.vote_count gov)

(* ================================================================ *)
(* Expiry                                                            *)
(* ================================================================ *)

let test_proposal_expiry () =
  let gov = Governance.create () in
  let ga_id = Governance.{ ga_tx_hash = make_hash 1; ga_index = 0 } in
  Governance.submit_proposal gov ~action_id:ga_id
    ~action:GA_InfoAction ~deposit:100000000000L
    ~return_addr:(make_addr ()) ~epoch:10L;
  let expired = Governance.expire_proposals gov ~current_epoch:20L ~max_lifetime:6 in
  Alcotest.(check int) "1 expired" 1 expired;
  Alcotest.(check int) "0 proposals" 0 (Governance.proposal_count gov)

let test_drep_expiry () =
  let gov = Governance.create () in
  Governance.register_drep gov ~credential:cred1 ~deposit:500000000L
    ~anchor:None ~expiry_epoch:15L;
  let expired = Governance.expire_dreps gov ~current_epoch:20L in
  Alcotest.(check int) "1 expired" 1 expired;
  Alcotest.(check int) "0 dreps" 0 (Governance.drep_count gov)

(* ================================================================ *)
(* Treasury                                                          *)
(* ================================================================ *)

let test_treasury () =
  let gov = Governance.create () in
  Alcotest.(check int64) "initial" 0L (Governance.treasury gov);
  Governance.add_treasury gov ~amount:5000000L;
  Alcotest.(check int64) "after add" 5000000L (Governance.treasury gov)

(* ================================================================ *)
(* Empty state                                                       *)
(* ================================================================ *)

let test_empty () =
  let gov = Governance.create () in
  Alcotest.(check int) "0 dreps" 0 (Governance.drep_count gov);
  Alcotest.(check int) "0 proposals" 0 (Governance.proposal_count gov);
  Alcotest.(check int) "0 votes" 0 (Governance.vote_count gov);
  Alcotest.(check int64) "0 treasury" 0L (Governance.treasury gov)

(* ================================================================ *)
(* Conway cert decoding                                              *)
(* ================================================================ *)

let test_decode_conway_certs () =
  (* DRep registration cert: [16, credential, deposit, anchor/null] *)
  let cert16 = Cbor.Array [Cbor.Uint 16L;
    Cbor.Array [Cbor.Uint 0L; Cbor.Bytes (Bytes.make 28 '\x01')];
    Cbor.Uint 500000000L; Cbor.Null] in
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 1000000L]]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
    (Cbor.Uint 4L, Cbor.Array [cert16]);
  ] in
  match Tx_decoder.decode_transaction ~era:Conway tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    Alcotest.(check int) "1 cert" 1 (List.length tx.dt_certs);
    (match List.hd tx.dt_certs with
     | Cert_drep_registration deposit ->
       Alcotest.(check int64) "deposit" 500000000L deposit
     | _ -> Alcotest.fail "expected drep registration")

let test_decode_voting_procedures () =
  (* Key 19: voting_procedures = { voter => { action_id => [vote, anchor] } } *)
  let voter = Cbor.Array [Cbor.Uint 2L; Cbor.Bytes (Bytes.make 28 '\x01')] in
  let action_id = Cbor.Array [Cbor.Bytes (make_hash 5); Cbor.Uint 0L] in
  let vote = Cbor.Array [Cbor.Uint 1L; Cbor.Null] in  (* yes, no anchor *)
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 1000000L]]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
    (Cbor.Uint 19L, Cbor.Map [(voter, Cbor.Map [(action_id, vote)])]);
  ] in
  match Tx_decoder.decode_transaction ~era:Conway tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    Alcotest.(check int) "1 vote" 1 tx.dt_voting_procedures

let test_decode_treasury_donation () =
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 1000000L]]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
    (Cbor.Uint 22L, Cbor.Uint 5000000L);  (* treasury donation *)
  ] in
  match Tx_decoder.decode_transaction ~era:Conway tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    Alcotest.(check int64) "donation" 5000000L tx.dt_treasury_donation

(* ================================================================ *)
(* Conservation with Conway deposits                                 *)
(* ================================================================ *)

let test_conservation_drep_deposit () =
  let utxo = Utxo.create () in
  Utxo.add utxo Utxo.TxIn.{ tx_hash = make_hash 1; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 600000000L;
                 has_datum = false; has_script_ref = false };
  (* Tx: 600M input = 99.8M output + 0.2M fee + 500M drep deposit *)
  let tx : Tx_decoder.decoded_tx = {
    dt_inputs = [{ ti_tx_hash = make_hash 1; ti_index = 0L }];
    dt_outputs = [{ to_address = make_addr ();
                    to_value = Multi_asset.of_lovelace 99800000L;
                    to_has_datum = false; to_has_script_ref = false }];
    dt_fee = 200000L; dt_ttl = None; dt_validity_start = None;
    dt_certs = [Cert_drep_registration 500000000L];
    dt_withdrawal_total = 0L; dt_mint = Multi_asset.zero;
    dt_collateral_inputs = []; dt_collateral_return = None;
    dt_total_collateral = None; dt_is_valid = true;
    dt_era = Conway;
    dt_voting_procedures = 0; dt_proposal_count = 0;
    dt_treasury_donation = 0L;
  } in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  let has_cons = List.exists (function
    | Utxo.Value_not_conserved _ -> true | _ -> false) errors in
  Alcotest.(check bool) "drep deposit passes" false has_cons

let test_conservation_treasury_donation () =
  let utxo = Utxo.create () in
  Utxo.add utxo Utxo.TxIn.{ tx_hash = make_hash 1; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 10000000L;
                 has_datum = false; has_script_ref = false };
  (* 10M = 4.8M output + 0.2M fee + 5M donation *)
  let tx : Tx_decoder.decoded_tx = {
    dt_inputs = [{ ti_tx_hash = make_hash 1; ti_index = 0L }];
    dt_outputs = [{ to_address = make_addr ();
                    to_value = Multi_asset.of_lovelace 4800000L;
                    to_has_datum = false; to_has_script_ref = false }];
    dt_fee = 200000L; dt_ttl = None; dt_validity_start = None;
    dt_certs = []; dt_withdrawal_total = 0L; dt_mint = Multi_asset.zero;
    dt_collateral_inputs = []; dt_collateral_return = None;
    dt_total_collateral = None; dt_is_valid = true;
    dt_era = Conway;
    dt_voting_procedures = 0; dt_proposal_count = 0;
    dt_treasury_donation = 5000000L;
  } in
  let errors = Utxo.validate_tx ~min_utxo_value:1000000L
    ~utxo ~current_slot:100L tx in
  let has_cons = List.exists (function
    | Utxo.Value_not_conserved _ -> true | _ -> false) errors in
  Alcotest.(check bool) "donation passes" false has_cons

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Governance"
    [ ( "DRep lifecycle",
        [ Alcotest.test_case "register" `Quick test_drep_register;
          Alcotest.test_case "deregister" `Quick test_drep_deregister;
          Alcotest.test_case "update" `Quick test_drep_update ] );
      ( "Proposals",
        [ Alcotest.test_case "submit" `Quick test_submit_proposal;
          Alcotest.test_case "vote" `Quick test_cast_votes ] );
      ( "Expiry",
        [ Alcotest.test_case "proposal" `Quick test_proposal_expiry;
          Alcotest.test_case "drep" `Quick test_drep_expiry ] );
      ( "Treasury",
        [ Alcotest.test_case "add" `Quick test_treasury ] );
      ( "Empty",
        [ Alcotest.test_case "defaults" `Quick test_empty ] );
      ( "CBOR decoding",
        [ Alcotest.test_case "drep cert" `Quick test_decode_conway_certs;
          Alcotest.test_case "voting" `Quick test_decode_voting_procedures;
          Alcotest.test_case "treasury donation" `Quick test_decode_treasury_donation ] );
      ( "Conservation",
        [ Alcotest.test_case "drep deposit" `Quick test_conservation_drep_deposit;
          Alcotest.test_case "treasury donation" `Quick test_conservation_treasury_donation ] );
    ]
