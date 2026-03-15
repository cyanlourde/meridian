open Meridian

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf
let bytes_testable = Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

let check name msg =
  let r1 = Local_tx_submission.to_bytes msg in
  match Local_tx_submission.of_bytes r1 with
  | Error e -> Alcotest.fail e
  | Ok msg' -> Alcotest.check bytes_testable name r1 (Local_tx_submission.to_bytes msg')

let test_roundtrips () =
  check "SubmitTx" (MsgSubmitTx (Bytes.make 200 '\xaa'));
  check "SubmitTx empty" (MsgSubmitTx Bytes.empty);
  check "AcceptTx" MsgAcceptTx;
  check "RejectTx" (MsgRejectTx (Cbor.Array [Cbor.Uint 0L; Cbor.Text "bad tx"]));
  check "Done" MsgDone

let test_tags () =
  let tag msg expected =
    match Local_tx_submission.encode_message msg with
    | Cbor.Array (Cbor.Uint t :: _) ->
      Alcotest.(check int64) "" expected t
    | _ -> Alcotest.fail "expected [tag, ...]"
  in
  tag (MsgSubmitTx Bytes.empty) 0L;
  tag MsgAcceptTx 1L;
  tag (MsgRejectTx Cbor.Null) 2L;
  tag MsgDone 3L

let test_valid_transitions () =
  let ok s m expected =
    match Local_tx_submission.transition s m with
    | Ok s' -> Alcotest.(check string) "" expected (Local_tx_submission.state_name s')
    | Error e -> Alcotest.fail e
  in
  ok StIdle (MsgSubmitTx Bytes.empty) "StBusy";
  ok StIdle MsgDone "StDone";
  ok StBusy MsgAcceptTx "StIdle";
  ok StBusy (MsgRejectTx Cbor.Null) "StIdle"

let test_invalid_transitions () =
  let bad s m =
    match Local_tx_submission.transition s m with
    | Error _ -> () | Ok _ -> Alcotest.fail "expected error"
  in
  bad StIdle MsgAcceptTx;
  bad StBusy (MsgSubmitTx Bytes.empty);
  bad StBusy MsgDone;
  bad StDone (MsgSubmitTx Bytes.empty)

let test_submit_accept_cycle () =
  let ck s m = match Local_tx_submission.transition s m with
    | Ok s' -> s' | Error e -> Alcotest.fail e
  in
  let s = Local_tx_submission.StIdle in
  let s = ck s (MsgSubmitTx (Bytes.make 100 '\xaa')) in
  let s = ck s MsgAcceptTx in
  Alcotest.(check string) "idle" "StIdle" (Local_tx_submission.state_name s);
  let s = ck s (MsgSubmitTx (Bytes.make 50 '\xbb')) in
  let s = ck s (MsgRejectTx (Cbor.Text "insufficient funds")) in
  Alcotest.(check string) "idle after reject" "StIdle" (Local_tx_submission.state_name s);
  let s = ck s MsgDone in
  Alcotest.(check string) "done" "StDone" (Local_tx_submission.state_name s)

let test_agency () =
  Alcotest.(check bool) "idle" true (Local_tx_submission.agency_of StIdle = Client_agency);
  Alcotest.(check bool) "busy" true (Local_tx_submission.agency_of StBusy = Server_agency);
  Alcotest.(check bool) "done" true (Local_tx_submission.agency_of StDone = Nobody_agency)

let () =
  Alcotest.run "Local-Tx-Submission"
    [ "Round-trips", [Alcotest.test_case "all messages" `Quick test_roundtrips];
      "CBOR tags", [Alcotest.test_case "all tags" `Quick test_tags];
      "Valid transitions", [Alcotest.test_case "all valid" `Quick test_valid_transitions];
      "Invalid transitions", [Alcotest.test_case "all invalid" `Quick test_invalid_transitions];
      "Full cycle", [Alcotest.test_case "submit/accept/reject" `Quick test_submit_accept_cycle];
      "Agency", [Alcotest.test_case "all states" `Quick test_agency] ]
