open Meridian

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf
let bytes_testable = Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

let check name msg =
  let r1 = Local_tx_monitor.to_bytes msg in
  match Local_tx_monitor.of_bytes r1 with
  | Error e -> Alcotest.fail e
  | Ok msg' -> Alcotest.check bytes_testable name r1 (Local_tx_monitor.to_bytes msg')

let txid () = Bytes.make 32 '\x01'

let test_roundtrips () =
  check "Acquire" MsgAcquire;
  check "Acquired" (MsgAcquired 42000L);
  check "NextTx" MsgNextTx;
  check "ReplyNextTx some" (MsgReplyNextTx (Some (Bytes.make 100 '\xaa')));
  check "ReplyNextTx none" (MsgReplyNextTx None);
  check "HasTx" (MsgHasTx (txid ()));
  check "ReplyHasTx true" (MsgReplyHasTx true);
  check "ReplyHasTx false" (MsgReplyHasTx false);
  check "GetSizes" MsgGetSizes;
  check "ReplyGetSizes"
    (MsgReplyGetSizes { capacity = 1000000L; size = 500000L; num_txs = 42 });
  check "Release" MsgRelease;
  check "Done" MsgDone

let test_tags () =
  let tag msg expected =
    match Local_tx_monitor.encode_message msg with
    | Cbor.Array (Cbor.Uint t :: _) ->
      Alcotest.(check int64) "" expected t
    | _ -> Alcotest.fail "expected [tag, ...]"
  in
  tag MsgAcquire 0L; tag (MsgAcquired 0L) 1L;
  tag MsgNextTx 2L; tag (MsgReplyNextTx None) 3L;
  tag (MsgHasTx (txid ())) 4L; tag (MsgReplyHasTx true) 5L;
  tag MsgGetSizes 6L;
  tag (MsgReplyGetSizes { capacity = 0L; size = 0L; num_txs = 0 }) 7L;
  tag MsgRelease 8L; tag MsgDone 9L

let test_valid_transitions () =
  let ok s m expected =
    match Local_tx_monitor.transition s m with
    | Ok s' -> Alcotest.(check string) "" expected (Local_tx_monitor.state_name s')
    | Error e -> Alcotest.fail e
  in
  ok StIdle MsgAcquire "StAcquiring";
  ok StIdle MsgDone "StDone";
  ok StAcquiring (MsgAcquired 100L) "StAcquired";
  ok StAcquired MsgNextTx "StAcquired";
  ok StAcquired (MsgReplyNextTx None) "StAcquired";
  ok StAcquired (MsgHasTx (txid ())) "StAcquired";
  ok StAcquired (MsgReplyHasTx true) "StAcquired";
  ok StAcquired MsgGetSizes "StAcquired";
  ok StAcquired (MsgReplyGetSizes { capacity = 0L; size = 0L; num_txs = 0 }) "StAcquired";
  ok StAcquired MsgRelease "StIdle"

let test_invalid_transitions () =
  let bad s m =
    match Local_tx_monitor.transition s m with
    | Error _ -> () | Ok _ -> Alcotest.fail "expected error"
  in
  bad StIdle MsgNextTx;
  bad StIdle (MsgAcquired 0L);
  bad StAcquiring MsgNextTx;
  bad StDone MsgAcquire

let test_full_sequence () =
  let ck s m = match Local_tx_monitor.transition s m with
    | Ok s' -> s' | Error e -> Alcotest.fail e
  in
  let s = Local_tx_monitor.StIdle in
  let s = ck s MsgAcquire in
  let s = ck s (MsgAcquired 1000L) in
  Alcotest.(check string) "acquired" "StAcquired" (Local_tx_monitor.state_name s);
  let s = ck s MsgNextTx in
  let s = ck s (MsgReplyNextTx (Some (Bytes.make 50 '\xaa'))) in
  let s = ck s MsgNextTx in
  let s = ck s (MsgReplyNextTx None) in
  let s = ck s (MsgHasTx (txid ())) in
  let s = ck s (MsgReplyHasTx true) in
  let s = ck s MsgGetSizes in
  let s = ck s (MsgReplyGetSizes { capacity = 1000000L; size = 500L; num_txs = 1 }) in
  let s = ck s MsgRelease in
  Alcotest.(check string) "idle" "StIdle" (Local_tx_monitor.state_name s);
  let s = ck s MsgDone in
  Alcotest.(check string) "done" "StDone" (Local_tx_monitor.state_name s)

let test_agency () =
  Alcotest.(check bool) "idle" true (Local_tx_monitor.agency_of StIdle = Client_agency);
  Alcotest.(check bool) "acquiring" true (Local_tx_monitor.agency_of StAcquiring = Server_agency);
  Alcotest.(check bool) "acquired" true (Local_tx_monitor.agency_of StAcquired = Client_agency);
  Alcotest.(check bool) "done" true (Local_tx_monitor.agency_of StDone = Nobody_agency)

let () =
  Alcotest.run "Local-Tx-Monitor"
    [ "Round-trips", [Alcotest.test_case "all messages" `Quick test_roundtrips];
      "CBOR tags", [Alcotest.test_case "all tags" `Quick test_tags];
      "Valid transitions", [Alcotest.test_case "all valid" `Quick test_valid_transitions];
      "Invalid transitions", [Alcotest.test_case "all invalid" `Quick test_invalid_transitions];
      "Full sequence", [Alcotest.test_case "acquire-query-release" `Quick test_full_sequence];
      "Agency", [Alcotest.test_case "all states" `Quick test_agency] ]
