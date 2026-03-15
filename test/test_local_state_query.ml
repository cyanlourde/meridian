open Meridian

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf
let bytes_testable = Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

let check name msg =
  let r1 = Local_state_query.to_bytes msg in
  match Local_state_query.of_bytes r1 with
  | Error e -> Alcotest.fail e
  | Ok msg' -> Alcotest.check bytes_testable name r1 (Local_state_query.to_bytes msg')

let pt = Chain_sync.Point (100L, Bytes.make 32 '\xab')

let test_roundtrips () =
  check "Acquire point" (MsgAcquire (Some pt));
  check "Acquire tip" (MsgAcquire None);
  check "Acquired" MsgAcquired;
  check "Failure too old" (MsgFailure AcquireFailurePointTooOld);
  check "Failure not on chain" (MsgFailure AcquireFailurePointNotOnChain);
  check "Query" (MsgQuery (Cbor.Array [Cbor.Uint 0L; Cbor.Uint 1L]));
  check "Result" (MsgResult (Cbor.Text "some result"));
  check "Release" MsgRelease;
  check "ReAcquire point" (MsgReAcquire (Some pt));
  check "ReAcquire tip" (MsgReAcquire None);
  check "Done" MsgDone

let test_tags () =
  let tag msg expected =
    match Local_state_query.encode_message msg with
    | Cbor.Array (Cbor.Uint t :: _) ->
      Alcotest.(check int64) (Printf.sprintf "tag %Ld" expected) expected t
    | _ -> Alcotest.fail "expected [tag, ...]"
  in
  tag (MsgAcquire None) 0L; tag MsgAcquired 1L;
  tag (MsgFailure AcquireFailurePointTooOld) 2L;
  tag (MsgQuery Cbor.Null) 3L; tag (MsgResult Cbor.Null) 4L;
  tag MsgRelease 5L; tag (MsgReAcquire None) 6L; tag MsgDone 7L

let test_valid_transitions () =
  let ok s m expected =
    match Local_state_query.transition s m with
    | Ok s' -> Alcotest.(check string) "" expected (Local_state_query.state_name s')
    | Error e -> Alcotest.fail e
  in
  ok StIdle (MsgAcquire (Some pt)) "StAcquiring";
  ok StIdle MsgDone "StDone";
  ok StAcquiring MsgAcquired "StAcquired";
  ok StAcquiring (MsgFailure AcquireFailurePointTooOld) "StIdle";
  ok StAcquired (MsgQuery Cbor.Null) "StQuerying";
  ok StAcquired (MsgReAcquire None) "StAcquiring";
  ok StAcquired MsgRelease "StIdle";
  ok StQuerying (MsgResult Cbor.Null) "StAcquired"

let test_invalid_transitions () =
  let bad s m =
    match Local_state_query.transition s m with
    | Error _ -> () | Ok _ -> Alcotest.fail "expected error"
  in
  bad StIdle MsgAcquired;
  bad StAcquiring (MsgQuery Cbor.Null);
  bad StAcquired MsgAcquired;
  bad StQuerying MsgRelease;
  bad StDone (MsgAcquire None)

let test_full_sequence () =
  let ck s m = match Local_state_query.transition s m with
    | Ok s' -> s' | Error e -> Alcotest.fail e
  in
  let s = Local_state_query.StIdle in
  let s = ck s (MsgAcquire (Some pt)) in
  let s = ck s MsgAcquired in
  let s = ck s (MsgQuery (Cbor.Uint 42L)) in
  let s = ck s (MsgResult (Cbor.Text "ok")) in
  Alcotest.(check string) "acquired" "StAcquired" (Local_state_query.state_name s);
  let s = ck s (MsgReAcquire None) in
  let s = ck s MsgAcquired in
  let s = ck s MsgRelease in
  Alcotest.(check string) "idle" "StIdle" (Local_state_query.state_name s);
  let s = ck s MsgDone in
  Alcotest.(check string) "done" "StDone" (Local_state_query.state_name s)

let test_agency () =
  Alcotest.(check bool) "idle" true (Local_state_query.agency_of StIdle = Client_agency);
  Alcotest.(check bool) "acquiring" true (Local_state_query.agency_of StAcquiring = Server_agency);
  Alcotest.(check bool) "acquired" true (Local_state_query.agency_of StAcquired = Client_agency);
  Alcotest.(check bool) "querying" true (Local_state_query.agency_of StQuerying = Server_agency);
  Alcotest.(check bool) "done" true (Local_state_query.agency_of StDone = Nobody_agency)

let () =
  Alcotest.run "Local-State-Query"
    [ "Round-trips", [Alcotest.test_case "all messages" `Quick test_roundtrips];
      "CBOR tags", [Alcotest.test_case "all tags" `Quick test_tags];
      "Valid transitions", [Alcotest.test_case "all valid" `Quick test_valid_transitions];
      "Invalid transitions", [Alcotest.test_case "all invalid" `Quick test_invalid_transitions];
      "Full sequence", [Alcotest.test_case "acquire-query-release" `Quick test_full_sequence];
      "Agency", [Alcotest.test_case "all states" `Quick test_agency] ]
