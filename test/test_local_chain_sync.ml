open Meridian

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf
let bytes_testable = Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

let check name msg =
  let r1 = Local_chain_sync.to_bytes msg in
  match Local_chain_sync.of_bytes r1 with
  | Error e -> Alcotest.fail e
  | Ok msg' -> Alcotest.check bytes_testable name r1 (Local_chain_sync.to_bytes msg')

let pt = Chain_sync.Point (100L, Bytes.make 32 '\xab')
let tip = Chain_sync.{ tip_point = Point (200L, Bytes.make 32 '\xcd'); tip_block_number = 50L }

let test_roundtrips () =
  check "RequestNext" MsgRequestNext;
  check "AwaitReply" MsgAwaitReply;
  check "RollForward" (MsgRollForward (Bytes.make 100 '\xee', tip));
  check "RollForward empty" (MsgRollForward (Bytes.empty, tip));
  check "RollBackward" (MsgRollBackward (pt, tip));
  check "RollBackward origin" (MsgRollBackward (Origin, tip));
  check "FindIntersect" (MsgFindIntersect [pt; Origin]);
  check "FindIntersect empty" (MsgFindIntersect []);
  check "IntersectFound" (MsgIntersectFound (pt, tip));
  check "IntersectNotFound" (MsgIntersectNotFound tip);
  check "Done" MsgDone

let test_roll_forward_carries_block () =
  let block = Bytes.make 500 '\xdd' in
  let cbor = Local_chain_sync.encode_message (MsgRollForward (block, tip)) in
  match cbor with
  | Cbor.Array [Cbor.Uint 2L; Cbor.Bytes b; _] ->
    Alcotest.(check int) "block bytes length" 500 (Bytes.length b)
  | _ -> Alcotest.fail "expected [2, bytes, tip]"

let test_valid_transitions () =
  let ok s m expected =
    match Local_chain_sync.transition s m with
    | Ok s' -> Alcotest.(check string) "" expected (Local_chain_sync.state_name s')
    | Error e -> Alcotest.fail e
  in
  ok StIdle MsgRequestNext "StNext";
  ok StIdle (MsgFindIntersect []) "StIntersect";
  ok StIdle MsgDone "StDone";
  ok StNext MsgAwaitReply "StNext";
  ok StNext (MsgRollForward (Bytes.empty, tip)) "StIdle";
  ok StNext (MsgRollBackward (pt, tip)) "StIdle";
  ok StIntersect (MsgIntersectFound (pt, tip)) "StIdle";
  ok StIntersect (MsgIntersectNotFound tip) "StIdle"

let test_invalid_transitions () =
  let bad s m =
    match Local_chain_sync.transition s m with
    | Error _ -> () | Ok _ -> Alcotest.fail "expected error"
  in
  bad StNext MsgRequestNext;
  bad StIdle (MsgRollForward (Bytes.empty, tip));
  bad StDone MsgRequestNext;
  bad StIntersect (MsgFindIntersect [])

let test_agency () =
  Alcotest.(check bool) "idle" true (Local_chain_sync.agency_of StIdle = Client_agency);
  Alcotest.(check bool) "next" true (Local_chain_sync.agency_of StNext = Server_agency);
  Alcotest.(check bool) "intersect" true (Local_chain_sync.agency_of StIntersect = Server_agency);
  Alcotest.(check bool) "done" true (Local_chain_sync.agency_of StDone = Nobody_agency)

let () =
  Alcotest.run "Local-Chain-Sync"
    [ "Round-trips", [Alcotest.test_case "all messages" `Quick test_roundtrips];
      "Block payload", [Alcotest.test_case "carries full block" `Quick test_roll_forward_carries_block];
      "Valid transitions", [Alcotest.test_case "all valid" `Quick test_valid_transitions];
      "Invalid transitions", [Alcotest.test_case "all invalid" `Quick test_invalid_transitions];
      "Agency", [Alcotest.test_case "all states" `Quick test_agency] ]
