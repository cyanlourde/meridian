open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    b;
  Buffer.contents buf

let bytes_testable =
  Alcotest.testable
    (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b))
    Bytes.equal

let check_roundtrip name msg =
  let raw1 = Chain_sync.to_bytes msg in
  match Chain_sync.of_bytes raw1 with
  | Error e -> Alcotest.fail (Printf.sprintf "decode failed: %s" e)
  | Ok msg' ->
    let raw2 = Chain_sync.to_bytes msg' in
    Alcotest.check bytes_testable name raw1 raw2

let hash32 () = Bytes.make 32 '\xab'
let hash32_alt () = Bytes.make 32 '\xcd'

let sample_point = Chain_sync.Point (1000L, hash32 ())
let sample_tip = Chain_sync.{ tip_point = Point (2000L, hash32_alt ()); tip_block_number = 500L }

(* ================================================================ *)
(* Message encode/decode round-trips                                 *)
(* ================================================================ *)

let test_request_next () =
  check_roundtrip "MsgRequestNext" MsgRequestNext

let test_await_reply () =
  check_roundtrip "MsgAwaitReply" MsgAwaitReply

let test_roll_forward () =
  let header = Cbor.Array [Cbor.Uint 1L; Cbor.Bytes (Bytes.make 64 '\xee')] in
  check_roundtrip "MsgRollForward" (MsgRollForward (header, sample_tip))

let test_roll_backward () =
  check_roundtrip "MsgRollBackward" (MsgRollBackward (sample_point, sample_tip))

let test_roll_backward_origin () =
  check_roundtrip "MsgRollBackward to origin" (MsgRollBackward (Origin, sample_tip))

let test_find_intersect () =
  let points = [
    Chain_sync.Point (100L, hash32 ());
    Chain_sync.Point (50L, hash32_alt ());
    Origin;
  ] in
  check_roundtrip "MsgFindIntersect" (MsgFindIntersect points)

let test_find_intersect_empty () =
  check_roundtrip "MsgFindIntersect empty" (MsgFindIntersect [])

let test_intersect_found_msg () =
  check_roundtrip "MsgIntersectFound" (MsgIntersectFound (sample_point, sample_tip))

let test_intersect_not_found_msg () =
  check_roundtrip "MsgIntersectNotFound" (MsgIntersectNotFound sample_tip)

let test_done () =
  check_roundtrip "MsgDone" MsgDone

(* ================================================================ *)
(* Point and tip encoding                                            *)
(* ================================================================ *)

let test_point_origin_cbor () =
  let cbor = Chain_sync.encode_point Origin in
  (match cbor with
   | Cbor.Array [] -> ()
   | _ -> Alcotest.fail "Origin should encode as []");
  match Chain_sync.decode_point cbor with
  | Ok Origin -> ()
  | _ -> Alcotest.fail "should decode back to Origin"

let test_point_specific_cbor () =
  let p = Chain_sync.Point (42L, hash32 ()) in
  let cbor = Chain_sync.encode_point p in
  (match cbor with
   | Cbor.Array [Cbor.Uint 42L; Cbor.Bytes _] -> ()
   | _ -> Alcotest.fail "Point should encode as [slot, hash]");
  match Chain_sync.decode_point cbor with
  | Ok (Point (42L, _)) -> ()
  | _ -> Alcotest.fail "should decode back to Point"

let test_tip_cbor () =
  let t = Chain_sync.{ tip_point = Point (100L, hash32 ()); tip_block_number = 99L } in
  let cbor = Chain_sync.encode_tip t in
  match cbor with
  | Cbor.Array [Cbor.Array [Cbor.Uint 100L; Cbor.Bytes _]; Cbor.Uint 99L] -> ()
  | _ -> Alcotest.fail "tip should encode as [point, block_number]"

let test_tip_origin_cbor () =
  let t = Chain_sync.{ tip_point = Origin; tip_block_number = 0L } in
  let cbor = Chain_sync.encode_tip t in
  match cbor with
  | Cbor.Array [Cbor.Array []; Cbor.Uint 0L] -> ()
  | _ -> Alcotest.fail "tip with origin should encode as [[], 0]"

(* ================================================================ *)
(* CBOR tag verification                                             *)
(* ================================================================ *)

let test_message_tags () =
  let check_tag name expected msg =
    let cbor = Chain_sync.encode_message msg in
    match cbor with
    | Cbor.Array (Cbor.Uint tag :: _) ->
      Alcotest.(check int64) name expected tag
    | _ -> Alcotest.fail (name ^ ": expected [tag, ...]")
  in
  check_tag "RequestNext tag"      0L MsgRequestNext;
  check_tag "AwaitReply tag"       1L MsgAwaitReply;
  check_tag "RollForward tag"      2L (MsgRollForward (Cbor.Null, sample_tip));
  check_tag "RollBackward tag"     3L (MsgRollBackward (Origin, sample_tip));
  check_tag "FindIntersect tag"    4L (MsgFindIntersect []);
  check_tag "IntersectFound tag"   5L (MsgIntersectFound (Origin, sample_tip));
  check_tag "IntersectNotFound tag" 6L (MsgIntersectNotFound sample_tip);
  check_tag "Done tag"             7L MsgDone

(* ================================================================ *)
(* State machine transitions                                         *)
(* ================================================================ *)

let test_idle_to_next () =
  match Chain_sync.transition StIdle MsgRequestNext with
  | Ok StNext -> ()
  | Ok s -> Alcotest.fail (Printf.sprintf "expected StNext, got %s" (Chain_sync.state_name s))
  | Error e -> Alcotest.fail e

let test_idle_to_intersect () =
  match Chain_sync.transition StIdle (MsgFindIntersect [sample_point]) with
  | Ok StIntersect -> ()
  | Ok s -> Alcotest.fail (Printf.sprintf "expected StIntersect, got %s" (Chain_sync.state_name s))
  | Error e -> Alcotest.fail e

let test_idle_to_done () =
  match Chain_sync.transition StIdle MsgDone with
  | Ok StDone -> ()
  | _ -> Alcotest.fail "expected StDone"

let test_next_roll_forward () =
  match Chain_sync.transition StNext (MsgRollForward (Cbor.Null, sample_tip)) with
  | Ok StIdle -> ()
  | _ -> Alcotest.fail "expected StIdle after RollForward"

let test_next_roll_backward () =
  match Chain_sync.transition StNext (MsgRollBackward (sample_point, sample_tip)) with
  | Ok StIdle -> ()
  | _ -> Alcotest.fail "expected StIdle after RollBackward"

let test_next_await_reply () =
  match Chain_sync.transition StNext MsgAwaitReply with
  | Ok StNext -> ()
  | _ -> Alcotest.fail "expected StNext after AwaitReply"

let test_intersect_found () =
  match Chain_sync.transition StIntersect (MsgIntersectFound (sample_point, sample_tip)) with
  | Ok StIdle -> ()
  | _ -> Alcotest.fail "expected StIdle after IntersectFound"

let test_intersect_not_found () =
  match Chain_sync.transition StIntersect (MsgIntersectNotFound sample_tip) with
  | Ok StIdle -> ()
  | _ -> Alcotest.fail "expected StIdle after IntersectNotFound"

(* Invalid transitions *)

let test_invalid_request_in_next () =
  match Chain_sync.transition StNext MsgRequestNext with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "RequestNext in StNext should be invalid"

let test_invalid_roll_forward_in_idle () =
  match Chain_sync.transition StIdle (MsgRollForward (Cbor.Null, sample_tip)) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "RollForward in StIdle should be invalid"

let test_invalid_done_in_next () =
  match Chain_sync.transition StNext MsgDone with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Done in StNext should be invalid"

let test_invalid_find_intersect_in_intersect () =
  match Chain_sync.transition StIntersect (MsgFindIntersect []) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "FindIntersect in StIntersect should be invalid"

let test_nothing_from_done () =
  match Chain_sync.transition StDone MsgRequestNext with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "any message in StDone should be invalid"

(* ================================================================ *)
(* Agency                                                            *)
(* ================================================================ *)

let test_agency () =
  Alcotest.(check bool) "StIdle = client"
    true (Chain_sync.agency_of StIdle = Client_agency);
  Alcotest.(check bool) "StNext = server"
    true (Chain_sync.agency_of StNext = Server_agency);
  Alcotest.(check bool) "StIntersect = server"
    true (Chain_sync.agency_of StIntersect = Server_agency);
  Alcotest.(check bool) "StDone = nobody"
    true (Chain_sync.agency_of StDone = Nobody_agency)

(* ================================================================ *)
(* Full sequence simulation                                          *)
(* ================================================================ *)

let test_full_sequence () =
  (* Simulate: FindIntersect -> IntersectFound -> RequestNext -> RollForward -> Done *)
  let state = Chain_sync.StIdle in
  let check s msg =
    match Chain_sync.transition s msg with
    | Ok s' -> s'
    | Error e -> Alcotest.fail e
  in
  let state = check state (MsgFindIntersect [sample_point; Origin]) in
  Alcotest.(check string) "after FindIntersect" "StIntersect" (Chain_sync.state_name state);
  let state = check state (MsgIntersectFound (sample_point, sample_tip)) in
  Alcotest.(check string) "after IntersectFound" "StIdle" (Chain_sync.state_name state);
  let state = check state MsgRequestNext in
  Alcotest.(check string) "after RequestNext" "StNext" (Chain_sync.state_name state);
  let state = check state (MsgRollForward (Cbor.Uint 42L, sample_tip)) in
  Alcotest.(check string) "after RollForward" "StIdle" (Chain_sync.state_name state);
  let state = check state MsgDone in
  Alcotest.(check string) "after Done" "StDone" (Chain_sync.state_name state)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Chain-Sync"
    [ ( "Message round-trips",
        [ Alcotest.test_case "RequestNext" `Quick test_request_next;
          Alcotest.test_case "AwaitReply" `Quick test_await_reply;
          Alcotest.test_case "RollForward" `Quick test_roll_forward;
          Alcotest.test_case "RollBackward" `Quick test_roll_backward;
          Alcotest.test_case "RollBackward to origin" `Quick test_roll_backward_origin;
          Alcotest.test_case "FindIntersect" `Quick test_find_intersect;
          Alcotest.test_case "FindIntersect empty" `Quick test_find_intersect_empty;
          Alcotest.test_case "IntersectFound" `Quick test_intersect_found_msg;
          Alcotest.test_case "IntersectNotFound" `Quick test_intersect_not_found_msg;
          Alcotest.test_case "Done" `Quick test_done ] );
      ( "Point/tip encoding",
        [ Alcotest.test_case "origin point" `Quick test_point_origin_cbor;
          Alcotest.test_case "specific point" `Quick test_point_specific_cbor;
          Alcotest.test_case "tip" `Quick test_tip_cbor;
          Alcotest.test_case "tip with origin" `Quick test_tip_origin_cbor ] );
      ( "CBOR tags",
        [ Alcotest.test_case "all message tags" `Quick test_message_tags ] );
      ( "State transitions (valid)",
        [ Alcotest.test_case "idle -> next" `Quick test_idle_to_next;
          Alcotest.test_case "idle -> intersect" `Quick test_idle_to_intersect;
          Alcotest.test_case "idle -> done" `Quick test_idle_to_done;
          Alcotest.test_case "next -> idle (forward)" `Quick test_next_roll_forward;
          Alcotest.test_case "next -> idle (backward)" `Quick test_next_roll_backward;
          Alcotest.test_case "next -> next (await)" `Quick test_next_await_reply;
          Alcotest.test_case "intersect -> idle (found)" `Quick test_intersect_found;
          Alcotest.test_case "intersect -> idle (not found)" `Quick test_intersect_not_found ] );
      ( "State transitions (invalid)",
        [ Alcotest.test_case "request in StNext" `Quick test_invalid_request_in_next;
          Alcotest.test_case "roll forward in StIdle" `Quick test_invalid_roll_forward_in_idle;
          Alcotest.test_case "done in StNext" `Quick test_invalid_done_in_next;
          Alcotest.test_case "find in StIntersect" `Quick test_invalid_find_intersect_in_intersect;
          Alcotest.test_case "anything in StDone" `Quick test_nothing_from_done ] );
      ( "Agency",
        [ Alcotest.test_case "agency per state" `Quick test_agency ] );
      ( "Full sequence",
        [ Alcotest.test_case "intersect + sync + done" `Quick test_full_sequence ] );
    ]
