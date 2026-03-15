open Meridian

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let bytes_testable =
  Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

let check_roundtrip name msg =
  let raw1 = Block_fetch.to_bytes msg in
  match Block_fetch.of_bytes raw1 with
  | Error e -> Alcotest.fail (Printf.sprintf "decode: %s" e)
  | Ok msg' ->
    let raw2 = Block_fetch.to_bytes msg' in
    Alcotest.check bytes_testable name raw1 raw2

let hash32 () = Bytes.make 32 '\xab'
let hash32_alt () = Bytes.make 32 '\xcd'
let pt1 = Chain_sync.Point (100L, Bytes.make 32 '\xab')
let pt2 = Chain_sync.Point (200L, Bytes.make 32 '\xcd')

(* ---- Round-trips ---- *)

let test_request_range () =
  check_roundtrip "MsgRequestRange" (MsgRequestRange (pt1, pt2))

let test_request_range_origin () =
  check_roundtrip "MsgRequestRange from origin" (MsgRequestRange (Origin, pt2))

let test_client_done () =
  check_roundtrip "MsgClientDone" MsgClientDone

let test_start_batch () =
  check_roundtrip "MsgStartBatch" MsgStartBatch

let test_no_blocks () =
  check_roundtrip "MsgNoBlocks" MsgNoBlocks

let test_block () =
  check_roundtrip "MsgBlock" (MsgBlock (Bytes.make 256 '\xee'))

let test_block_empty () =
  check_roundtrip "MsgBlock empty" (MsgBlock Bytes.empty)

let test_batch_done () =
  check_roundtrip "MsgBatchDone" MsgBatchDone

(* ---- CBOR tags ---- *)

let test_tags () =
  let check tag msg =
    match Block_fetch.encode_message msg with
    | Cbor.Array (Cbor.Uint t :: _) ->
      Alcotest.(check int64) (Printf.sprintf "tag %Ld" tag) tag t
    | _ -> Alcotest.fail "expected [tag, ...]"
  in
  check 0L (MsgRequestRange (pt1, pt2));
  check 1L MsgClientDone;
  check 2L MsgStartBatch;
  check 3L MsgNoBlocks;
  check 4L (MsgBlock Bytes.empty);
  check 5L MsgBatchDone

(* ---- State transitions ---- *)

let test_idle_request_range () =
  match Block_fetch.transition StIdle (MsgRequestRange (pt1, pt2)) with
  | Ok StBusy -> () | _ -> Alcotest.fail "expected StBusy"

let test_idle_client_done () =
  match Block_fetch.transition StIdle MsgClientDone with
  | Ok StDone -> () | _ -> Alcotest.fail "expected StDone"

let test_busy_start_batch () =
  match Block_fetch.transition StBusy MsgStartBatch with
  | Ok StStreaming -> () | _ -> Alcotest.fail "expected StStreaming"

let test_busy_no_blocks () =
  match Block_fetch.transition StBusy MsgNoBlocks with
  | Ok StIdle -> () | _ -> Alcotest.fail "expected StIdle"

let test_streaming_block () =
  match Block_fetch.transition StStreaming (MsgBlock Bytes.empty) with
  | Ok StStreaming -> () | _ -> Alcotest.fail "expected StStreaming"

let test_streaming_batch_done () =
  match Block_fetch.transition StStreaming MsgBatchDone with
  | Ok StIdle -> () | _ -> Alcotest.fail "expected StIdle"

let test_invalid_block_in_idle () =
  match Block_fetch.transition StIdle (MsgBlock Bytes.empty) with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

let test_invalid_request_in_busy () =
  match Block_fetch.transition StBusy (MsgRequestRange (pt1, pt2)) with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

let test_invalid_in_done () =
  match Block_fetch.transition StDone MsgStartBatch with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

(* ---- Full sequence ---- *)

let test_full_fetch_sequence () =
  let check s m = match Block_fetch.transition s m with
    | Ok s' -> s' | Error e -> Alcotest.fail e
  in
  let s = Block_fetch.StIdle in
  let s = check s (MsgRequestRange (pt1, pt2)) in
  Alcotest.(check string) "busy" "StBusy" (Block_fetch.state_name s);
  let s = check s MsgStartBatch in
  let s = check s (MsgBlock (hash32 ())) in
  let s = check s (MsgBlock (hash32_alt ())) in
  let s = check s MsgBatchDone in
  Alcotest.(check string) "idle again" "StIdle" (Block_fetch.state_name s);
  let s = check s MsgClientDone in
  Alcotest.(check string) "done" "StDone" (Block_fetch.state_name s)

(* ---- Agency ---- *)

let test_agency () =
  Alcotest.(check bool) "idle=client" true
    (Block_fetch.agency_of StIdle = Client_agency);
  Alcotest.(check bool) "busy=server" true
    (Block_fetch.agency_of StBusy = Server_agency);
  Alcotest.(check bool) "streaming=server" true
    (Block_fetch.agency_of StStreaming = Server_agency);
  Alcotest.(check bool) "done=nobody" true
    (Block_fetch.agency_of StDone = Nobody_agency)

let () =
  Alcotest.run "Block-Fetch"
    [ ( "Round-trips",
        [ Alcotest.test_case "RequestRange" `Quick test_request_range;
          Alcotest.test_case "RequestRange origin" `Quick test_request_range_origin;
          Alcotest.test_case "ClientDone" `Quick test_client_done;
          Alcotest.test_case "StartBatch" `Quick test_start_batch;
          Alcotest.test_case "NoBlocks" `Quick test_no_blocks;
          Alcotest.test_case "Block" `Quick test_block;
          Alcotest.test_case "Block empty" `Quick test_block_empty;
          Alcotest.test_case "BatchDone" `Quick test_batch_done ] );
      ( "CBOR tags",
        [ Alcotest.test_case "all tags" `Quick test_tags ] );
      ( "Valid transitions",
        [ Alcotest.test_case "idle->busy" `Quick test_idle_request_range;
          Alcotest.test_case "idle->done" `Quick test_idle_client_done;
          Alcotest.test_case "busy->streaming" `Quick test_busy_start_batch;
          Alcotest.test_case "busy->idle" `Quick test_busy_no_blocks;
          Alcotest.test_case "streaming->streaming" `Quick test_streaming_block;
          Alcotest.test_case "streaming->idle" `Quick test_streaming_batch_done ] );
      ( "Invalid transitions",
        [ Alcotest.test_case "block in idle" `Quick test_invalid_block_in_idle;
          Alcotest.test_case "request in busy" `Quick test_invalid_request_in_busy;
          Alcotest.test_case "anything in done" `Quick test_invalid_in_done ] );
      ( "Full sequence",
        [ Alcotest.test_case "fetch range" `Quick test_full_fetch_sequence ] );
      ( "Agency",
        [ Alcotest.test_case "all states" `Quick test_agency ] );
    ]
