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

(* ---- Protocol ID in mux framing ---- *)

let test_mux_protocol_id () =
  let payload = Block_fetch.to_bytes (MsgRequestRange (pt1, pt2)) in
  let hdr = Mux.encode_segment_header Mux.{
    timestamp = 0l;
    protocol_id = Miniprotocol.block_fetch;
    payload_length = Bytes.length payload;
    from_initiator = true;
  } in
  match Mux.decode_segment_header hdr with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.(check int) "protocol_id = 3" 3 decoded.protocol_id;
    Alcotest.(check bool) "initiator" true decoded.from_initiator

(* ---- Pipe simulation: full batch ---- *)

let test_pipe_full_batch () =
  let (c2s_rd, c2s_wr) = Unix.pipe () in
  let (s2c_rd, s2c_wr) = Unix.pipe () in
  let client_out = Mux.create ~fd:c2s_wr ~mode:Initiator in
  let client_in = Mux.create ~fd:s2c_rd ~mode:Initiator in
  let server_in = Mux.create ~fd:c2s_rd ~mode:Responder in
  let server_out = Mux.create ~fd:s2c_wr ~mode:Responder in
  let cleanup () =
    List.iter (fun fd -> try Unix.close fd with _ -> ())
      [c2s_rd; c2s_wr; s2c_rd; s2c_wr]
  in
  let pid = Miniprotocol.block_fetch in
  let send mux msg =
    let payload = Block_fetch.to_bytes msg in
    match Mux.send_segment mux ~protocol_id:pid ~timestamp:0l payload with
    | Ok () -> () | Error e -> cleanup (); Alcotest.fail e
  in
  let recv mux =
    match Mux.recv_segment mux with
    | Error e -> cleanup (); Alcotest.fail e
    | Ok (hdr, payload) ->
      Alcotest.(check int) "bf protocol" 3 hdr.protocol_id;
      match Block_fetch.of_bytes payload with
      | Error e -> cleanup (); Alcotest.fail e
      | Ok msg -> msg
  in
  (* Client sends RequestRange *)
  send client_out (MsgRequestRange (pt1, pt2));
  (match recv server_in with
   | MsgRequestRange _ -> ()
   | _ -> cleanup (); Alcotest.fail "expected RequestRange");
  (* Server sends StartBatch *)
  send server_out MsgStartBatch;
  (match recv client_in with
   | MsgStartBatch -> ()
   | _ -> cleanup (); Alcotest.fail "expected StartBatch");
  (* Server streams 3 blocks with known payloads *)
  let block_data = [|
    Bytes.make 100 '\x01';
    Bytes.make 200 '\x02';
    Bytes.make 50 '\x03';
  |] in
  Array.iter (fun data ->
    send server_out (MsgBlock data);
    match recv client_in with
    | MsgBlock received ->
      Alcotest.check bytes_testable "block payload" data received
    | _ -> cleanup (); Alcotest.fail "expected MsgBlock"
  ) block_data;
  (* Server sends BatchDone *)
  send server_out MsgBatchDone;
  (match recv client_in with
   | MsgBatchDone -> ()
   | _ -> cleanup (); Alcotest.fail "expected BatchDone");
  cleanup ()

(* ---- Pipe simulation: NoBlocks path ---- *)

let test_pipe_no_blocks () =
  let (c2s_rd, c2s_wr) = Unix.pipe () in
  let (s2c_rd, s2c_wr) = Unix.pipe () in
  let client_out = Mux.create ~fd:c2s_wr ~mode:Initiator in
  let client_in = Mux.create ~fd:s2c_rd ~mode:Initiator in
  let server_in = Mux.create ~fd:c2s_rd ~mode:Responder in
  let server_out = Mux.create ~fd:s2c_wr ~mode:Responder in
  let cleanup () =
    List.iter (fun fd -> try Unix.close fd with _ -> ())
      [c2s_rd; c2s_wr; s2c_rd; s2c_wr]
  in
  let pid = Miniprotocol.block_fetch in
  let send mux msg =
    let payload = Block_fetch.to_bytes msg in
    match Mux.send_segment mux ~protocol_id:pid ~timestamp:0l payload with
    | Ok () -> () | Error e -> cleanup (); Alcotest.fail e
  in
  let recv mux =
    match Mux.recv_segment mux with
    | Error e -> cleanup (); Alcotest.fail e
    | Ok (_, payload) ->
      match Block_fetch.of_bytes payload with
      | Error e -> cleanup (); Alcotest.fail e
      | Ok msg -> msg
  in
  (* Client sends RequestRange *)
  send client_out (MsgRequestRange (pt1, pt2));
  ignore (recv server_in : Block_fetch.block_fetch_message);
  (* Server sends NoBlocks *)
  send server_out MsgNoBlocks;
  (match recv client_in with
   | MsgNoBlocks -> ()
   | _ -> cleanup (); Alcotest.fail "expected NoBlocks");
  cleanup ()

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
      ( "Protocol ID",
        [ Alcotest.test_case "mux framing" `Quick test_mux_protocol_id ] );
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
      ( "Pipe simulation",
        [ Alcotest.test_case "full batch" `Quick test_pipe_full_batch;
          Alcotest.test_case "no blocks" `Quick test_pipe_no_blocks ] );
    ]
