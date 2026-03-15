open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let _bytes_testable =
  Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

let hash32 () = Bytes.make 32 '\xab'

let sample_tip = Chain_sync.{
  tip_point = Point (1000L, Bytes.make 32 '\xcd');
  tip_block_number = 500L;
}

(* ================================================================ *)
(* Message encoding round-trips for chain-sync wire format           *)
(* ================================================================ *)

let test_find_intersect_empty_roundtrip () =
  let msg = Chain_sync.MsgFindIntersect [] in
  let payload = Chain_sync.to_bytes msg in
  match Chain_sync.of_bytes payload with
  | Error e -> Alcotest.fail e
  | Ok (MsgFindIntersect []) -> ()
  | Ok _ -> Alcotest.fail "expected MsgFindIntersect []"

let test_find_intersect_origin_roundtrip () =
  let msg = Chain_sync.MsgFindIntersect [Origin] in
  let payload = Chain_sync.to_bytes msg in
  match Chain_sync.of_bytes payload with
  | Error e -> Alcotest.fail e
  | Ok (MsgFindIntersect [Origin]) -> ()
  | Ok _ -> Alcotest.fail "expected MsgFindIntersect [Origin]"

let test_request_next_roundtrip () =
  let msg = Chain_sync.MsgRequestNext in
  let payload = Chain_sync.to_bytes msg in
  match Chain_sync.of_bytes payload with
  | Error e -> Alcotest.fail e
  | Ok MsgRequestNext -> ()
  | Ok _ -> Alcotest.fail "expected MsgRequestNext"

let test_roll_forward_roundtrip () =
  let header = Cbor.Array [Cbor.Uint 0L; Cbor.Bytes (Bytes.make 64 '\xee')] in
  let msg = Chain_sync.MsgRollForward (header, sample_tip) in
  let payload = Chain_sync.to_bytes msg in
  match Chain_sync.of_bytes payload with
  | Error e -> Alcotest.fail e
  | Ok (MsgRollForward (_, tip)) ->
    Alcotest.(check int64) "tip block" 500L tip.tip_block_number
  | Ok _ -> Alcotest.fail "expected MsgRollForward"

(* ================================================================ *)
(* Protocol ID verification                                          *)
(* ================================================================ *)

let test_chain_sync_protocol_id () =
  Alcotest.(check int) "chain_sync = 2" 2 Miniprotocol.chain_sync

let test_chain_sync_mux_framing () =
  (* Verify chain-sync messages are framed with protocol ID 2 *)
  let payload = Chain_sync.to_bytes MsgRequestNext in
  let hdr = Mux.encode_segment_header Mux.{
    timestamp = 0l;
    protocol_id = Miniprotocol.chain_sync;
    payload_length = Bytes.length payload;
    from_initiator = true;
  } in
  match Mux.decode_segment_header hdr with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.(check int) "protocol_id = 2" 2 decoded.protocol_id;
    Alcotest.(check bool) "initiator" true decoded.from_initiator

(* ================================================================ *)
(* Simulated chain-sync exchange over pipe                           *)
(* ================================================================ *)

let test_simulated_chain_sync () =
  (* Simulate a chain-sync exchange:
     Client sends FindIntersect -> Server replies IntersectNotFound
     Client sends RequestNext -> Server replies RollForward (x3) *)
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
  let pid = Miniprotocol.chain_sync in
  let send_cs mux msg =
    let payload = Chain_sync.to_bytes msg in
    match Mux.send_segment mux ~protocol_id:pid ~timestamp:0l payload with
    | Ok () -> () | Error e -> cleanup (); Alcotest.fail e
  in
  let recv_cs mux =
    match Mux.recv_segment mux with
    | Error e -> cleanup (); Alcotest.fail e
    | Ok (hdr, payload) ->
      Alcotest.(check int) "protocol_id" 2 hdr.protocol_id;
      match Chain_sync.of_bytes payload with
      | Error e -> cleanup (); Alcotest.fail e
      | Ok msg -> msg
  in
  (* Client: FindIntersect with origin *)
  send_cs client_out (MsgFindIntersect [Origin]);
  (* Server: receive and reply IntersectNotFound *)
  (match recv_cs server_in with
   | MsgFindIntersect [Origin] -> ()
   | _ -> cleanup (); Alcotest.fail "expected FindIntersect [Origin]");
  let genesis_tip = Chain_sync.{
    tip_point = Point (0L, Bytes.make 32 '\x00');
    tip_block_number = 0L;
  } in
  send_cs server_out (MsgIntersectNotFound genesis_tip);
  (* Client: receive IntersectNotFound *)
  (match recv_cs client_in with
   | MsgIntersectNotFound tip ->
     Alcotest.(check int64) "genesis tip block" 0L tip.tip_block_number
   | _ -> cleanup (); Alcotest.fail "expected IntersectNotFound");
  (* Client: RequestNext x3, Server: RollForward x3 *)
  for i = 1 to 3 do
    send_cs client_out MsgRequestNext;
    (match recv_cs server_in with
     | MsgRequestNext -> ()
     | _ -> cleanup (); Alcotest.fail "expected RequestNext");
    let slot = Int64.of_int (i * 100) in
    let block_hash = Bytes.make 32 (Char.chr i) in
    let tip = Chain_sync.{
      tip_point = Point (slot, block_hash);
      tip_block_number = Int64.of_int i;
    } in
    let header = Cbor.Array [Cbor.Uint (Int64.of_int i); Cbor.Bytes block_hash] in
    send_cs server_out (MsgRollForward (header, tip));
    match recv_cs client_in with
    | MsgRollForward (_hdr, tip) ->
      Alcotest.(check int64) (Printf.sprintf "block %d slot" i) slot
        (match tip.tip_point with Point (s, _) -> s | Origin -> -1L);
      Alcotest.(check int64) (Printf.sprintf "block %d number" i)
        (Int64.of_int i) tip.tip_block_number
    | _ -> cleanup (); Alcotest.fail "expected RollForward"
  done;
  cleanup ()

(* ================================================================ *)
(* Wire format: FindIntersect CBOR structure                         *)
(* ================================================================ *)

let test_find_intersect_cbor_structure () =
  (* [4, []] for empty points list *)
  let cbor = Chain_sync.encode_message (MsgFindIntersect []) in
  match cbor with
  | Cbor.Array [Cbor.Uint 4L; Cbor.Array []] -> ()
  | _ -> Alcotest.fail "expected [4, []]"

let test_find_intersect_origin_cbor () =
  (* [4, [[]]] for origin *)
  let cbor = Chain_sync.encode_message (MsgFindIntersect [Origin]) in
  match cbor with
  | Cbor.Array [Cbor.Uint 4L; Cbor.Array [Cbor.Array []]] -> ()
  | _ -> Alcotest.fail "expected [4, [[]]]"

let test_find_intersect_point_cbor () =
  (* [4, [[slot, hash]]] for a specific point *)
  let pt = Chain_sync.Point (42L, hash32 ()) in
  let cbor = Chain_sync.encode_message (MsgFindIntersect [pt]) in
  match cbor with
  | Cbor.Array [Cbor.Uint 4L; Cbor.Array [Cbor.Array [Cbor.Uint 42L; Cbor.Bytes _]]] -> ()
  | _ -> Alcotest.fail "expected [4, [[42, hash]]]"

let test_request_next_cbor () =
  let cbor = Chain_sync.encode_message MsgRequestNext in
  match cbor with
  | Cbor.Array [Cbor.Uint 0L] -> ()
  | _ -> Alcotest.fail "expected [0]"

(* ================================================================ *)
(* Simulated AwaitReply -> RollForward                               *)
(* ================================================================ *)

let test_await_reply_then_forward () =
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
  let pid = Miniprotocol.chain_sync in
  let send mux msg =
    let payload = Chain_sync.to_bytes msg in
    match Mux.send_segment mux ~protocol_id:pid ~timestamp:0l payload with
    | Ok () -> () | Error e -> cleanup (); Alcotest.fail e
  in
  let recv mux =
    match Mux.recv_segment mux with
    | Error e -> cleanup (); Alcotest.fail e
    | Ok (_, payload) ->
      match Chain_sync.of_bytes payload with
      | Error e -> cleanup (); Alcotest.fail e
      | Ok msg -> msg
  in
  (* Client: RequestNext *)
  send client_out MsgRequestNext;
  ignore (recv server_in : Chain_sync.chain_sync_message);
  (* Server: AwaitReply (at tip) *)
  send server_out MsgAwaitReply;
  (match recv client_in with
   | MsgAwaitReply -> ()
   | _ -> cleanup (); Alcotest.fail "expected AwaitReply");
  (* Server: RollForward (new block arrived) *)
  let tip = Chain_sync.{
    tip_point = Point (999L, Bytes.make 32 '\xff');
    tip_block_number = 100L;
  } in
  send server_out (MsgRollForward (Cbor.Null, tip));
  (match recv client_in with
   | MsgRollForward (_, tip) ->
     Alcotest.(check int64) "new block" 100L tip.tip_block_number
   | _ -> cleanup (); Alcotest.fail "expected RollForward");
  cleanup ()

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Chain-Sync-Live"
    [ ( "Message encoding",
        [ Alcotest.test_case "FindIntersect empty" `Quick test_find_intersect_empty_roundtrip;
          Alcotest.test_case "FindIntersect origin" `Quick test_find_intersect_origin_roundtrip;
          Alcotest.test_case "RequestNext" `Quick test_request_next_roundtrip;
          Alcotest.test_case "RollForward" `Quick test_roll_forward_roundtrip ] );
      ( "Protocol ID",
        [ Alcotest.test_case "chain_sync = 2" `Quick test_chain_sync_protocol_id;
          Alcotest.test_case "mux framing" `Quick test_chain_sync_mux_framing ] );
      ( "CBOR structure",
        [ Alcotest.test_case "FindIntersect empty" `Quick test_find_intersect_cbor_structure;
          Alcotest.test_case "FindIntersect origin" `Quick test_find_intersect_origin_cbor;
          Alcotest.test_case "FindIntersect point" `Quick test_find_intersect_point_cbor;
          Alcotest.test_case "RequestNext" `Quick test_request_next_cbor ] );
      ( "Pipe simulation",
        [ Alcotest.test_case "full exchange" `Quick test_simulated_chain_sync;
          Alcotest.test_case "await then forward" `Quick test_await_reply_then_forward ] );
    ]
