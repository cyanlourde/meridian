open Meridian

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let _bytes_testable =
  Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

(* ================================================================ *)
(* Message encoding round-trips                                      *)
(* ================================================================ *)

let test_keep_alive_roundtrip () =
  let msg = Keep_alive.MsgKeepAlive 12345 in
  let raw = Keep_alive.to_bytes msg in
  match Keep_alive.of_bytes raw with
  | Ok (MsgKeepAlive 12345) -> ()
  | _ -> Alcotest.fail "expected MsgKeepAlive 12345"

let test_keep_alive_response_roundtrip () =
  let msg = Keep_alive.MsgKeepAliveResponse 54321 in
  let raw = Keep_alive.to_bytes msg in
  match Keep_alive.of_bytes raw with
  | Ok (MsgKeepAliveResponse 54321) -> ()
  | _ -> Alcotest.fail "expected MsgKeepAliveResponse 54321"

(* ================================================================ *)
(* Protocol ID in mux framing                                        *)
(* ================================================================ *)

let test_protocol_id () =
  Alcotest.(check int) "keep_alive = 8" 8 Miniprotocol.keep_alive

let test_mux_framing () =
  let payload = Keep_alive.to_bytes (MsgKeepAlive 42) in
  let hdr = Mux.encode_segment_header Mux.{
    timestamp = 0l; protocol_id = Miniprotocol.keep_alive;
    payload_length = Bytes.length payload; from_initiator = false;
  } in
  match Mux.decode_segment_header hdr with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.(check int) "proto 8" 8 decoded.protocol_id;
    Alcotest.(check bool) "from responder" false decoded.from_initiator

(* ================================================================ *)
(* Pipe simulation: keep-alive interleaved with chain-sync           *)
(* ================================================================ *)

let test_keep_alive_during_chain_sync () =
  (* Simulate: client sends chain-sync MsgRequestNext.
     Server sends keep-alive MsgKeepAlive, then chain-sync MsgRollForward.
     The keep-alive responder should answer the ping AND deliver
     the chain-sync message to the caller. *)
  let (c2s_rd, c2s_wr) = Unix.pipe () in
  let (s2c_rd, s2c_wr) = Unix.pipe () in
  let client_out = Mux.create ~fd:c2s_wr ~mode:Initiator in
  let client_in = Mux.create ~fd:s2c_rd ~mode:Initiator in
  let _server_in = Mux.create ~fd:c2s_rd ~mode:Responder in
  let server_out = Mux.create ~fd:s2c_wr ~mode:Responder in
  let cleanup () =
    List.iter (fun fd -> try Unix.close fd with _ -> ())
      [c2s_rd; c2s_wr; s2c_rd; s2c_wr]
  in
  let send_seg mux pid msg_bytes =
    match Mux.send_segment mux ~protocol_id:pid ~timestamp:0l msg_bytes with
    | Ok () -> () | Error e -> cleanup (); Alcotest.fail e
  in
  (* Client sends chain-sync MsgRequestNext *)
  send_seg client_out Miniprotocol.chain_sync (Chain_sync.to_bytes MsgRequestNext);
  (* Server sends keep-alive ping FIRST *)
  send_seg server_out Miniprotocol.keep_alive (Keep_alive.to_bytes (MsgKeepAlive 999));
  (* Server then sends chain-sync RollForward *)
  let tip = Chain_sync.{ tip_point = Point (100L, Bytes.make 32 '\xaa');
                         tip_block_number = 10L } in
  send_seg server_out Miniprotocol.chain_sync
    (Chain_sync.to_bytes (MsgRollForward (Cbor.Null, tip)));
  (* Now use recv_bytes_for_protocol on client_in to read chain-sync.
     Since we can't use Network.t directly (it needs Tcp_connection),
     we'll manually replicate what it does with the mux. *)
  (* Read first segment — should be keep-alive (proto 8) *)
  (match Mux.recv_segment client_in with
   | Error e -> cleanup (); Alcotest.fail e
   | Ok (hdr1, payload1) ->
     Alcotest.(check int) "first seg proto" 8 hdr1.Mux.protocol_id;
     (* Decode keep-alive *)
     (match Keep_alive.of_bytes payload1 with
      | Ok (MsgKeepAlive 999) ->
        (* Send response back *)
        send_seg client_out Miniprotocol.keep_alive
          (Keep_alive.to_bytes (MsgKeepAliveResponse 999))
      | _ -> cleanup (); Alcotest.fail "expected MsgKeepAlive 999");
     (* Read second segment — should be chain-sync *)
     match Mux.recv_segment client_in with
     | Error e -> cleanup (); Alcotest.fail e
     | Ok (hdr2, payload2) ->
       Alcotest.(check int) "second seg proto" 2 hdr2.Mux.protocol_id;
       match Chain_sync.of_bytes payload2 with
       | Ok (MsgRollForward (_, t)) ->
         Alcotest.(check int64) "tip block" 10L t.tip_block_number
       | _ -> cleanup (); Alcotest.fail "expected MsgRollForward");
  (* Verify the keep-alive response was sent *)
  (match Mux.recv_segment _server_in with
   | Ok (_, data) ->
     (* First segment from client is the MsgRequestNext *)
     ignore data;
     (* Second is the keep-alive response *)
     (match Mux.recv_segment _server_in with
      | Ok (hdr, data) ->
        Alcotest.(check int) "ka response proto" 8 hdr.Mux.protocol_id;
        (match Keep_alive.of_bytes data with
         | Ok (MsgKeepAliveResponse 999) -> ()
         | _ -> cleanup (); Alcotest.fail "expected KA response 999")
      | Error e -> cleanup (); Alcotest.fail e)
   | Error e -> cleanup (); Alcotest.fail e);
  cleanup ()

(* ================================================================ *)
(* Pipe simulation: keep-alive during block-fetch batch              *)
(* ================================================================ *)

let test_keep_alive_during_block_fetch () =
  (* Server interleaves keep-alive pings between block messages.
     Verify all pings answered and all blocks received. *)
  let (c2s_rd, c2s_wr) = Unix.pipe () in
  let (s2c_rd, s2c_wr) = Unix.pipe () in
  let _client_out = Mux.create ~fd:c2s_wr ~mode:Initiator in
  let client_in = Mux.create ~fd:s2c_rd ~mode:Initiator in
  let server_out = Mux.create ~fd:s2c_wr ~mode:Responder in
  let cleanup () =
    List.iter (fun fd -> try Unix.close fd with _ -> ())
      [c2s_rd; c2s_wr; s2c_rd; s2c_wr]
  in
  let send_seg mux pid msg_bytes =
    match Mux.send_segment mux ~protocol_id:pid ~timestamp:0l msg_bytes with
    | Ok () -> () | Error e -> cleanup (); Alcotest.fail e
  in
  let bf_id = Miniprotocol.block_fetch in
  let ka_id = Miniprotocol.keep_alive in
  (* Server sends: StartBatch, KA ping, Block1, KA ping, Block2, BatchDone *)
  send_seg server_out bf_id (Block_fetch.to_bytes MsgStartBatch);
  send_seg server_out ka_id (Keep_alive.to_bytes (MsgKeepAlive 100));
  send_seg server_out bf_id (Block_fetch.to_bytes (MsgBlock (Bytes.make 50 '\x01')));
  send_seg server_out ka_id (Keep_alive.to_bytes (MsgKeepAlive 200));
  send_seg server_out bf_id (Block_fetch.to_bytes (MsgBlock (Bytes.make 60 '\x02')));
  send_seg server_out bf_id (Block_fetch.to_bytes MsgBatchDone);
  (* Client reads block-fetch messages, manually handling KA *)
  let ka_count = ref 0 in
  let blocks = ref [] in
  let rec read_bf () =
    match Mux.recv_segment client_in with
    | Error e -> cleanup (); Alcotest.fail e
    | Ok (hdr, payload) ->
      if hdr.Mux.protocol_id = ka_id then begin
        (* Handle keep-alive *)
        (match Keep_alive.of_bytes payload with
         | Ok (MsgKeepAlive _cookie) -> incr ka_count
         | _ -> ());
        read_bf ()
      end else if hdr.Mux.protocol_id = bf_id then begin
        match Block_fetch.of_bytes payload with
        | Ok MsgStartBatch -> read_bf ()
        | Ok (MsgBlock b) -> blocks := b :: !blocks; read_bf ()
        | Ok MsgBatchDone -> ()
        | Ok _ -> cleanup (); Alcotest.fail "unexpected bf msg"
        | Error e -> cleanup (); Alcotest.fail e
      end else
        read_bf ()
  in
  read_bf ();
  Alcotest.(check int) "2 ka pings" 2 !ka_count;
  Alcotest.(check int) "2 blocks" 2 (List.length !blocks);
  let blocks = List.rev !blocks in
  Alcotest.(check int) "block1 size" 50 (Bytes.length (List.hd blocks));
  Alcotest.(check int) "block2 size" 60 (Bytes.length (List.nth blocks 1));
  cleanup ()

(* ================================================================ *)
(* Cookie value preservation                                         *)
(* ================================================================ *)

let test_cookie_value_preserved () =
  (* Verify various cookie values round-trip correctly *)
  List.iter (fun cookie ->
    let raw = Keep_alive.to_bytes (MsgKeepAlive cookie) in
    match Keep_alive.of_bytes raw with
    | Ok (MsgKeepAlive c) -> Alcotest.(check int) "cookie" cookie c
    | _ -> Alcotest.fail "decode failed"
  ) [0; 1; 255; 256; 65535; 12345]

(* ================================================================ *)
(* CBOR structure                                                    *)
(* ================================================================ *)

let test_cbor_structure () =
  let cbor = Keep_alive.encode_message (MsgKeepAlive 42) in
  (match cbor with
   | Cbor.Array [Cbor.Uint 0L; Cbor.Uint 42L] -> ()
   | _ -> Alcotest.fail "expected [0, 42]");
  let cbor = Keep_alive.encode_message (MsgKeepAliveResponse 42) in
  match cbor with
  | Cbor.Array [Cbor.Uint 1L; Cbor.Uint 42L] -> ()
  | _ -> Alcotest.fail "expected [1, 42]"

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Keep-Alive-Live"
    [ ( "Message encoding",
        [ Alcotest.test_case "KeepAlive roundtrip" `Quick test_keep_alive_roundtrip;
          Alcotest.test_case "Response roundtrip" `Quick test_keep_alive_response_roundtrip;
          Alcotest.test_case "cookie values" `Quick test_cookie_value_preserved ] );
      ( "Protocol ID",
        [ Alcotest.test_case "keep_alive = 8" `Quick test_protocol_id;
          Alcotest.test_case "mux framing" `Quick test_mux_framing ] );
      ( "CBOR structure",
        [ Alcotest.test_case "message tags" `Quick test_cbor_structure ] );
      ( "Pipe simulation",
        [ Alcotest.test_case "KA during chain-sync" `Quick test_keep_alive_during_chain_sync;
          Alcotest.test_case "KA during block-fetch" `Quick test_keep_alive_during_block_fetch ] );
    ]
