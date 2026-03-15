open Meridian

(* ================================================================ *)
(* Test helpers                                                      *)
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

let segment_header_testable =
  Alcotest.testable
    (fun fmt h ->
       Format.fprintf fmt "{ts=%ld, pid=%d, len=%d, init=%b}"
         h.Mux.timestamp h.protocol_id h.payload_length h.from_initiator)
    (fun a b ->
       Int32.equal a.Mux.timestamp b.Mux.timestamp
       && a.protocol_id = b.protocol_id
       && a.payload_length = b.payload_length
       && a.from_initiator = b.from_initiator)

(* ================================================================ *)
(* Header encode/decode round-trip                                   *)
(* ================================================================ *)

let test_header_roundtrip_initiator () =
  let hdr = Mux.{
    timestamp = 1000l;
    protocol_id = Miniprotocol.chain_sync;
    payload_length = 256;
    from_initiator = true;
  } in
  let encoded = Mux.encode_segment_header hdr in
  Alcotest.(check int) "header size" 8 (Bytes.length encoded);
  match Mux.decode_segment_header encoded with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.check segment_header_testable "round-trip initiator" hdr decoded

let test_header_roundtrip_responder () =
  let hdr = Mux.{
    timestamp = 42000l;
    protocol_id = Miniprotocol.block_fetch;
    payload_length = 65535;
    from_initiator = false;
  } in
  let encoded = Mux.encode_segment_header hdr in
  match Mux.decode_segment_header encoded with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.check segment_header_testable "round-trip responder" hdr decoded

let test_header_roundtrip_zero () =
  let hdr = Mux.{
    timestamp = 0l;
    protocol_id = Miniprotocol.handshake;
    payload_length = 0;
    from_initiator = true;
  } in
  let encoded = Mux.encode_segment_header hdr in
  match Mux.decode_segment_header encoded with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.check segment_header_testable "round-trip zero" hdr decoded

let test_header_roundtrip_keep_alive () =
  let hdr = Mux.{
    timestamp = Int32.max_int;
    protocol_id = Miniprotocol.keep_alive;
    payload_length = 4;
    from_initiator = true;
  } in
  let encoded = Mux.encode_segment_header hdr in
  match Mux.decode_segment_header encoded with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.check segment_header_testable "round-trip keep-alive" hdr decoded

(* ================================================================ *)
(* Byte layout verification                                          *)
(* ================================================================ *)

let test_byte_layout_initiator () =
  (* Initiator sending chain-sync (id=2), 100 bytes, timestamp=0 *)
  let hdr = Mux.{
    timestamp = 0l;
    protocol_id = 2;
    payload_length = 100;
    from_initiator = true;
  } in
  let encoded = Mux.encode_segment_header hdr in
  (* Bytes 0-3: timestamp = 0x00000000 *)
  Alcotest.(check int) "ts byte 0" 0x00 (Bytes.get_uint8 encoded 0);
  Alcotest.(check int) "ts byte 3" 0x00 (Bytes.get_uint8 encoded 3);
  (* Bytes 4-5: protocol_id = 0x0002 (no direction bit) *)
  Alcotest.(check int) "pid byte 4" 0x00 (Bytes.get_uint8 encoded 4);
  Alcotest.(check int) "pid byte 5" 0x02 (Bytes.get_uint8 encoded 5);
  (* Bytes 6-7: length = 0x0064 = 100 *)
  Alcotest.(check int) "len byte 6" 0x00 (Bytes.get_uint8 encoded 6);
  Alcotest.(check int) "len byte 7" 0x64 (Bytes.get_uint8 encoded 7)

let test_byte_layout_responder () =
  (* Responder sending chain-sync (id=2), 100 bytes, timestamp=0 *)
  let hdr = Mux.{
    timestamp = 0l;
    protocol_id = 2;
    payload_length = 100;
    from_initiator = false;
  } in
  let encoded = Mux.encode_segment_header hdr in
  (* Bytes 4-5: protocol_id = 0x8002 (direction bit set) *)
  Alcotest.(check int) "pid byte 4 (dir bit)" 0x80 (Bytes.get_uint8 encoded 4);
  Alcotest.(check int) "pid byte 5" 0x02 (Bytes.get_uint8 encoded 5)

let test_byte_layout_timestamp () =
  let hdr = Mux.{
    timestamp = 0x12345678l;
    protocol_id = 0;
    payload_length = 0;
    from_initiator = true;
  } in
  let encoded = Mux.encode_segment_header hdr in
  Alcotest.(check int) "ts[0]=0x12" 0x12 (Bytes.get_uint8 encoded 0);
  Alcotest.(check int) "ts[1]=0x34" 0x34 (Bytes.get_uint8 encoded 1);
  Alcotest.(check int) "ts[2]=0x56" 0x56 (Bytes.get_uint8 encoded 2);
  Alcotest.(check int) "ts[3]=0x78" 0x78 (Bytes.get_uint8 encoded 3)

let test_byte_layout_max_payload () =
  let hdr = Mux.{
    timestamp = 0l;
    protocol_id = 8;
    payload_length = 65535;
    from_initiator = true;
  } in
  let encoded = Mux.encode_segment_header hdr in
  (* Bytes 6-7: 0xFFFF *)
  Alcotest.(check int) "len byte 6" 0xFF (Bytes.get_uint8 encoded 6);
  Alcotest.(check int) "len byte 7" 0xFF (Bytes.get_uint8 encoded 7)

(* ================================================================ *)
(* Direction bit handling                                             *)
(* ================================================================ *)

let test_direction_bit_all_protocols () =
  (* Verify direction bit works for all known protocol IDs *)
  let pids = [0; 2; 3; 4; 5; 6; 7; 8; 9] in
  List.iter (fun pid ->
    (* Initiator: direction bit = 0 *)
    let hdr_init = Mux.{
      timestamp = 0l; protocol_id = pid;
      payload_length = 10; from_initiator = true;
    } in
    let enc = Mux.encode_segment_header hdr_init in
    let wire_id = (Bytes.get_uint8 enc 4 lsl 8) lor Bytes.get_uint8 enc 5 in
    Alcotest.(check int)
      (Printf.sprintf "pid %d initiator: no direction bit" pid)
      pid wire_id;
    (* Responder: direction bit = 1 *)
    let hdr_resp = Mux.{ hdr_init with from_initiator = false } in
    let enc = Mux.encode_segment_header hdr_resp in
    let wire_id = (Bytes.get_uint8 enc 4 lsl 8) lor Bytes.get_uint8 enc 5 in
    Alcotest.(check int)
      (Printf.sprintf "pid %d responder: direction bit set" pid)
      (pid lor 0x8000) wire_id;
    (* Round-trip responder *)
    (match Mux.decode_segment_header enc with
     | Error e -> Alcotest.fail e
     | Ok decoded ->
       Alcotest.(check int)
         (Printf.sprintf "pid %d responder decode: protocol_id" pid)
         pid decoded.protocol_id;
       Alcotest.(check bool)
         (Printf.sprintf "pid %d responder decode: from_initiator" pid)
         false decoded.from_initiator)
  ) pids

let test_decode_raw_initiator_bytes () =
  (* Manually construct bytes: ts=0, pid=chain_sync from initiator, len=50 *)
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 0; Bytes.set_uint8 buf 1 0;
  Bytes.set_uint8 buf 2 0; Bytes.set_uint8 buf 3 0;
  Bytes.set_uint8 buf 4 0x00; Bytes.set_uint8 buf 5 0x02; (* pid=2, no dir bit *)
  Bytes.set_uint8 buf 6 0x00; Bytes.set_uint8 buf 7 0x32; (* len=50 *)
  match Mux.decode_segment_header buf with
  | Error e -> Alcotest.fail e
  | Ok hdr ->
    Alcotest.(check int) "protocol_id" 2 hdr.protocol_id;
    Alcotest.(check bool) "from_initiator" true hdr.from_initiator;
    Alcotest.(check int) "payload_length" 50 hdr.payload_length

let test_decode_raw_responder_bytes () =
  (* pid=chain_sync from responder: 0x8002 *)
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 0; Bytes.set_uint8 buf 1 0;
  Bytes.set_uint8 buf 2 0; Bytes.set_uint8 buf 3 0;
  Bytes.set_uint8 buf 4 0x80; Bytes.set_uint8 buf 5 0x02;
  Bytes.set_uint8 buf 6 0x00; Bytes.set_uint8 buf 7 0x32;
  match Mux.decode_segment_header buf with
  | Error e -> Alcotest.fail e
  | Ok hdr ->
    Alcotest.(check int) "protocol_id" 2 hdr.protocol_id;
    Alcotest.(check bool) "from_initiator" false hdr.from_initiator;
    Alcotest.(check int) "payload_length" 50 hdr.payload_length

(* ================================================================ *)
(* Error cases                                                       *)
(* ================================================================ *)

let test_decode_too_short () =
  let buf = Bytes.create 7 in
  match Mux.decode_segment_header buf with
  | Ok _ -> Alcotest.fail "expected error for short buffer"
  | Error _ -> ()

(* ================================================================ *)
(* Encode/decode with full segment payload                           *)
(* ================================================================ *)

let test_full_segment_encode () =
  (* Verify we get correct total bytes: header + payload *)
  let hdr = Mux.{
    timestamp = 500l;
    protocol_id = Miniprotocol.tx_submission;
    payload_length = 5;
    from_initiator = true;
  } in
  let encoded_hdr = Mux.encode_segment_header hdr in
  let payload = Bytes.of_string "hello" in
  (* Total frame = 8 + 5 = 13 bytes *)
  Alcotest.(check int) "header len" 8 (Bytes.length encoded_hdr);
  Alcotest.(check int) "payload len" 5 (Bytes.length payload);
  (* Verify payload_length in header matches *)
  match Mux.decode_segment_header encoded_hdr with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.(check int) "decoded payload len" 5 decoded.payload_length;
    Alcotest.check bytes_testable "payload unchanged" payload (Bytes.of_string "hello")

(* ================================================================ *)
(* Mini-protocol name lookup                                         *)
(* ================================================================ *)

let test_protocol_names () =
  Alcotest.(check string) "handshake" "handshake"
    (Miniprotocol.protocol_name Miniprotocol.handshake);
  Alcotest.(check string) "chain-sync" "chain-sync"
    (Miniprotocol.protocol_name Miniprotocol.chain_sync);
  Alcotest.(check string) "block-fetch" "block-fetch"
    (Miniprotocol.protocol_name Miniprotocol.block_fetch);
  Alcotest.(check string) "keep-alive" "keep-alive"
    (Miniprotocol.protocol_name Miniprotocol.keep_alive);
  Alcotest.(check string) "unknown" "unknown(99)"
    (Miniprotocol.protocol_name 99)

(* ================================================================ *)
(* Negative timestamp (wrapping 32-bit) *)
(* ================================================================ *)

let test_header_negative_timestamp () =
  (* 0xFFFFFFFF as int32 is -1, should round-trip correctly *)
  let hdr = Mux.{
    timestamp = -1l;
    protocol_id = 2;
    payload_length = 0;
    from_initiator = true;
  } in
  let encoded = Mux.encode_segment_header hdr in
  Alcotest.(check int) "ts[0]=0xFF" 0xFF (Bytes.get_uint8 encoded 0);
  Alcotest.(check int) "ts[1]=0xFF" 0xFF (Bytes.get_uint8 encoded 1);
  Alcotest.(check int) "ts[2]=0xFF" 0xFF (Bytes.get_uint8 encoded 2);
  Alcotest.(check int) "ts[3]=0xFF" 0xFF (Bytes.get_uint8 encoded 3);
  match Mux.decode_segment_header encoded with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.check segment_header_testable "negative ts round-trip" hdr decoded

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Mux"
    [ ( "Header round-trip",
        [ Alcotest.test_case "initiator" `Quick test_header_roundtrip_initiator;
          Alcotest.test_case "responder" `Quick test_header_roundtrip_responder;
          Alcotest.test_case "zero values" `Quick test_header_roundtrip_zero;
          Alcotest.test_case "keep-alive" `Quick test_header_roundtrip_keep_alive;
          Alcotest.test_case "negative timestamp" `Quick test_header_negative_timestamp ] );
      ( "Byte layout",
        [ Alcotest.test_case "initiator layout" `Quick test_byte_layout_initiator;
          Alcotest.test_case "responder layout" `Quick test_byte_layout_responder;
          Alcotest.test_case "timestamp bytes" `Quick test_byte_layout_timestamp;
          Alcotest.test_case "max payload" `Quick test_byte_layout_max_payload;
          Alcotest.test_case "full segment" `Quick test_full_segment_encode ] );
      ( "Direction bit",
        [ Alcotest.test_case "all protocols" `Quick test_direction_bit_all_protocols;
          Alcotest.test_case "raw initiator" `Quick test_decode_raw_initiator_bytes;
          Alcotest.test_case "raw responder" `Quick test_decode_raw_responder_bytes ] );
      ( "Error cases",
        [ Alcotest.test_case "too short" `Quick test_decode_too_short ] );
      ( "Protocol names",
        [ Alcotest.test_case "name lookup" `Quick test_protocol_names ] );
    ]
