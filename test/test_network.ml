open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let bytes_testable =
  Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

(* ================================================================ *)
(* Segment framing round-trips                                       *)
(* ================================================================ *)

(** Build a complete segment (header + payload) from parts, then
    parse it back and verify all fields match. *)
let test_segment_frame_roundtrip () =
  let payload = Bytes.of_string "hello mux" in
  let hdr = Mux.{
    timestamp = 12345l;
    protocol_id = Miniprotocol.handshake;
    payload_length = Bytes.length payload;
    from_initiator = true;
  } in
  let hdr_bytes = Mux.encode_segment_header hdr in
  (* Concatenate header + payload to simulate wire data *)
  let frame = Bytes.create (8 + Bytes.length payload) in
  Bytes.blit hdr_bytes 0 frame 0 8;
  Bytes.blit payload 0 frame 8 (Bytes.length payload);
  (* Parse header back *)
  match Mux.decode_segment_header (Bytes.sub frame 0 8) with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.(check int32) "timestamp" 12345l decoded.timestamp;
    Alcotest.(check int) "protocol_id" 0 decoded.protocol_id;
    Alcotest.(check int) "payload_length" 9 decoded.payload_length;
    Alcotest.(check bool) "from_initiator" true decoded.from_initiator;
    (* Extract payload *)
    let extracted = Bytes.sub frame 8 decoded.payload_length in
    Alcotest.check bytes_testable "payload" payload extracted

let test_segment_responder_frame () =
  let payload = Bytes.of_string "response" in
  let hdr = Mux.{
    timestamp = 0l;
    protocol_id = Miniprotocol.handshake;
    payload_length = Bytes.length payload;
    from_initiator = false;
  } in
  let hdr_bytes = Mux.encode_segment_header hdr in
  match Mux.decode_segment_header hdr_bytes with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.(check bool) "from_initiator" false decoded.from_initiator;
    Alcotest.(check int) "protocol_id" 0 decoded.protocol_id

let test_segment_chain_sync_frame () =
  let hdr = Mux.{
    timestamp = 999l;
    protocol_id = Miniprotocol.chain_sync;
    payload_length = 1024;
    from_initiator = true;
  } in
  let encoded = Mux.encode_segment_header hdr in
  match Mux.decode_segment_header encoded with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.(check int) "chain_sync id" 2 decoded.protocol_id;
    Alcotest.(check int) "length" 1024 decoded.payload_length

(* ================================================================ *)
(* Handshake message wire format                                     *)
(* ================================================================ *)

(** Verify that a ProposeVersions message encodes to valid CBOR that
    could be sent over the wire. *)
let test_handshake_propose_wire_format () =
  let versions = [
    (13L, Handshake.default_params ~network_magic:Handshake.preview_magic);
  ] in
  let msg = Handshake.propose_versions versions in
  let payload = Handshake.to_bytes msg in
  (* Payload should be valid CBOR *)
  (match Cbor.decode payload with
   | Error e -> Alcotest.fail (Printf.sprintf "not valid CBOR: %s" e)
   | Ok cbor ->
     (* Should be [0, {version => params}] *)
     match cbor with
     | Cbor.Array [Cbor.Uint 0L; Cbor.Map entries] ->
       Alcotest.(check int) "one version" 1 (List.length entries);
       (match entries with
        | [(Cbor.Uint 13L, _)] -> ()
        | _ -> Alcotest.fail "expected version 13 entry")
     | _ -> Alcotest.fail "expected [0, map]")

let test_handshake_accept_wire_format () =
  let params = Handshake.default_params ~network_magic:Handshake.preview_magic in
  let msg = Handshake.AcceptVersion (13L, params) in
  let payload = Handshake.to_bytes msg in
  match Cbor.decode payload with
  | Error e -> Alcotest.fail e
  | Ok (Cbor.Array [Cbor.Uint 1L; Cbor.Uint 13L; _]) -> ()
  | Ok _ -> Alcotest.fail "expected [1, 13, params]"

(** Simulate a full handshake exchange: encode propose, decode it,
    negotiate on server side, encode response, decode on client side. *)
let test_handshake_full_wire_simulation () =
  let client_versions = [
    (10L, Handshake.default_params ~network_magic:Handshake.preview_magic);
    (13L, Handshake.default_params ~network_magic:Handshake.preview_magic);
  ] in
  let server_versions = [
    (11L, Handshake.default_params ~network_magic:Handshake.preview_magic);
    (13L, Handshake.default_params ~network_magic:Handshake.preview_magic);
  ] in
  (* Client: encode ProposeVersions *)
  let propose_payload = Handshake.to_bytes (Handshake.propose_versions client_versions) in
  (* Wrap in mux segment *)
  let hdr = Mux.encode_segment_header Mux.{
    timestamp = 0l;
    protocol_id = Miniprotocol.handshake;
    payload_length = Bytes.length propose_payload;
    from_initiator = true;
  } in
  (* Simulate wire: header + payload *)
  let wire_propose = Bytes.create (8 + Bytes.length propose_payload) in
  Bytes.blit hdr 0 wire_propose 0 8;
  Bytes.blit propose_payload 0 wire_propose 8 (Bytes.length propose_payload);
  (* Server: decode header *)
  (match Mux.decode_segment_header (Bytes.sub wire_propose 0 8) with
   | Error e -> Alcotest.fail e
   | Ok seg_hdr ->
     Alcotest.(check int) "handshake protocol" 0 seg_hdr.protocol_id;
     let payload = Bytes.sub wire_propose 8 seg_hdr.payload_length in
     (* Server: decode ProposeVersions *)
     match Handshake.of_bytes payload with
     | Error e -> Alcotest.fail e
     | Ok propose_msg ->
       (* Server: negotiate *)
       let response = Handshake.negotiate ~supported:server_versions propose_msg in
       let response_payload = Handshake.to_bytes response in
       (* Wrap in mux segment (responder direction) *)
       let resp_hdr = Mux.encode_segment_header Mux.{
         timestamp = 1l;
         protocol_id = Miniprotocol.handshake;
         payload_length = Bytes.length response_payload;
         from_initiator = false;
       } in
       (* Simulate wire *)
       let wire_response = Bytes.create (8 + Bytes.length response_payload) in
       Bytes.blit resp_hdr 0 wire_response 0 8;
       Bytes.blit response_payload 0 wire_response 8 (Bytes.length response_payload);
       (* Client: decode response header *)
       match Mux.decode_segment_header (Bytes.sub wire_response 0 8) with
       | Error e -> Alcotest.fail e
       | Ok resp_seg_hdr ->
         Alcotest.(check bool) "responder direction" false resp_seg_hdr.from_initiator;
         let resp_bytes = Bytes.sub wire_response 8 resp_seg_hdr.payload_length in
         match Handshake.of_bytes resp_bytes with
         | Error e -> Alcotest.fail e
         | Ok resp_msg ->
           match Handshake.handle_response ~supported:client_versions resp_msg with
           | Error e -> Alcotest.fail e
           | Ok (version, params) ->
             Alcotest.(check int64) "negotiated v13" 13L version;
             Alcotest.(check int64) "preview magic" 2L params.Handshake.network_magic)

(* ================================================================ *)
(* TCP connection error handling                                     *)
(* ================================================================ *)

let test_connect_invalid_host () =
  match Tcp_connection.connect ~timeout_s:2.0 ~host:"this.host.does.not.exist.example.invalid" ~port:3001 () with
  | Error (Dns_error _) -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "expected Dns_error, got: %s"
                                (Tcp_connection.error_to_string e))
  | Ok conn -> Tcp_connection.close conn; Alcotest.fail "expected connection failure"

let test_connect_refused_port () =
  (* Connect to localhost on a port nothing listens on *)
  match Tcp_connection.connect ~timeout_s:2.0 ~host:"127.0.0.1" ~port:19999 () with
  | Error (Connection_refused _) -> ()
  | Error (Network_error _) -> ()  (* also acceptable *)
  | Error e -> Alcotest.fail (Printf.sprintf "expected refusal, got: %s"
                                (Tcp_connection.error_to_string e))
  | Ok conn -> Tcp_connection.close conn; Alcotest.fail "expected connection failure"

let test_error_to_string () =
  let s = Tcp_connection.error_to_string Tcp_connection.Timeout in
  Alcotest.(check bool) "timeout string" true (String.length s > 0);
  let s = Tcp_connection.error_to_string (Dns_error "test") in
  Alcotest.(check bool) "dns string" true (String.length s > 0)

(* ================================================================ *)
(* Mux segment over pipe (simulated connection)                      *)
(* ================================================================ *)

let test_mux_over_pipe () =
  (* Create a Unix pipe to simulate a connection *)
  let (rd, wr) = Unix.pipe () in
  let mux_writer = Mux.create ~fd:wr ~mode:Initiator in
  let mux_reader = Mux.create ~fd:rd ~mode:Responder in
  let payload = Bytes.of_string "test payload" in
  (* Send a segment *)
  (match Mux.send_segment mux_writer
           ~protocol_id:Miniprotocol.handshake ~timestamp:42l payload with
   | Error e -> Unix.close rd; Unix.close wr; Alcotest.fail e
   | Ok () ->
     (* Receive the segment *)
     match Mux.recv_segment mux_reader with
     | Error e -> Unix.close rd; Unix.close wr; Alcotest.fail e
     | Ok (hdr, recv_payload) ->
       Alcotest.(check int32) "timestamp" 42l hdr.timestamp;
       Alcotest.(check int) "protocol_id" 0 hdr.protocol_id;
       Alcotest.(check bool) "from_initiator" true hdr.from_initiator;
       Alcotest.check bytes_testable "payload" payload recv_payload;
       Unix.close rd; Unix.close wr)

let test_mux_handshake_over_pipe () =
  (* Full handshake over a pipe pair *)
  let (c2s_rd, c2s_wr) = Unix.pipe () in
  let (s2c_rd, s2c_wr) = Unix.pipe () in
  (* Client writes to c2s_wr, reads from s2c_rd *)
  let client_out = Mux.create ~fd:c2s_wr ~mode:Initiator in
  let client_in = Mux.create ~fd:s2c_rd ~mode:Initiator in
  (* Server reads from c2s_rd, writes to s2c_wr *)
  let server_in = Mux.create ~fd:c2s_rd ~mode:Responder in
  let server_out = Mux.create ~fd:s2c_wr ~mode:Responder in
  let versions = [
    (13L, Handshake.default_params ~network_magic:Handshake.preview_magic);
  ] in
  let cleanup () =
    List.iter (fun fd -> try Unix.close fd with _ -> ())
      [c2s_rd; c2s_wr; s2c_rd; s2c_wr]
  in
  (* Client sends propose *)
  let propose = Handshake.to_bytes (Handshake.propose_versions versions) in
  (match Mux.send_segment client_out
           ~protocol_id:Miniprotocol.handshake ~timestamp:0l propose with
   | Error e -> cleanup (); Alcotest.fail e
   | Ok () ->
     (* Server receives *)
     match Mux.recv_segment server_in with
     | Error e -> cleanup (); Alcotest.fail e
     | Ok (_, payload) ->
       match Handshake.of_bytes payload with
       | Error e -> cleanup (); Alcotest.fail e
       | Ok msg ->
         let response = Handshake.negotiate ~supported:versions msg in
         let resp_bytes = Handshake.to_bytes response in
         (* Server sends response *)
         (match Mux.send_segment server_out
                  ~protocol_id:Miniprotocol.handshake ~timestamp:1l resp_bytes with
          | Error e -> cleanup (); Alcotest.fail e
          | Ok () ->
            (* Client receives *)
            match Mux.recv_segment client_in with
            | Error e -> cleanup (); Alcotest.fail e
            | Ok (_, resp_payload) ->
              match Handshake.of_bytes resp_payload with
              | Error e -> cleanup (); Alcotest.fail e
              | Ok resp ->
                match Handshake.handle_response ~supported:versions resp with
                | Error e -> cleanup (); Alcotest.fail e
                | Ok (v, p) ->
                  Alcotest.(check int64) "version" 13L v;
                  Alcotest.(check int64) "magic" 2L p.Handshake.network_magic;
                  cleanup ()))

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Network"
    [ ( "Segment framing",
        [ Alcotest.test_case "initiator roundtrip" `Quick test_segment_frame_roundtrip;
          Alcotest.test_case "responder frame" `Quick test_segment_responder_frame;
          Alcotest.test_case "chain-sync frame" `Quick test_segment_chain_sync_frame ] );
      ( "Handshake wire format",
        [ Alcotest.test_case "propose format" `Quick test_handshake_propose_wire_format;
          Alcotest.test_case "accept format" `Quick test_handshake_accept_wire_format;
          Alcotest.test_case "full simulation" `Quick test_handshake_full_wire_simulation ] );
      ( "TCP errors",
        [ Alcotest.test_case "invalid host" `Quick test_connect_invalid_host;
          Alcotest.test_case "refused port" `Quick test_connect_refused_port;
          Alcotest.test_case "error strings" `Quick test_error_to_string ] );
      ( "Mux over pipe",
        [ Alcotest.test_case "send/recv segment" `Quick test_mux_over_pipe;
          Alcotest.test_case "full handshake" `Quick test_mux_handshake_over_pipe ] );
    ]
