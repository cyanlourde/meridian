open Meridian

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let bytes_testable =
  Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

let check_roundtrip name msg =
  let raw1 = Keep_alive.to_bytes msg in
  match Keep_alive.of_bytes raw1 with
  | Error e -> Alcotest.fail (Printf.sprintf "decode: %s" e)
  | Ok msg' ->
    let raw2 = Keep_alive.to_bytes msg' in
    Alcotest.check bytes_testable name raw1 raw2

(* ---- Round-trips ---- *)

let test_keep_alive () =
  check_roundtrip "MsgKeepAlive 0" (MsgKeepAlive 0);
  check_roundtrip "MsgKeepAlive 42" (MsgKeepAlive 42);
  check_roundtrip "MsgKeepAlive max u16" (MsgKeepAlive 65535)

let test_keep_alive_response () =
  check_roundtrip "MsgKeepAliveResponse 0" (MsgKeepAliveResponse 0);
  check_roundtrip "MsgKeepAliveResponse 42" (MsgKeepAliveResponse 42);
  check_roundtrip "MsgKeepAliveResponse max" (MsgKeepAliveResponse 65535)

let test_done () =
  check_roundtrip "MsgDone" MsgDone

(* ---- CBOR tags ---- *)

let test_tags () =
  let check tag msg =
    match Keep_alive.encode_message msg with
    | Cbor.Array (Cbor.Uint t :: _) ->
      Alcotest.(check int64) (Printf.sprintf "tag %Ld" tag) tag t
    | _ -> Alcotest.fail "expected [tag, ...]"
  in
  check 0L (MsgKeepAlive 1);
  check 1L (MsgKeepAliveResponse 1);
  check 2L MsgDone

(* ---- Cookie encoding ---- *)

let test_cookie_encoding () =
  let cbor = Keep_alive.encode_message (MsgKeepAlive 12345) in
  match cbor with
  | Cbor.Array [Cbor.Uint 0L; Cbor.Uint 12345L] -> ()
  | _ -> Alcotest.fail "cookie should be encoded as uint"

let test_cookie_response_matches () =
  (* Verify response cookie round-trips correctly *)
  let raw = Keep_alive.to_bytes (MsgKeepAliveResponse 9999) in
  match Keep_alive.of_bytes raw with
  | Ok (MsgKeepAliveResponse 9999) -> ()
  | Ok _ -> Alcotest.fail "cookie mismatch"
  | Error e -> Alcotest.fail e

(* ---- State transitions ---- *)

let test_client_keep_alive () =
  match Keep_alive.transition StClient (MsgKeepAlive 1) with
  | Ok StServer -> () | _ -> Alcotest.fail "expected StServer"

let test_client_done () =
  match Keep_alive.transition StClient MsgDone with
  | Ok StDone -> () | _ -> Alcotest.fail "expected StDone"

let test_server_response () =
  match Keep_alive.transition StServer (MsgKeepAliveResponse 1) with
  | Ok StClient -> () | _ -> Alcotest.fail "expected StClient"

let test_invalid_response_in_client () =
  match Keep_alive.transition StClient (MsgKeepAliveResponse 1) with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

let test_invalid_keep_alive_in_server () =
  match Keep_alive.transition StServer (MsgKeepAlive 1) with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

let test_invalid_done_in_server () =
  match Keep_alive.transition StServer MsgDone with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

let test_invalid_in_done () =
  match Keep_alive.transition StDone (MsgKeepAlive 1) with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

(* ---- Full sequence ---- *)

let test_ping_pong () =
  let check s m = match Keep_alive.transition s m with
    | Ok s' -> s' | Error e -> Alcotest.fail e
  in
  let s = Keep_alive.StClient in
  let s = check s (MsgKeepAlive 100) in
  Alcotest.(check string) "server" "StServer" (Keep_alive.state_name s);
  let s = check s (MsgKeepAliveResponse 100) in
  Alcotest.(check string) "client" "StClient" (Keep_alive.state_name s);
  let s = check s (MsgKeepAlive 200) in
  let s = check s (MsgKeepAliveResponse 200) in
  let s = check s MsgDone in
  Alcotest.(check string) "done" "StDone" (Keep_alive.state_name s)

(* ---- Agency ---- *)

let test_agency () =
  Alcotest.(check bool) "client=client" true
    (Keep_alive.agency_of StClient = Client_agency);
  Alcotest.(check bool) "server=server" true
    (Keep_alive.agency_of StServer = Server_agency);
  Alcotest.(check bool) "done=nobody" true
    (Keep_alive.agency_of StDone = Nobody_agency)

let () =
  Alcotest.run "Keep-Alive"
    [ ( "Round-trips",
        [ Alcotest.test_case "KeepAlive" `Quick test_keep_alive;
          Alcotest.test_case "KeepAliveResponse" `Quick test_keep_alive_response;
          Alcotest.test_case "Done" `Quick test_done ] );
      ( "CBOR tags",
        [ Alcotest.test_case "all tags" `Quick test_tags ] );
      ( "Cookie encoding",
        [ Alcotest.test_case "uint encoding" `Quick test_cookie_encoding;
          Alcotest.test_case "response matches" `Quick test_cookie_response_matches ] );
      ( "Valid transitions",
        [ Alcotest.test_case "client->server" `Quick test_client_keep_alive;
          Alcotest.test_case "client->done" `Quick test_client_done;
          Alcotest.test_case "server->client" `Quick test_server_response ] );
      ( "Invalid transitions",
        [ Alcotest.test_case "response in client" `Quick test_invalid_response_in_client;
          Alcotest.test_case "keepalive in server" `Quick test_invalid_keep_alive_in_server;
          Alcotest.test_case "done in server" `Quick test_invalid_done_in_server;
          Alcotest.test_case "anything in done" `Quick test_invalid_in_done ] );
      ( "Full sequence",
        [ Alcotest.test_case "ping pong" `Quick test_ping_pong ] );
      ( "Agency",
        [ Alcotest.test_case "all states" `Quick test_agency ] );
    ]
