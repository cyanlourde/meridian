open Meridian

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let bytes_testable =
  Alcotest.testable (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b)) Bytes.equal

let check_roundtrip name msg =
  let raw1 = Tx_submission.to_bytes msg in
  match Tx_submission.of_bytes raw1 with
  | Error e -> Alcotest.fail (Printf.sprintf "decode: %s" e)
  | Ok msg' ->
    let raw2 = Tx_submission.to_bytes msg' in
    Alcotest.check bytes_testable name raw1 raw2

let txid1 () = Bytes.make 32 '\x01'
let txid2 () = Bytes.make 32 '\x02'

(* ---- Round-trips ---- *)

let test_init () =
  check_roundtrip "MsgInit" MsgInit

let test_request_tx_ids_blocking () =
  check_roundtrip "MsgRequestTxIds blocking"
    (MsgRequestTxIds { blocking = true; ack_count = 0; req_count = 3 })

let test_request_tx_ids_non_blocking () =
  check_roundtrip "MsgRequestTxIds non-blocking"
    (MsgRequestTxIds { blocking = false; ack_count = 2; req_count = 5 })

let test_reply_tx_ids () =
  check_roundtrip "MsgReplyTxIds"
    (MsgReplyTxIds [(txid1 (), 250); (txid2 (), 400)])

let test_reply_tx_ids_empty () =
  check_roundtrip "MsgReplyTxIds empty" (MsgReplyTxIds [])

let test_request_txs () =
  check_roundtrip "MsgRequestTxs" (MsgRequestTxs [txid1 (); txid2 ()])

let test_request_txs_empty () =
  check_roundtrip "MsgRequestTxs empty" (MsgRequestTxs [])

let test_reply_txs () =
  check_roundtrip "MsgReplyTxs"
    (MsgReplyTxs [Bytes.make 100 '\xaa'; Bytes.make 200 '\xbb'])

let test_reply_txs_empty () =
  check_roundtrip "MsgReplyTxs empty" (MsgReplyTxs [])

let test_done () =
  check_roundtrip "MsgDone" MsgDone

(* ---- CBOR tags ---- *)

let test_tags () =
  let check tag msg =
    match Tx_submission.encode_message msg with
    | Cbor.Array (Cbor.Uint t :: _) ->
      Alcotest.(check int64) (Printf.sprintf "tag %Ld" tag) tag t
    | _ -> Alcotest.fail "expected [tag, ...]"
  in
  check 0L (MsgRequestTxIds { blocking = true; ack_count = 0; req_count = 1 });
  check 1L (MsgReplyTxIds []);
  check 2L (MsgRequestTxs []);
  check 3L (MsgReplyTxs []);
  check 4L MsgDone;
  check 6L MsgInit

(* ---- State transitions ---- *)

let test_init_to_idle () =
  match Tx_submission.transition StInit MsgInit with
  | Ok StIdle -> () | _ -> Alcotest.fail "expected StIdle"

let test_idle_to_txids () =
  match Tx_submission.transition StIdle
    (MsgRequestTxIds { blocking = true; ack_count = 0; req_count = 3 }) with
  | Ok StTxIds -> () | _ -> Alcotest.fail "expected StTxIds"

let test_idle_to_txs () =
  match Tx_submission.transition StIdle (MsgRequestTxs [txid1 ()]) with
  | Ok StTxs -> () | _ -> Alcotest.fail "expected StTxs"

let test_txids_reply () =
  match Tx_submission.transition StTxIds (MsgReplyTxIds [(txid1 (), 100)]) with
  | Ok StIdle -> () | _ -> Alcotest.fail "expected StIdle"

let test_txids_done () =
  match Tx_submission.transition StTxIds MsgDone with
  | Ok StDone -> () | _ -> Alcotest.fail "expected StDone"

let test_txs_reply () =
  match Tx_submission.transition StTxs (MsgReplyTxs [Bytes.empty]) with
  | Ok StIdle -> () | _ -> Alcotest.fail "expected StIdle"

let test_txs_done () =
  match Tx_submission.transition StTxs MsgDone with
  | Ok StDone -> () | _ -> Alcotest.fail "expected StDone"

let test_invalid_init_in_idle () =
  match Tx_submission.transition StIdle MsgInit with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

let test_invalid_request_in_init () =
  match Tx_submission.transition StInit
    (MsgRequestTxIds { blocking = true; ack_count = 0; req_count = 1 }) with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

let test_invalid_in_done () =
  match Tx_submission.transition StDone MsgInit with
  | Error _ -> () | Ok _ -> Alcotest.fail "expected error"

(* ---- Full sequence ---- *)

let test_full_sequence () =
  let check s m = match Tx_submission.transition s m with
    | Ok s' -> s' | Error e -> Alcotest.fail e
  in
  let s = Tx_submission.StInit in
  let s = check s MsgInit in
  Alcotest.(check string) "idle" "StIdle" (Tx_submission.state_name s);
  let s = check s (MsgRequestTxIds { blocking = true; ack_count = 0; req_count = 2 }) in
  let s = check s (MsgReplyTxIds [(txid1 (), 100); (txid2 (), 200)]) in
  Alcotest.(check string) "idle again" "StIdle" (Tx_submission.state_name s);
  let s = check s (MsgRequestTxs [txid1 ()]) in
  let s = check s (MsgReplyTxs [Bytes.make 100 '\xaa']) in
  Alcotest.(check string) "idle again 2" "StIdle" (Tx_submission.state_name s);
  let s = check s (MsgRequestTxIds { blocking = false; ack_count = 1; req_count = 1 }) in
  let s = check s MsgDone in
  Alcotest.(check string) "done" "StDone" (Tx_submission.state_name s)

(* ---- Agency ---- *)

let test_agency () =
  Alcotest.(check bool) "init=client" true
    (Tx_submission.agency_of StInit = Client_agency);
  Alcotest.(check bool) "idle=server" true
    (Tx_submission.agency_of StIdle = Server_agency);
  Alcotest.(check bool) "txids=client" true
    (Tx_submission.agency_of StTxIds = Client_agency);
  Alcotest.(check bool) "txs=client" true
    (Tx_submission.agency_of StTxs = Client_agency);
  Alcotest.(check bool) "done=nobody" true
    (Tx_submission.agency_of StDone = Nobody_agency)

let () =
  Alcotest.run "Tx-Submission"
    [ ( "Round-trips",
        [ Alcotest.test_case "Init" `Quick test_init;
          Alcotest.test_case "RequestTxIds blocking" `Quick test_request_tx_ids_blocking;
          Alcotest.test_case "RequestTxIds non-blocking" `Quick test_request_tx_ids_non_blocking;
          Alcotest.test_case "ReplyTxIds" `Quick test_reply_tx_ids;
          Alcotest.test_case "ReplyTxIds empty" `Quick test_reply_tx_ids_empty;
          Alcotest.test_case "RequestTxs" `Quick test_request_txs;
          Alcotest.test_case "RequestTxs empty" `Quick test_request_txs_empty;
          Alcotest.test_case "ReplyTxs" `Quick test_reply_txs;
          Alcotest.test_case "ReplyTxs empty" `Quick test_reply_txs_empty;
          Alcotest.test_case "Done" `Quick test_done ] );
      ( "CBOR tags",
        [ Alcotest.test_case "all tags" `Quick test_tags ] );
      ( "Valid transitions",
        [ Alcotest.test_case "init->idle" `Quick test_init_to_idle;
          Alcotest.test_case "idle->txids" `Quick test_idle_to_txids;
          Alcotest.test_case "idle->txs" `Quick test_idle_to_txs;
          Alcotest.test_case "txids->idle" `Quick test_txids_reply;
          Alcotest.test_case "txids->done" `Quick test_txids_done;
          Alcotest.test_case "txs->idle" `Quick test_txs_reply;
          Alcotest.test_case "txs->done" `Quick test_txs_done ] );
      ( "Invalid transitions",
        [ Alcotest.test_case "init in idle" `Quick test_invalid_init_in_idle;
          Alcotest.test_case "request in init" `Quick test_invalid_request_in_init;
          Alcotest.test_case "anything in done" `Quick test_invalid_in_done ] );
      ( "Full sequence",
        [ Alcotest.test_case "request+reply cycle" `Quick test_full_sequence ] );
      ( "Agency",
        [ Alcotest.test_case "all states" `Quick test_agency ] );
    ]
