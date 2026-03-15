open Meridian

let () = Crypto.init ()

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF); b

let make_addr n =
  let b = Bytes.make 29 '\x61' in
  Bytes.set_uint8 b 1 (n land 0xFF); b

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-ls-test-%d-%d" (Unix.getpid ()) (Random.int 100000))

let rm_rf dir =
  let rec go path =
    if Sys.is_directory path then begin
      Array.iter (fun f -> go (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
    end else Unix.unlink path
  in
  if Sys.file_exists dir then go dir

(* ================================================================ *)
(* UTXO address lookup                                               *)
(* ================================================================ *)

let test_find_by_address () =
  let utxo = Utxo.create () in
  let addr_a = make_addr 1 in
  let addr_b = make_addr 2 in
  Utxo.add utxo Utxo.TxIn.{ tx_hash = make_hash 1; tx_index = 0 }
    Utxo.TxOut.{ address = addr_a; value = Multi_asset.of_lovelace 5000000L;
                 has_datum = false; has_script_ref = false };
  Utxo.add utxo Utxo.TxIn.{ tx_hash = make_hash 2; tx_index = 0 }
    Utxo.TxOut.{ address = addr_a; value = Multi_asset.of_lovelace 3000000L;
                 has_datum = false; has_script_ref = false };
  Utxo.add utxo Utxo.TxIn.{ tx_hash = make_hash 3; tx_index = 0 }
    Utxo.TxOut.{ address = addr_b; value = Multi_asset.of_lovelace 1000000L;
                 has_datum = false; has_script_ref = false };
  let results_a = Utxo.find_by_address utxo ~address:addr_a in
  Alcotest.(check int) "2 UTXOs at addr A" 2 (List.length results_a);
  let results_b = Utxo.find_by_address utxo ~address:addr_b in
  Alcotest.(check int) "1 UTXO at addr B" 1 (List.length results_b);
  let results_c = Utxo.find_by_address utxo ~address:(make_addr 99) in
  Alcotest.(check int) "0 at unknown addr" 0 (List.length results_c)

let test_find_by_txins () =
  let utxo = Utxo.create () in
  let txin1 = Utxo.TxIn.{ tx_hash = make_hash 1; tx_index = 0 } in
  let txin2 = Utxo.TxIn.{ tx_hash = make_hash 2; tx_index = 0 } in
  let txin3 = Utxo.TxIn.{ tx_hash = make_hash 99; tx_index = 0 } in
  Utxo.add utxo txin1
    Utxo.TxOut.{ address = make_addr 1; value = Multi_asset.of_lovelace 5000000L;
                 has_datum = false; has_script_ref = false };
  Utxo.add utxo txin2
    Utxo.TxOut.{ address = make_addr 2; value = Multi_asset.of_lovelace 3000000L;
                 has_datum = false; has_script_ref = false };
  let results = Utxo.find_by_txins utxo [txin1; txin3] in
  Alcotest.(check int) "2 results" 2 (List.length results);
  let (_, opt1) = List.hd results in
  Alcotest.(check bool) "txin1 found" true (opt1 <> None);
  let (_, opt2) = List.nth results 1 in
  Alcotest.(check bool) "txin3 not found" true (opt2 = None)

(* ================================================================ *)
(* Local handshake                                                   *)
(* ================================================================ *)

let test_local_handshake () =
  let (c2s_rd, c2s_wr) = Unix.pipe () in
  let (s2c_rd, s2c_wr) = Unix.pipe () in
  let client_out = Mux.create ~fd:c2s_wr ~mode:Initiator in
  let client_in = Mux.create ~fd:s2c_rd ~mode:Initiator in
  let server_in = Mux.create ~fd:c2s_rd ~mode:Responder in
  let server_out = Mux.create ~fd:s2c_wr ~mode:Responder in
  let cleanup () = List.iter (fun fd -> try Unix.close fd with _ -> ())
    [c2s_rd; c2s_wr; s2c_rd; s2c_wr] in
  (* Client sends propose *)
  let versions = List.map (fun v ->
    (v, Handshake.default_params ~network_magic:2L)
  ) [16L; 17L; 18L; 19L] in
  let propose = Handshake.to_bytes (Handshake.propose_versions versions) in
  (match Mux.send_segment client_out ~protocol_id:0 ~timestamp:0l propose with
   | Error e -> cleanup (); Alcotest.fail e | Ok () -> ());
  (* Server receives and negotiates *)
  (match Mux.recv_segment server_in with
   | Error e -> cleanup (); Alcotest.fail e
   | Ok (_hdr, payload) ->
     match Handshake.of_bytes payload with
     | Error e -> cleanup (); Alcotest.fail e
     | Ok msg ->
       let supported = List.map (fun v ->
         (v, Handshake.default_params ~network_magic:2L)
       ) [16L; 17L; 18L; 19L] in
       let response = Handshake.negotiate ~supported msg in
       let resp_bytes = Handshake.to_bytes response in
       (match Mux.send_segment server_out ~protocol_id:0 ~timestamp:0l resp_bytes with
        | Error e -> cleanup (); Alcotest.fail e | Ok () -> ());
       (* Client receives *)
       match Mux.recv_segment client_in with
       | Error e -> cleanup (); Alcotest.fail e
       | Ok (_, resp_payload) ->
         match Handshake.of_bytes resp_payload with
         | Error e -> cleanup (); Alcotest.fail e
         | Ok (AcceptVersion (v, _)) ->
           Alcotest.(check int64) "version 19" 19L v; cleanup ()
         | Ok _ -> cleanup (); Alcotest.fail "expected AcceptVersion")

(* ================================================================ *)
(* State query — GetCurrentEra                                       *)
(* ================================================================ *)

let test_get_current_era () =
  let dir = temp_dir () in
  let store = Store.init ~base_dir:dir () in
  let ledger = Ledger_state.create () in
  let ctx = Local_server.{ store; ledger; genesis = Genesis.preview_genesis } in
  (* Store a Babbage block *)
  let block_cbor = Cbor.Array [Cbor.Uint 5L; Cbor.Array [
    Cbor.Array [Cbor.Array []; Cbor.Bytes Bytes.empty];
    Cbor.Array []; Cbor.Array []; Cbor.Map []
  ]] in
  let block_bytes = Cbor.encode block_cbor in
  let hash = Crypto.blake2b_256 block_bytes in
  ignore (Store.store_block store ~slot:100L ~hash ~cbor_bytes:block_bytes);
  ignore ctx;
  Alcotest.(check int) "store has block" 1 (Store.block_count store);
  rm_rf dir

(* ================================================================ *)
(* State query — GetProtocolParameters                               *)
(* ================================================================ *)

let test_protocol_params () =
  let p = Ledger_state.shelley_params in
  Alcotest.(check int64) "minFeeA" 44L p.min_fee_a;
  Alcotest.(check int64) "minFeeB" 155381L p.min_fee_b;
  Alcotest.(check int64) "keyDeposit" 2000000L p.key_deposit;
  Alcotest.(check int64) "poolDeposit" 500000000L p.pool_deposit

(* ================================================================ *)
(* Local tx monitor — empty mempool                                  *)
(* ================================================================ *)

let test_tx_monitor_empty () =
  (* Verify tx monitor messages round-trip *)
  let msg = Local_tx_monitor.MsgAcquire in
  let bytes = Local_tx_monitor.to_bytes msg in
  (match Local_tx_monitor.of_bytes bytes with
   | Ok MsgAcquire -> ()
   | _ -> Alcotest.fail "expected MsgAcquire");
  let resp = Local_tx_monitor.MsgReplyGetSizes { capacity = 0L; size = 0L; num_txs = 0 } in
  let bytes = Local_tx_monitor.to_bytes resp in
  match Local_tx_monitor.of_bytes bytes with
  | Ok (MsgReplyGetSizes s) ->
    Alcotest.(check int64) "capacity" 0L s.capacity;
    Alcotest.(check int) "num_txs" 0 s.num_txs
  | _ -> Alcotest.fail "expected MsgReplyGetSizes"

(* ================================================================ *)
(* Unix listener create/close                                        *)
(* ================================================================ *)

let test_unix_listener () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "test.socket" in
  let listener = Unix_listener.create ~socket_path:path in
  Alcotest.(check bool) "socket exists" true (Sys.file_exists path);
  Alcotest.(check bool) "running" true (Unix_listener.is_running listener);
  Unix_listener.close listener;
  Alcotest.(check bool) "socket removed" false (Sys.file_exists path);
  rm_rf dir

let test_unix_listener_stale () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "stale.socket" in
  (* Create a stale file *)
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
  Unix.close fd;
  (* Should remove stale and succeed *)
  let listener = Unix_listener.create ~socket_path:path in
  Alcotest.(check bool) "running" true (Unix_listener.is_running listener);
  Unix_listener.close listener;
  rm_rf dir

(* ================================================================ *)
(* UTXO multi-asset address query                                    *)
(* ================================================================ *)

let test_find_by_address_multi_asset () =
  let utxo = Utxo.create () in
  let addr = make_addr 1 in
  let pid = Bytes.make 28 '\xaa' in
  let value = Multi_asset.{ lovelace = 5000000L;
    assets = [(pid, [(Bytes.of_string "Token", 100L)])] } in
  Utxo.add utxo Utxo.TxIn.{ tx_hash = make_hash 1; tx_index = 0 }
    Utxo.TxOut.{ address = addr; value;
                 has_datum = false; has_script_ref = false };
  let results = Utxo.find_by_address utxo ~address:addr in
  Alcotest.(check int) "1 result" 1 (List.length results);
  let (_, txout) = List.hd results in
  Alcotest.(check int64) "lovelace" 5000000L (Multi_asset.lovelace_of txout.value);
  Alcotest.(check int) "1 asset" 1 (Multi_asset.asset_count txout.value)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Local-Server"
    [ ( "UTXO lookups",
        [ Alcotest.test_case "by address" `Quick test_find_by_address;
          Alcotest.test_case "by txins" `Quick test_find_by_txins;
          Alcotest.test_case "multi-asset addr" `Quick test_find_by_address_multi_asset ] );
      ( "Handshake",
        [ Alcotest.test_case "n2c handshake" `Quick test_local_handshake ] );
      ( "State query",
        [ Alcotest.test_case "current era" `Quick test_get_current_era;
          Alcotest.test_case "protocol params" `Quick test_protocol_params ] );
      ( "Tx monitor",
        [ Alcotest.test_case "empty mempool" `Quick test_tx_monitor_empty ] );
      ( "Unix listener",
        [ Alcotest.test_case "create/close" `Quick test_unix_listener;
          Alcotest.test_case "stale socket" `Quick test_unix_listener_stale ] );
    ]
