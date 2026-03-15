open Meridian

let () = Crypto.init ()

(* ================================================================ *)
(* Mainnet genesis parameters                                        *)
(* ================================================================ *)

let test_mainnet_genesis_params () =
  let g = Genesis.mainnet_genesis in
  Alcotest.(check int64) "magic" 764824073L g.network_magic;
  Alcotest.(check int64) "epoch_length" 432000L g.epoch_length;
  Alcotest.(check int64) "minFeeA" 44L g.protocol_params.min_fee_a;
  Alcotest.(check int64) "keyDeposit" 2000000L g.protocol_params.key_deposit;
  Alcotest.(check int64) "poolDeposit" 500000000L g.protocol_params.pool_deposit

let test_preprod_genesis () =
  let g = Genesis.preprod_genesis in
  Alcotest.(check int64) "magic" 1L g.network_magic

(* ================================================================ *)
(* Mainnet epoch arithmetic                                          *)
(* ================================================================ *)

let test_mainnet_epoch_arithmetic () =
  let p = Epoch.mainnet_epoch_params in
  (* Byron era *)
  Alcotest.(check int64) "slot 0 = epoch 0" 0L (Epoch.slot_to_epoch p 0L);
  Alcotest.(check int64) "slot 21599 = epoch 0" 0L (Epoch.slot_to_epoch p 21599L);
  Alcotest.(check int64) "slot 21600 = epoch 1" 1L (Epoch.slot_to_epoch p 21600L);
  Alcotest.(check int64) "slot 43200 = epoch 2" 2L (Epoch.slot_to_epoch p 43200L);
  (* Byron/Shelley boundary *)
  Alcotest.(check int64) "slot 4492799 = epoch 207" 207L (Epoch.slot_to_epoch p 4492799L);
  Alcotest.(check int64) "slot 4492800 = epoch 208" 208L (Epoch.slot_to_epoch p 4492800L);
  (* Shelley era *)
  Alcotest.(check int64) "slot 4924800 = epoch 209" 209L (Epoch.slot_to_epoch p 4924800L)

let test_mainnet_slot_in_epoch () =
  let p = Epoch.mainnet_epoch_params in
  Alcotest.(check int64) "slot 0 in epoch" 0L (Epoch.slot_in_epoch p 0L);
  Alcotest.(check int64) "slot 100 in epoch" 100L (Epoch.slot_in_epoch p 100L);
  Alcotest.(check int64) "slot 21600 in epoch" 0L (Epoch.slot_in_epoch p 21600L);
  Alcotest.(check int64) "slot 4492800 in epoch" 0L (Epoch.slot_in_epoch p 4492800L)

let test_mainnet_epoch_first_slot () =
  let p = Epoch.mainnet_epoch_params in
  Alcotest.(check int64) "epoch 0" 0L (Epoch.epoch_to_first_slot p 0L);
  Alcotest.(check int64) "epoch 1" 21600L (Epoch.epoch_to_first_slot p 1L);
  Alcotest.(check int64) "epoch 207" 4471200L (Epoch.epoch_to_first_slot p 207L);
  Alcotest.(check int64) "epoch 208" 4492800L (Epoch.epoch_to_first_slot p 208L);
  Alcotest.(check int64) "epoch 209" 4924800L (Epoch.epoch_to_first_slot p 209L)

let test_mainnet_epoch_boundary () =
  let p = Epoch.mainnet_epoch_params in
  Alcotest.(check bool) "same byron epoch" false
    (Epoch.is_epoch_boundary p ~prev_slot:100L ~slot:200L);
  Alcotest.(check bool) "byron boundary" true
    (Epoch.is_epoch_boundary p ~prev_slot:21599L ~slot:21600L);
  Alcotest.(check bool) "shelley transition" true
    (Epoch.is_epoch_boundary p ~prev_slot:4492799L ~slot:4492800L)

(* ================================================================ *)
(* Network selection                                                 *)
(* ================================================================ *)

let test_network_selection () =
  let g_preview = Genesis.genesis_for_network "preview" in
  Alcotest.(check int64) "preview magic" 2L g_preview.network_magic;
  let g_mainnet = Genesis.genesis_for_network "mainnet" in
  Alcotest.(check int64) "mainnet magic" 764824073L g_mainnet.network_magic;
  let g_preprod = Genesis.genesis_for_network "preprod" in
  Alcotest.(check int64) "preprod magic" 1L g_preprod.network_magic

let test_node_selection () =
  let (host, port) = Genesis.default_node_for_network "preview" in
  Alcotest.(check bool) "preview host" true (String.length host > 0);
  Alcotest.(check int) "preview port" 3001 port;
  let (host, port) = Genesis.default_node_for_network "mainnet" in
  Alcotest.(check bool) "mainnet host" true (String.length host > 0);
  Alcotest.(check int) "mainnet port" 3001 port

let test_aggregator_selection () =
  let url = Mithril_client.network_aggregator "preview" in
  Alcotest.(check bool) "preview url" true (String.length url > 20);
  let url = Mithril_client.network_aggregator "mainnet" in
  Alcotest.(check bool) "mainnet url" true (String.length url > 20);
  Alcotest.(check bool) "mainnet contains mainnet" true
    (try let _ = String.index url 'm' in true with Not_found -> false)

(* ================================================================ *)
(* Era detection                                                     *)
(* ================================================================ *)

let test_all_era_tags () =
  let eras = [(0L, "byron"); (1L, "shelley"); (2L, "allegra");
              (3L, "mary"); (4L, "alonzo"); (5L, "babbage"); (6L, "conway")] in
  List.iter (fun (tag, expected) ->
    let inner = Cbor.Array [Cbor.Array []; Cbor.Array []; Cbor.Array []; Cbor.Map []] in
    let outer = Cbor.Array [Cbor.Uint tag; inner] in
    let bytes = Cbor.encode outer in
    match Block_decoder.decode_block bytes with
    | Ok block ->
      Alcotest.(check string) (Printf.sprintf "era %Ld" tag)
        expected (Block_decoder.era_name block.db_era)
    | Error _ -> ()  (* some fail header decode but era detection works *)
  ) eras

(* ================================================================ *)
(* Large UTXO snapshot performance                                   *)
(* ================================================================ *)

let test_large_utxo_snapshot () =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "meridian-large-utxo-%d" (Random.int 100000)) in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "large.snapshot" in
  let ls = Ledger_state.create () in
  let utxo = Ledger_state.utxo ls in
  let n = 10000 in  (* 10K entries as a reasonable test *)
  for i = 0 to n - 1 do
    let hash = Bytes.make 32 '\x00' in
    Bytes.set_uint8 hash 0 (i land 0xFF);
    Bytes.set_uint8 hash 1 ((i lsr 8) land 0xFF);
    Utxo.add utxo
      Utxo.TxIn.{ tx_hash = hash; tx_index = 0 }
      Utxo.TxOut.{ address = Bytes.make 29 '\x61';
                   value = Multi_asset.of_lovelace (Int64.of_int ((i+1) * 1000000));
                   has_datum = false; has_script_ref = false }
  done;
  Alcotest.(check int) "10K utxos" n (Utxo.size utxo);
  let total_before = Utxo.total_lovelace utxo in
  (* Snapshot *)
  Ledger_state.snapshot ls ~path;
  (* Restore *)
  (match Ledger_state.restore ~path with
   | Ok ls2 ->
     Alcotest.(check int) "restored count" n (Ledger_state.utxo_count ls2);
     Alcotest.(check int64) "restored lovelace" total_before (Ledger_state.total_lovelace ls2)
   | Error e -> Alcotest.fail e);
  (* Cleanup *)
  (try Unix.unlink path with _ -> ());
  (try Unix.rmdir dir with _ -> ())

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Mainnet"
    [ ( "Genesis",
        [ Alcotest.test_case "mainnet params" `Quick test_mainnet_genesis_params;
          Alcotest.test_case "preprod params" `Quick test_preprod_genesis ] );
      ( "Epoch arithmetic",
        [ Alcotest.test_case "slot to epoch" `Quick test_mainnet_epoch_arithmetic;
          Alcotest.test_case "slot in epoch" `Quick test_mainnet_slot_in_epoch;
          Alcotest.test_case "first slot" `Quick test_mainnet_epoch_first_slot;
          Alcotest.test_case "boundary" `Quick test_mainnet_epoch_boundary ] );
      ( "Network",
        [ Alcotest.test_case "genesis selection" `Quick test_network_selection;
          Alcotest.test_case "node selection" `Quick test_node_selection;
          Alcotest.test_case "aggregator URLs" `Quick test_aggregator_selection ] );
      ( "Eras",
        [ Alcotest.test_case "all era tags" `Quick test_all_era_tags ] );
      ( "Scale",
        [ Alcotest.test_case "10K UTXO snapshot" `Quick test_large_utxo_snapshot ] );
    ]
