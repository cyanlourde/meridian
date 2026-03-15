open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF);
  Bytes.set_uint8 b 1 ((n lsr 8) land 0xFF); b

let make_addr () = Bytes.make 29 '\x61'

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

(** Build a minimal Shelley-era decoded_block with given transactions. *)
let make_block ?(slot = 100L) ?(block_no = 1L) tx_bodies =
  let header = Block_decoder.{
    bh_slot = slot; bh_block_number = block_no;
    bh_prev_hash = None; bh_issuer_vkey = Bytes.empty;
    bh_body_hash = Bytes.empty;
    bh_protocol_version = (2L, 0L); bh_era = Shelley;
    bh_vrf_vkey = Bytes.empty; bh_block_signature = Bytes.empty;
    bh_opcert = None; bh_header_body_cbor = Bytes.empty;
  } in
  Block_decoder.{
    db_era = Shelley; db_header = header;
    db_tx_count = List.length tx_bodies;
    db_tx_raw = tx_bodies;
    db_raw_cbor = Cbor.Null;
  }

(** Build a CBOR tx body map. *)
let make_tx_cbor ~inputs ~outputs ~fee =
  let inp_cbor = List.map (fun (hash_n, idx) ->
    Cbor.Array [Cbor.Bytes (make_hash hash_n); Cbor.Uint (Int64.of_int idx)]
  ) inputs in
  let out_cbor = List.map (fun lovelace ->
    Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint lovelace]
  ) outputs in
  Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array inp_cbor);
    (Cbor.Uint 1L, Cbor.Array out_cbor);
    (Cbor.Uint 2L, Cbor.Uint fee);
  ]

(* ================================================================ *)
(* Empty ledger                                                      *)
(* ================================================================ *)

let test_empty () =
  let ls = Ledger_state.create () in
  Alcotest.(check int) "utxo 0" 0 (Ledger_state.utxo_count ls);
  Alcotest.(check int64) "lovelace 0" 0L (Ledger_state.total_lovelace ls);
  let (slot, blocks) = Ledger_state.tip ls in
  Alcotest.(check int64) "slot 0" 0L slot;
  Alcotest.(check int) "blocks 0" 0 blocks

(* ================================================================ *)
(* Apply single block                                                *)
(* ================================================================ *)

let test_apply_single_block () =
  let ls = Ledger_state.create () in
  (* Seed the UTXO with a genesis output *)
  let utxo = Ledger_state.utxo ls in
  Utxo.add utxo
    Utxo.TxIn.{ tx_hash = make_hash 0; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); lovelace = 10000000L;
                 has_multi_asset = false; has_datum = false;
                 has_script_ref = false };
  Alcotest.(check int) "seeded utxo" 1 (Utxo.size utxo);
  (* Block with 1 tx: spend genesis, produce 2 outputs *)
  let tx = make_tx_cbor ~inputs:[(0, 0)] ~outputs:[6000000L; 3800000L] ~fee:200000L in
  let block = make_block ~slot:100L ~block_no:1L [tx] in
  let errors = Ledger_state.apply_block ls block in
  (* Check errors *)
  Alcotest.(check int) "no errors" 0 (List.length errors);
  (* Check state *)
  let (slot, blocks) = Ledger_state.tip ls in
  Alcotest.(check int64) "slot 100" 100L slot;
  Alcotest.(check int) "blocks 1" 1 blocks;
  (* UTXO: genesis consumed, 2 new outputs *)
  Alcotest.(check int) "utxo size" 2 (Ledger_state.utxo_count ls);
  Alcotest.(check int64) "total" 9800000L (Ledger_state.total_lovelace ls)

(* ================================================================ *)
(* Apply chain of 3 blocks                                           *)
(* ================================================================ *)

let test_apply_chain () =
  let ls = Ledger_state.create () in
  let utxo = Ledger_state.utxo ls in
  (* Genesis: 10 ADA *)
  Utxo.add utxo
    Utxo.TxIn.{ tx_hash = make_hash 0; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); lovelace = 10000000L;
                 has_multi_asset = false; has_datum = false;
                 has_script_ref = false };
  (* Block 1: split genesis into 2 outputs *)
  let tx1 = make_tx_cbor ~inputs:[(0, 0)] ~outputs:[6000000L; 3800000L] ~fee:200000L in
  let _ = Ledger_state.apply_block ls (make_block ~slot:100L ~block_no:1L [tx1]) in
  Alcotest.(check int) "after block 1" 2 (Ledger_state.utxo_count ls);
  (* Block 2: spend first output *)
  (* We need the tx_hash of tx1 to reference its outputs *)
  let tx1_hash = Crypto.blake2b_256 (Cbor.encode tx1) in
  let tx2 = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes tx1_hash; Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 5800000L]]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
  ] in
  let _ = Ledger_state.apply_block ls (make_block ~slot:200L ~block_no:2L [tx2]) in
  Alcotest.(check int) "after block 2" 2 (Ledger_state.utxo_count ls);
  (* Block 3: empty block *)
  let _ = Ledger_state.apply_block ls (make_block ~slot:300L ~block_no:3L []) in
  let (slot, blocks) = Ledger_state.tip ls in
  Alcotest.(check int64) "slot 300" 300L slot;
  Alcotest.(check int) "blocks 3" 3 blocks;
  Alcotest.(check int) "utxo still 2" 2 (Ledger_state.utxo_count ls)

(* ================================================================ *)
(* Invalid tx: errors recorded, other txs still applied              *)
(* ================================================================ *)

let test_invalid_tx () =
  let ls = Ledger_state.create () in
  let utxo = Ledger_state.utxo ls in
  Utxo.add utxo
    Utxo.TxIn.{ tx_hash = make_hash 0; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); lovelace = 5000000L;
                 has_multi_asset = false; has_datum = false;
                 has_script_ref = false };
  (* Tx that references a non-existent input *)
  let bad_tx = make_tx_cbor ~inputs:[(99, 0)] ~outputs:[4800000L] ~fee:200000L in
  (* Valid tx that spends genesis *)
  let good_tx = make_tx_cbor ~inputs:[(0, 0)] ~outputs:[4800000L] ~fee:200000L in
  let block = make_block ~slot:100L ~block_no:1L [bad_tx; good_tx] in
  let errors = Ledger_state.apply_block ls block in
  (* Bad tx should have errors *)
  Alcotest.(check bool) "has errors" true (List.length errors > 0);
  (* Both txs still applied (bad tx applied with errors recorded) *)
  let (_, blocks) = Ledger_state.tip ls in
  Alcotest.(check int) "block applied" 1 blocks

(* ================================================================ *)
(* Empty block: no txs                                               *)
(* ================================================================ *)

let test_empty_block () =
  let ls = Ledger_state.create () in
  let block = make_block ~slot:42L ~block_no:1L [] in
  let errors = Ledger_state.apply_block ls block in
  Alcotest.(check int) "no errors" 0 (List.length errors);
  let (slot, blocks) = Ledger_state.tip ls in
  Alcotest.(check int64) "slot 42" 42L slot;
  Alcotest.(check int) "blocks 1" 1 blocks;
  Alcotest.(check int) "utxo unchanged" 0 (Ledger_state.utxo_count ls)

(* ================================================================ *)
(* Snapshot and restore                                              *)
(* ================================================================ *)

let test_snapshot_restore () =
  let dir = temp_dir () in
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "ledger.snapshot" in
  let ls = Ledger_state.create () in
  let utxo = Ledger_state.utxo ls in
  (* Add some UTXOs directly *)
  for i = 0 to 4 do
    Utxo.add utxo
      Utxo.TxIn.{ tx_hash = make_hash i; tx_index = 0 }
      Utxo.TxOut.{ address = make_addr (); lovelace = Int64.of_int ((i + 1) * 1000000);
                   has_multi_asset = (i = 2); has_datum = (i = 3);
                   has_script_ref = (i = 4) }
  done;
  (* Apply a block to advance slot *)
  let _ = Ledger_state.apply_block ls (make_block ~slot:500L ~block_no:5L []) in
  let count_before = Ledger_state.utxo_count ls in
  let total_before = Ledger_state.total_lovelace ls in
  let (slot_before, blocks_before) = Ledger_state.tip ls in
  (* Snapshot *)
  Ledger_state.snapshot ls ~path;
  (* Verify no .tmp file *)
  Alcotest.(check bool) "no tmp" false (Sys.file_exists (path ^ ".tmp"));
  Alcotest.(check bool) "snapshot exists" true (Sys.file_exists path);
  (* Restore *)
  match Ledger_state.restore ~path with
  | Error e -> Alcotest.fail e
  | Ok ls2 ->
    Alcotest.(check int) "same count" count_before (Ledger_state.utxo_count ls2);
    Alcotest.(check int64) "same lovelace" total_before (Ledger_state.total_lovelace ls2);
    let (slot2, blocks2) = Ledger_state.tip ls2 in
    Alcotest.(check int64) "same slot" slot_before slot2;
    Alcotest.(check int) "same blocks" blocks_before blocks2;
    rm_rf dir

let test_restore_missing () =
  match Ledger_state.restore ~path:"/nonexistent/file" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error"

(* ================================================================ *)
(* Protocol params                                                   *)
(* ================================================================ *)

let test_shelley_fee () =
  let p = Ledger_state.shelley_params in
  (* fee = 44 * 300 + 155381 = 13200 + 155381 = 168581 *)
  let fee = Int64.add (Int64.mul p.min_fee_a 300L) p.min_fee_b in
  Alcotest.(check int64) "fee calc" 168581L fee

(* ================================================================ *)
(* UTxO stats                                                        *)
(* ================================================================ *)

let test_utxo_stats () =
  let ls = Ledger_state.create () in
  let utxo = Ledger_state.utxo ls in
  Utxo.add utxo
    Utxo.TxIn.{ tx_hash = make_hash 0; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); lovelace = 5000000L;
                 has_multi_asset = false; has_datum = false;
                 has_script_ref = false };
  let stats = Ledger_state.utxo_stats ls in
  Alcotest.(check int) "count" 1 stats.us_count;
  Alcotest.(check int64) "total" 5000000L stats.us_total_lovelace

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Ledger-State"
    [ ( "Empty",
        [ Alcotest.test_case "empty ledger" `Quick test_empty ] );
      ( "Apply blocks",
        [ Alcotest.test_case "single block" `Quick test_apply_single_block;
          Alcotest.test_case "chain of 3" `Quick test_apply_chain;
          Alcotest.test_case "invalid tx" `Quick test_invalid_tx;
          Alcotest.test_case "empty block" `Quick test_empty_block ] );
      ( "Snapshot",
        [ Alcotest.test_case "snapshot/restore" `Quick test_snapshot_restore;
          Alcotest.test_case "restore missing" `Quick test_restore_missing ] );
      ( "Protocol params",
        [ Alcotest.test_case "shelley fee" `Quick test_shelley_fee ] );
      ( "Stats",
        [ Alcotest.test_case "utxo stats" `Quick test_utxo_stats ] );
    ]
