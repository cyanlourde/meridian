open Meridian

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF);
  Bytes.set_uint8 b 1 ((n lsr 8) land 0xFF); b

let make_addr () = Bytes.make 29 '\x61'

let make_decoded_tx ?(fee = 200000L) inputs =
  let dt_inputs = List.map (fun (h, i) ->
    Tx_decoder.{ ti_tx_hash = make_hash h; ti_index = Int64.of_int i }
  ) inputs in
  Tx_decoder.{
    dt_inputs;
    dt_outputs = [{ to_address = make_addr ();
                    to_value = Multi_asset.of_lovelace 1000000L;
                    to_has_datum = false; to_has_script_ref = false }];
    dt_fee = fee; dt_ttl = None; dt_validity_start = None;
    dt_certs = []; dt_withdrawal_total = 0L;
    dt_mint = Multi_asset.zero;
    dt_collateral_inputs = []; dt_collateral_return = None;
    dt_total_collateral = None; dt_is_valid = true;
    dt_voting_procedures = 0; dt_proposal_count = 0;
    dt_treasury_donation = 0L;
    dt_era = Block_decoder.Shelley;
  }

(* ================================================================ *)
(* Basic operations                                                  *)
(* ================================================================ *)

let test_empty () =
  let mp = Mempool.create () in
  let (count, sz) = Mempool.size mp in
  Alcotest.(check int) "count" 0 count;
  Alcotest.(check int) "size" 0 sz;
  Alcotest.(check bool) "has_tx" false (Mempool.has_tx mp ~tx_hash:(make_hash 1));
  Alcotest.(check int) "get_all" 0 (List.length (Mempool.get_all mp))

let test_add_single () =
  let mp = Mempool.create () in
  let hash = make_hash 1 in
  let raw = Bytes.make 100 '\xaa' in
  let decoded = make_decoded_tx [(1, 0)] in
  (match Mempool.add_tx mp ~tx_hash:hash ~raw_cbor:raw ~decoded ~fee:200000L with
   | Ok () -> ()
   | Error e -> Alcotest.fail e);
  let (count, sz) = Mempool.size mp in
  Alcotest.(check int) "count" 1 count;
  Alcotest.(check int) "size" 100 sz;
  Alcotest.(check bool) "has_tx" true (Mempool.has_tx mp ~tx_hash:hash);
  (match Mempool.get_tx mp ~tx_hash:hash with
   | Some e -> Alcotest.(check int64) "fee" 200000L e.fee
   | None -> Alcotest.fail "not found")

let test_duplicate () =
  let mp = Mempool.create () in
  let hash = make_hash 1 in
  let raw = Bytes.make 50 '\xaa' in
  let decoded = make_decoded_tx [(1, 0)] in
  ignore (Mempool.add_tx mp ~tx_hash:hash ~raw_cbor:raw ~decoded ~fee:200000L);
  match Mempool.add_tx mp ~tx_hash:hash ~raw_cbor:raw ~decoded ~fee:200000L with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected duplicate error"

let test_full_by_count () =
  let mp = Mempool.create ~max_tx_count:2 () in
  let raw = Bytes.make 10 '\xaa' in
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 1) ~raw_cbor:raw
            ~decoded:(make_decoded_tx [(1,0)]) ~fee:100L);
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 2) ~raw_cbor:raw
            ~decoded:(make_decoded_tx [(2,0)]) ~fee:200L);
  match Mempool.add_tx mp ~tx_hash:(make_hash 3) ~raw_cbor:raw
          ~decoded:(make_decoded_tx [(3,0)]) ~fee:300L with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected full"

let test_full_by_size () =
  let mp = Mempool.create ~max_size_bytes:100 () in
  let raw = Bytes.make 60 '\xaa' in
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 1) ~raw_cbor:raw
            ~decoded:(make_decoded_tx [(1,0)]) ~fee:100L);
  match Mempool.add_tx mp ~tx_hash:(make_hash 2) ~raw_cbor:raw
          ~decoded:(make_decoded_tx [(2,0)]) ~fee:200L with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected full by size"

let test_remove () =
  let mp = Mempool.create () in
  let hash = make_hash 1 in
  ignore (Mempool.add_tx mp ~tx_hash:hash ~raw_cbor:(Bytes.make 50 '\xaa')
            ~decoded:(make_decoded_tx [(1,0)]) ~fee:100L);
  Mempool.remove_tx mp ~tx_hash:hash;
  Alcotest.(check bool) "gone" false (Mempool.has_tx mp ~tx_hash:hash);
  let (count, _) = Mempool.size mp in
  Alcotest.(check int) "count 0" 0 count

let test_remove_confirmed () =
  let mp = Mempool.create () in
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 1) ~raw_cbor:(Bytes.make 10 '\xaa')
            ~decoded:(make_decoded_tx [(1,0)]) ~fee:100L);
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 2) ~raw_cbor:(Bytes.make 10 '\xbb')
            ~decoded:(make_decoded_tx [(2,0)]) ~fee:200L);
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 3) ~raw_cbor:(Bytes.make 10 '\xcc')
            ~decoded:(make_decoded_tx [(3,0)]) ~fee:300L);
  let removed = Mempool.remove_confirmed mp
    ~block_tx_hashes:[make_hash 1; make_hash 3] in
  Alcotest.(check int) "removed 2" 2 removed;
  let (count, _) = Mempool.size mp in
  Alcotest.(check int) "1 remaining" 1 count;
  Alcotest.(check bool) "tx2 still there" true (Mempool.has_tx mp ~tx_hash:(make_hash 2))

let test_revalidate () =
  let mp = Mempool.create () in
  let utxo = Utxo.create () in
  (* Add a UTXO that the mempool tx spends *)
  Utxo.add utxo Utxo.TxIn.{ tx_hash = make_hash 10; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 5000000L;
                 has_datum = false; has_script_ref = false };
  let decoded = make_decoded_tx [(10, 0)] in
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 1) ~raw_cbor:(Bytes.make 10 '\xaa')
            ~decoded ~fee:200000L);
  (* Remove the UTXO (simulating it being consumed by a block) *)
  Utxo.remove utxo Utxo.TxIn.{ tx_hash = make_hash 10; tx_index = 0 };
  (* Revalidate — tx should be evicted *)
  let evicted = Mempool.revalidate_all mp ~utxo ~current_slot:100L in
  Alcotest.(check int) "1 evicted" 1 (List.length evicted);
  let (count, _) = Mempool.size mp in
  Alcotest.(check int) "empty" 0 count

let test_expire () =
  let mp = Mempool.create () in
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 1) ~raw_cbor:(Bytes.make 10 '\xaa')
            ~decoded:(make_decoded_tx [(1,0)]) ~fee:100L);
  (* Expire with a future cutoff — should expire the tx *)
  let expired = Mempool.expire mp ~current_time:(Unix.gettimeofday () +. 7200.0)
    ~max_age_seconds:3600.0 in
  Alcotest.(check int) "1 expired" 1 expired;
  let (count, _) = Mempool.size mp in
  Alcotest.(check int) "empty" 0 count

let test_get_all_ordered () =
  let mp = Mempool.create () in
  (* 3 txs: fee 100/50bytes=2.0, fee 300/200bytes=1.5, fee 500/100bytes=5.0 *)
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 1) ~raw_cbor:(Bytes.make 50 '\xaa')
            ~decoded:(make_decoded_tx [(1,0)]) ~fee:100L);
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 2) ~raw_cbor:(Bytes.make 200 '\xbb')
            ~decoded:(make_decoded_tx [(2,0)]) ~fee:300L);
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 3) ~raw_cbor:(Bytes.make 100 '\xcc')
            ~decoded:(make_decoded_tx [(3,0)]) ~fee:500L);
  let all = Mempool.get_all mp in
  Alcotest.(check int) "3 txs" 3 (List.length all);
  (* Highest density first: 500/100=5.0, then 100/50=2.0, then 300/200=1.5 *)
  let fees = List.map (fun e -> e.Mempool.fee) all in
  Alcotest.(check int64) "first" 500L (List.hd fees);
  Alcotest.(check int64) "last" 300L (List.nth fees 2)

let test_snapshot () =
  let mp = Mempool.create () in
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 1) ~raw_cbor:(Bytes.make 10 '\xaa')
            ~decoded:(make_decoded_tx [(1,0)]) ~fee:100L);
  ignore (Mempool.add_tx mp ~tx_hash:(make_hash 2) ~raw_cbor:(Bytes.make 20 '\xbb')
            ~decoded:(make_decoded_tx [(2,0)]) ~fee:200L);
  let snap = Mempool.get_snapshot mp in
  Alcotest.(check int) "snap count" 2 snap.snap_tx_count;
  Alcotest.(check int) "snap size" 30 snap.snap_total_size;
  Alcotest.(check int) "snap hashes" 2 (List.length snap.snap_tx_hashes)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Mempool"
    [ ( "Basic",
        [ Alcotest.test_case "empty" `Quick test_empty;
          Alcotest.test_case "add single" `Quick test_add_single;
          Alcotest.test_case "duplicate" `Quick test_duplicate ] );
      ( "Limits",
        [ Alcotest.test_case "full by count" `Quick test_full_by_count;
          Alcotest.test_case "full by size" `Quick test_full_by_size ] );
      ( "Remove",
        [ Alcotest.test_case "remove" `Quick test_remove;
          Alcotest.test_case "remove confirmed" `Quick test_remove_confirmed ] );
      ( "Maintenance",
        [ Alcotest.test_case "revalidate" `Quick test_revalidate;
          Alcotest.test_case "expire" `Quick test_expire ] );
      ( "Ordering",
        [ Alcotest.test_case "fee density" `Quick test_get_all_ordered ] );
      ( "Snapshot",
        [ Alcotest.test_case "point in time" `Quick test_snapshot ] );
    ]
