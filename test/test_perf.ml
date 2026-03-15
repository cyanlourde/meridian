open Meridian

let () = Crypto.init ()

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF);
  Bytes.set_uint8 b 1 ((n lsr 8) land 0xFF);
  Bytes.set_uint8 b 2 ((n lsr 16) land 0xFF); b

let make_addr () = Bytes.make 29 '\x61'

(* ================================================================ *)
(* Hashtbl UTXO performance                                          *)
(* ================================================================ *)

let test_utxo_100k () =
  let utxo = Utxo.create () in
  let n = 100_000 in
  let start = Unix.gettimeofday () in
  (* Insert 100K entries *)
  for i = 0 to n - 1 do
    Utxo.add utxo
      Utxo.TxIn.{ tx_hash = make_hash i; tx_index = 0 }
      Utxo.TxOut.{ address = make_addr ();
                   value = Multi_asset.of_lovelace (Int64.of_int ((i+1) * 1000));
                   has_datum = false; has_script_ref = false }
  done;
  Alcotest.(check int) "100K inserted" n (Utxo.size utxo);
  (* Lookup all *)
  for i = 0 to n - 1 do
    ignore (Utxo.find utxo Utxo.TxIn.{ tx_hash = make_hash i; tx_index = 0 })
  done;
  (* Delete half *)
  for i = 0 to n / 2 - 1 do
    Utxo.remove utxo Utxo.TxIn.{ tx_hash = make_hash i; tx_index = 0 }
  done;
  Alcotest.(check int) "50K remaining" (n / 2) (Utxo.size utxo);
  let elapsed = Unix.gettimeofday () -. start in
  Alcotest.(check bool) "under 2 seconds" true (elapsed < 2.0)

(* ================================================================ *)
(* Incremental lovelace tracking                                     *)
(* ================================================================ *)

let test_incremental_lovelace () =
  let utxo = Utxo.create () in
  Utxo.add utxo
    Utxo.TxIn.{ tx_hash = make_hash 1; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 5000000L;
                 has_datum = false; has_script_ref = false };
  Alcotest.(check int64) "5M" 5000000L (Utxo.total_lovelace utxo);
  Utxo.add utxo
    Utxo.TxIn.{ tx_hash = make_hash 2; tx_index = 0 }
    Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 3000000L;
                 has_datum = false; has_script_ref = false };
  Alcotest.(check int64) "8M" 8000000L (Utxo.total_lovelace utxo);
  Utxo.remove utxo Utxo.TxIn.{ tx_hash = make_hash 1; tx_index = 0 };
  Alcotest.(check int64) "3M after remove" 3000000L (Utxo.total_lovelace utxo);
  Utxo.remove utxo Utxo.TxIn.{ tx_hash = make_hash 2; tx_index = 0 };
  Alcotest.(check int64) "0 after all removed" 0L (Utxo.total_lovelace utxo)

(* ================================================================ *)
(* UTXO replace updates lovelace correctly                           *)
(* ================================================================ *)

let test_replace_lovelace () =
  let utxo = Utxo.create () in
  let txin = Utxo.TxIn.{ tx_hash = make_hash 1; tx_index = 0 } in
  Utxo.add utxo txin
    Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 5000000L;
                 has_datum = false; has_script_ref = false };
  Alcotest.(check int64) "5M" 5000000L (Utxo.total_lovelace utxo);
  (* Replace with different value *)
  Utxo.add utxo txin
    Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 3000000L;
                 has_datum = false; has_script_ref = false };
  Alcotest.(check int) "still 1" 1 (Utxo.size utxo);
  Alcotest.(check int64) "3M after replace" 3000000L (Utxo.total_lovelace utxo)

(* ================================================================ *)
(* Hash function distribution                                        *)
(* ================================================================ *)

let test_hash_distribution () =
  (* Verify different TxIns produce different hashes (no trivial collisions) *)
  let hashes = Hashtbl.create 1000 in
  let collisions = ref 0 in
  for i = 0 to 999 do
    let h = Utxo.TxIn.hash Utxo.TxIn.{ tx_hash = make_hash i; tx_index = 0 } in
    if Hashtbl.mem hashes h then incr collisions;
    Hashtbl.replace hashes h ()
  done;
  (* Allow up to 5% collision rate *)
  Alcotest.(check bool) "low collisions" true (!collisions < 50)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Performance"
    [ ( "UTXO Hashtbl",
        [ Alcotest.test_case "100K ops" `Quick test_utxo_100k ] );
      ( "Incremental lovelace",
        [ Alcotest.test_case "add/remove tracking" `Quick test_incremental_lovelace;
          Alcotest.test_case "replace tracking" `Quick test_replace_lovelace ] );
      ( "Hash function",
        [ Alcotest.test_case "distribution" `Quick test_hash_distribution ] );
    ]
