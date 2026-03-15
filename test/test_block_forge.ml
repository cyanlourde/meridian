open Meridian

let () = Crypto.init ()

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF); b

let make_addr () = Bytes.make 29 '\x61'

let make_decoded_tx ?(fee = 200000L) inputs =
  Tx_decoder.{
    dt_inputs = List.map (fun (h, i) ->
      { ti_tx_hash = make_hash h; ti_index = Int64.of_int i }) inputs;
    dt_outputs = [{ to_address = make_addr ();
                    to_value = Multi_asset.of_lovelace 4800000L;
                    to_has_datum = false; to_has_script_ref = false }];
    dt_fee = fee; dt_ttl = None; dt_validity_start = None;
    dt_certs = []; dt_withdrawal_total = 0L;
    dt_mint = Multi_asset.zero;
    dt_collateral_inputs = []; dt_collateral_return = None;
    dt_total_collateral = None; dt_is_valid = true;
    dt_voting_procedures = 0; dt_proposal_count = 0;
    dt_treasury_donation = 0L;
    dt_era = Shelley }

let make_test_opcert () =
  match Crypto.ed25519_keypair () with
  | Error e -> failwith e
  | Ok (cold_pk, cold_sk) ->
    let hot_vkey = Bytes.make 32 '\xbb' in
    match Kes.opcert_sign ~cold_skey:cold_sk ~hot_vkey
            ~sequence_number:0L ~kes_period:0L with
    | Error e -> failwith e
    | Ok opcert -> (cold_pk, opcert)

(* ================================================================ *)
(* Forge empty block                                                 *)
(* ================================================================ *)

let test_forge_empty () =
  let mempool = Mempool.create () in
  let utxo = Utxo.create () in
  let (cold_pk, opcert) = make_test_opcert () in
  let block = Block_forge.forge_block
    ~slot:100L ~block_number:1L ~prev_hash:(make_hash 0)
    ~issuer_vkey:cold_pk ~vrf_vkey:(Bytes.make 32 '\xcc')
    ~vrf_output:(Bytes.make 32 '\xdd') ~vrf_proof:(Bytes.make 80 '\xee')
    ~opcert ~kes_signature:(Bytes.make 448 '\xff')
    ~protocol_version:(7L, 0L)
    ~mempool ~utxo ~max_block_body_size:65536 in
  Alcotest.(check int) "0 txs" 0 block.fb_tx_count;
  Alcotest.(check int64) "slot" 100L block.fb_slot;
  Alcotest.(check int64) "block_no" 1L block.fb_block_number;
  (* Verify it round-trips through decoder *)
  match Block_decoder.decode_block block.fb_encoded with
  | Ok decoded ->
    Alcotest.(check string) "era" "babbage" (Block_decoder.era_name decoded.db_era);
    Alcotest.(check int) "decoded 0 txs" 0 decoded.db_tx_count
  | Error e -> Alcotest.fail e

(* ================================================================ *)
(* Forge with transactions                                           *)
(* ================================================================ *)

let test_forge_with_txs () =
  let mempool = Mempool.create () in
  let utxo = Utxo.create () in
  (* Seed UTXO *)
  for i = 1 to 3 do
    Utxo.add utxo Utxo.TxIn.{ tx_hash = make_hash (i * 10); tx_index = 0 }
      Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 5000000L;
                   has_datum = false; has_script_ref = false }
  done;
  (* Add 3 txs to mempool *)
  for i = 1 to 3 do
    let raw = Cbor.encode (Cbor.Map [
      (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash (i*10)); Cbor.Uint 0L]]);
      (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 4800000L]]);
      (Cbor.Uint 2L, Cbor.Uint 200000L);
    ]) in
    ignore (Mempool.add_tx mempool ~tx_hash:(make_hash i)
              ~raw_cbor:raw ~decoded:(make_decoded_tx [(i*10, 0)]) ~fee:200000L)
  done;
  let (cold_pk, opcert) = make_test_opcert () in
  let block = Block_forge.forge_block
    ~slot:200L ~block_number:2L ~prev_hash:(make_hash 99)
    ~issuer_vkey:cold_pk ~vrf_vkey:(Bytes.make 32 '\xcc')
    ~vrf_output:(Bytes.make 32 '\xdd') ~vrf_proof:(Bytes.make 80 '\xee')
    ~opcert ~kes_signature:(Bytes.make 448 '\xff')
    ~protocol_version:(7L, 0L)
    ~mempool ~utxo ~max_block_body_size:65536 in
  Alcotest.(check int) "3 txs" 3 block.fb_tx_count;
  Alcotest.(check int64) "total fees" 600000L block.fb_total_fees;
  Alcotest.(check int) "body hash 32 bytes" 32 (Bytes.length block.fb_body_hash)

(* ================================================================ *)
(* Block size limit                                                  *)
(* ================================================================ *)

let test_size_limit () =
  let mempool = Mempool.create () in
  let utxo = Utxo.create () in
  for i = 1 to 5 do
    Utxo.add utxo Utxo.TxIn.{ tx_hash = make_hash (i*10); tx_index = 0 }
      Utxo.TxOut.{ address = make_addr (); value = Multi_asset.of_lovelace 5000000L;
                   has_datum = false; has_script_ref = false };
    let raw = Bytes.make 500 '\xaa' in  (* 500 bytes each *)
    ignore (Mempool.add_tx mempool ~tx_hash:(make_hash i)
              ~raw_cbor:raw ~decoded:(make_decoded_tx [(i*10, 0)]) ~fee:200000L)
  done;
  let (cold_pk, opcert) = make_test_opcert () in
  let block = Block_forge.forge_block
    ~slot:300L ~block_number:3L ~prev_hash:(make_hash 99)
    ~issuer_vkey:cold_pk ~vrf_vkey:(Bytes.make 32 '\xcc')
    ~vrf_output:(Bytes.make 32 '\xdd') ~vrf_proof:(Bytes.make 80 '\xee')
    ~opcert ~kes_signature:(Bytes.make 448 '\xff')
    ~protocol_version:(7L, 0L)
    ~mempool ~utxo ~max_block_body_size:1200 in
  (* 1200 bytes fits ~2 txs of 500 bytes each *)
  Alcotest.(check bool) "limited" true (block.fb_tx_count < 5)

(* ================================================================ *)
(* Opcert creation and verification                                  *)
(* ================================================================ *)

let test_opcert () =
  match Crypto.ed25519_keypair () with
  | Error e -> Alcotest.fail e
  | Ok (cold_pk, cold_sk) ->
    let hot_vkey = Bytes.make 32 '\xaa' in
    match Kes.opcert_sign ~cold_skey:cold_sk ~hot_vkey
            ~sequence_number:5L ~kes_period:100L with
    | Error e -> Alcotest.fail e
    | Ok opcert ->
      Alcotest.(check int) "hot_vkey 32" 32 (Bytes.length opcert.oc_hot_vkey);
      Alcotest.(check int64) "seq 5" 5L opcert.oc_sequence_number;
      Alcotest.(check int64) "period 100" 100L opcert.oc_kes_period;
      Alcotest.(check int) "sig 64" 64 (Bytes.length opcert.oc_cold_signature);
      (* Verify with block_validator *)
      match Block_validator.verify_opcert_signature ~issuer_vkey:cold_pk opcert with
      | Ok () -> ()
      | Error e -> Alcotest.fail (Block_validator.error_to_string e)

let test_opcert_cbor () =
  let (_, opcert) = make_test_opcert () in
  let cbor_list = Kes.opcert_encode opcert in
  Alcotest.(check int) "4 elements" 4 (List.length cbor_list)

(* ================================================================ *)
(* Full block round-trip                                             *)
(* ================================================================ *)

let test_full_roundtrip () =
  let mempool = Mempool.create () in
  let utxo = Utxo.create () in
  let (cold_pk, opcert) = make_test_opcert () in
  let block = Block_forge.forge_block
    ~slot:500L ~block_number:10L ~prev_hash:(make_hash 42)
    ~issuer_vkey:cold_pk ~vrf_vkey:(Bytes.make 32 '\xcc')
    ~vrf_output:(Bytes.make 32 '\xdd') ~vrf_proof:(Bytes.make 80 '\xee')
    ~opcert ~kes_signature:(Bytes.make 448 '\xff')
    ~protocol_version:(7L, 0L)
    ~mempool ~utxo ~max_block_body_size:65536 in
  match Block_decoder.decode_block block.fb_encoded with
  | Error e -> Alcotest.fail e
  | Ok decoded ->
    Alcotest.(check int64) "slot" 500L decoded.db_header.bh_slot;
    Alcotest.(check int64) "block_no" 10L decoded.db_header.bh_block_number;
    Alcotest.(check int) "issuer 32" 32 (Bytes.length decoded.db_header.bh_issuer_vkey)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Block-Forge"
    [ ( "Forge",
        [ Alcotest.test_case "empty block" `Quick test_forge_empty;
          Alcotest.test_case "with transactions" `Quick test_forge_with_txs;
          Alcotest.test_case "size limit" `Quick test_size_limit ] );
      ( "Opcert",
        [ Alcotest.test_case "create and verify" `Quick test_opcert;
          Alcotest.test_case "CBOR encode" `Quick test_opcert_cbor ] );
      ( "Round-trip",
        [ Alcotest.test_case "full block" `Quick test_full_roundtrip ] );
    ]
