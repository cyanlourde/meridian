open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let make_hash n =
  let b = Bytes.make 32 '\x00' in
  Bytes.set_uint8 b 0 (n land 0xFF); b

let make_addr () = Bytes.make 29 '\x61'  (* enterprise address *)

(* ================================================================ *)
(* Shelley tx decode                                                 *)
(* ================================================================ *)

let test_shelley_tx () =
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [
      Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L];
      Cbor.Array [Cbor.Bytes (make_hash 2); Cbor.Uint 1L];
    ]);
    (Cbor.Uint 1L, Cbor.Array [
      Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 2000000L];
      Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 1500000L];
    ]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
    (Cbor.Uint 3L, Cbor.Uint 50000000L);
  ] in
  match Tx_decoder.decode_transaction ~era:Shelley tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    Alcotest.(check int) "2 inputs" 2 (List.length tx.dt_inputs);
    Alcotest.(check int) "2 outputs" 2 (List.length tx.dt_outputs);
    Alcotest.(check int64) "fee" 200000L tx.dt_fee;
    Alcotest.(check bool) "has ttl" true (tx.dt_ttl <> None);
    Alcotest.(check int64) "ttl value" 50000000L (Option.get tx.dt_ttl);
    let out0 = List.hd tx.dt_outputs in
    Alcotest.(check int64) "output 0 lovelace" 2000000L out0.to_lovelace;
    Alcotest.(check int) "output 0 addr len" 29 (Bytes.length out0.to_address)

(* ================================================================ *)
(* Tx fee is positive                                                *)
(* ================================================================ *)

let test_fee_positive () =
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 1000000L]]);
    (Cbor.Uint 2L, Cbor.Uint 175000L);
  ] in
  match Tx_decoder.decode_transaction ~era:Shelley tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    Alcotest.(check bool) "fee > 0" true (tx.dt_fee > 0L)

(* ================================================================ *)
(* Output address non-empty                                          *)
(* ================================================================ *)

let test_output_address () =
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 5000000L]]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
  ] in
  match Tx_decoder.decode_transaction ~era:Shelley tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    let out = List.hd tx.dt_outputs in
    Alcotest.(check bool) "addr non-empty" true (Bytes.length out.to_address > 0);
    Alcotest.(check bool) "lovelace > 0" true (out.to_lovelace > 0L)

(* ================================================================ *)
(* Mary multi-asset output                                           *)
(* ================================================================ *)

let test_mary_multi_asset () =
  let multi_asset = Cbor.Map [
    (Cbor.Bytes (Bytes.make 28 '\xaa'), Cbor.Map [
      (Cbor.Bytes (Bytes.of_string "Token"), Cbor.Uint 100L)
    ])
  ] in
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [
      Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Array [Cbor.Uint 2000000L; multi_asset]];
    ]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
  ] in
  match Tx_decoder.decode_transaction ~era:Mary tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    let out = List.hd tx.dt_outputs in
    Alcotest.(check bool) "has multi-asset" true out.to_has_multi_asset;
    Alcotest.(check int64) "lovelace" 2000000L out.to_lovelace

(* ================================================================ *)
(* Alonzo with datum hash                                            *)
(* ================================================================ *)

let test_alonzo_datum () =
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [
      Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 2000000L; Cbor.Bytes (make_hash 99)];
    ]);
    (Cbor.Uint 2L, Cbor.Uint 300000L);
    (Cbor.Uint 11L, Cbor.Bytes (make_hash 50));  (* script data hash *)
    (Cbor.Uint 13L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 5); Cbor.Uint 0L]]);
  ] in
  match Tx_decoder.decode_transaction ~era:Alonzo tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    let out = List.hd tx.dt_outputs in
    Alcotest.(check bool) "has datum" true out.to_has_datum;
    Alcotest.(check int) "1 collateral" 1 (List.length tx.dt_collateral_inputs)

(* ================================================================ *)
(* Babbage map-based output                                          *)
(* ================================================================ *)

let test_babbage_output () =
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [
      Cbor.Map [
        (Cbor.Uint 0L, Cbor.Bytes (make_addr ()));
        (Cbor.Uint 1L, Cbor.Uint 5000000L);
        (Cbor.Uint 2L, Cbor.Array [Cbor.Uint 1L; Cbor.Uint 42L]);  (* inline datum *)
        (Cbor.Uint 3L, Cbor.Tag (24L, Cbor.Bytes (Bytes.make 10 '\xcc')));  (* script ref *)
      ];
    ]);
    (Cbor.Uint 2L, Cbor.Uint 250000L);
  ] in
  match Tx_decoder.decode_transaction ~era:Babbage tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    let out = List.hd tx.dt_outputs in
    Alcotest.(check int64) "lovelace" 5000000L out.to_lovelace;
    Alcotest.(check bool) "has datum" true out.to_has_datum;
    Alcotest.(check bool) "has script ref" true out.to_has_script_ref

(* ================================================================ *)
(* Certificates and withdrawals                                      *)
(* ================================================================ *)

let test_certs_withdrawals () =
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 1000000L]]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
    (Cbor.Uint 4L, Cbor.Array [Cbor.Array [Cbor.Uint 0L; Cbor.Array [Cbor.Uint 0L; Cbor.Bytes (Bytes.make 28 '\x00')]]]); (* 1 cert *)
    (Cbor.Uint 5L, Cbor.Map [
      (Cbor.Bytes (Bytes.make 29 '\xe1'), Cbor.Uint 5000000L);
      (Cbor.Bytes (Bytes.make 29 '\xe2'), Cbor.Uint 3000000L);
    ]); (* 2 withdrawals *)
  ] in
  match Tx_decoder.decode_transaction ~era:Shelley tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    Alcotest.(check int) "1 cert" 1 (List.length tx.dt_certs);
    Alcotest.(check bool) "has withdrawal" true (tx.dt_withdrawal_total > 0L)

(* ================================================================ *)
(* Mint field (Mary+)                                                *)
(* ================================================================ *)

let test_mint () =
  let tx_body = Cbor.Map [
    (Cbor.Uint 0L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_hash 1); Cbor.Uint 0L]]);
    (Cbor.Uint 1L, Cbor.Array [Cbor.Array [Cbor.Bytes (make_addr ()); Cbor.Uint 1000000L]]);
    (Cbor.Uint 2L, Cbor.Uint 200000L);
    (Cbor.Uint 9L, Cbor.Map [(Cbor.Bytes (Bytes.make 28 '\xaa'),
                               Cbor.Map [(Cbor.Bytes (Bytes.of_string "T"), Cbor.Uint 1L)])]);
  ] in
  match Tx_decoder.decode_transaction ~era:Mary tx_body with
  | Error e -> Alcotest.fail e
  | Ok tx ->
    Alcotest.(check bool) "has mint" true tx.dt_mint

(* ================================================================ *)
(* Address decode integration                                        *)
(* ================================================================ *)

let test_address_decode () =
  (* Enterprise address: type 6, network 1 *)
  let addr = Bytes.make 29 '\x00' in
  Bytes.set_uint8 addr 0 0x61;  (* type=6, network=1 *)
  match Address.decode_address addr with
  | Error e -> Alcotest.fail e
  | Ok a ->
    Alcotest.(check string) "type" "enterprise" (Address.addr_type_name a.addr_type);
    Alcotest.(check int) "network" 1 a.network_id;
    Alcotest.(check bool) "has payment" true (a.payment_credential <> None)

let test_address_types () =
  (* Base address: type 0 *)
  let addr = Bytes.make 57 '\x00' in
  Bytes.set_uint8 addr 0 0x01;  (* type=0, network=1 *)
  (match Address.decode_address addr with
   | Ok a -> Alcotest.(check string) "base" "base" (Address.addr_type_name a.addr_type)
   | Error e -> Alcotest.fail e);
  (* Byron address *)
  let addr = Bytes.make 10 '\x00' in
  Bytes.set_uint8 addr 0 0x82;  (* type=8, network=2 *)
  (match Address.decode_address addr with
   | Ok a -> Alcotest.(check string) "byron" "byron" (Address.addr_type_name a.addr_type)
   | Error e -> Alcotest.fail e);
  (* Reward address *)
  let addr = Bytes.make 29 '\x00' in
  Bytes.set_uint8 addr 0 0xe1;  (* type=14, network=1 *)
  (match Address.decode_address addr with
   | Ok a -> Alcotest.(check string) "reward" "reward" (Address.addr_type_name a.addr_type)
   | Error e -> Alcotest.fail e)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Tx-Decoder"
    [ ( "Shelley tx",
        [ Alcotest.test_case "full decode" `Quick test_shelley_tx;
          Alcotest.test_case "fee positive" `Quick test_fee_positive;
          Alcotest.test_case "output address" `Quick test_output_address ] );
      ( "Multi-era outputs",
        [ Alcotest.test_case "mary multi-asset" `Quick test_mary_multi_asset;
          Alcotest.test_case "alonzo datum" `Quick test_alonzo_datum;
          Alcotest.test_case "babbage map output" `Quick test_babbage_output ] );
      ( "Tx fields",
        [ Alcotest.test_case "certs and withdrawals" `Quick test_certs_withdrawals;
          Alcotest.test_case "mint" `Quick test_mint ] );
      ( "Address decode",
        [ Alcotest.test_case "enterprise" `Quick test_address_decode;
          Alcotest.test_case "all types" `Quick test_address_types ] );
    ]
