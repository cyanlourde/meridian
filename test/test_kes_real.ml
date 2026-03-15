open Meridian

let () = Crypto.init ()

(* ================================================================ *)
(* KES depth 0 (just Ed25519)                                        *)
(* ================================================================ *)

let test_depth0_sign_verify () =
  match Kes.generate ~depth:0 with
  | Error e -> Alcotest.fail e
  | Ok key ->
    let vkey = Kes.vkey_of_key key in
    let msg = Bytes.of_string "test message" in
    match Kes.sign ~kes_key:key ~message:msg ~period:0 with
    | Error e -> Alcotest.fail e
    | Ok sig_ ->
      Alcotest.(check int) "sig 64 bytes" 64 (Bytes.length sig_);
      Alcotest.(check bool) "verify OK" true
        (Kes.verify ~vkey ~signature:sig_ ~message:msg ~period:0 ~depth:0)

let test_depth0_wrong_message () =
  match Kes.generate ~depth:0 with
  | Error e -> Alcotest.fail e
  | Ok key ->
    let vkey = Kes.vkey_of_key key in
    let msg = Bytes.of_string "original" in
    (match Kes.sign ~kes_key:key ~message:msg ~period:0 with
     | Error e -> Alcotest.fail e
     | Ok sig_ ->
       let wrong = Bytes.of_string "modified" in
       Alcotest.(check bool) "wrong msg fails" false
         (Kes.verify ~vkey ~signature:sig_ ~message:wrong ~period:0 ~depth:0))

(* ================================================================ *)
(* KES depth 1 (2 periods)                                          *)
(* ================================================================ *)

let test_depth1_both_periods () =
  match Kes.generate ~depth:1 with
  | Error e -> Alcotest.fail e
  | Ok key ->
    let vkey = Kes.vkey_of_key key in
    let msg = Bytes.of_string "test" in
    (* Sign at period 0 *)
    (match Kes.sign ~kes_key:key ~message:msg ~period:0 with
     | Error e -> Alcotest.fail e
     | Ok sig0 ->
       Alcotest.(check int) "sig0 size" 96 (Bytes.length sig0);
       Alcotest.(check bool) "period 0 OK" true
         (Kes.verify ~vkey ~signature:sig0 ~message:msg ~period:0 ~depth:1));
    (* Sign at period 1 *)
    (match Kes.sign ~kes_key:key ~message:msg ~period:1 with
     | Error e -> Alcotest.fail e
     | Ok sig1 ->
       Alcotest.(check int) "sig1 size" 96 (Bytes.length sig1))

(* ================================================================ *)
(* KES depth 6 (64 periods, Cardano standard)                       *)
(* ================================================================ *)

let test_depth6_sign_verify () =
  match Kes.generate ~depth:6 with
  | Error e -> Alcotest.fail e
  | Ok key ->
    let vkey = Kes.vkey_of_key key in
    Alcotest.(check int) "vkey 32 bytes" 32 (Bytes.length vkey);
    let msg = Bytes.of_string "block header hash" in
    match Kes.sign ~kes_key:key ~message:msg ~period:0 with
    | Error e -> Alcotest.fail e
    | Ok sig_ ->
      (* Depth 6: sig = 64 + 6*32 = 256 bytes *)
      Alcotest.(check bool) "sig > 64 bytes" true (Bytes.length sig_ > 64);
      Alcotest.(check bool) "verify OK" true
        (Kes.verify ~vkey ~signature:sig_ ~message:msg ~period:0 ~depth:6)

let test_depth6_later_period () =
  match Kes.generate ~depth:6 with
  | Error e -> Alcotest.fail e
  | Ok key ->
    let msg = Bytes.of_string "test" in
    match Kes.sign ~kes_key:key ~message:msg ~period:5 with
    | Error e -> Alcotest.fail e
    | Ok _sig -> ()  (* Just verify it doesn't crash *)

let test_depth6_out_of_range () =
  match Kes.generate ~depth:6 with
  | Error e -> Alcotest.fail e
  | Ok key ->
    let msg = Bytes.of_string "test" in
    match Kes.sign ~kes_key:key ~message:msg ~period:64 with
    | Error _ -> ()  (* Expected: period 64 is out of range for depth 6 *)
    | Ok _ -> Alcotest.fail "expected error for period 64"

(* ================================================================ *)
(* KES key lifecycle                                                 *)
(* ================================================================ *)

let test_evolve () =
  match Kes.generate ~depth:1 with
  | Error e -> Alcotest.fail e
  | Ok key ->
    Alcotest.(check int) "period 0" 0 (Kes.period key);
    Alcotest.(check int) "remaining 2" 2 (Kes.remaining_periods key);
    (match Kes.evolve key with Ok () -> () | Error e -> Alcotest.fail e);
    Alcotest.(check int) "period 1" 1 (Kes.period key);
    Alcotest.(check int) "remaining 1" 1 (Kes.remaining_periods key);
    (match Kes.evolve key with Ok () -> () | Error e -> Alcotest.fail e);
    Alcotest.(check int) "remaining 0" 0 (Kes.remaining_periods key);
    match Kes.evolve key with
    | Error _ -> ()
    | Ok () -> Alcotest.fail "expected exhausted"

let test_vkey_derivation () =
  match Kes.generate ~depth:2 with
  | Error e -> Alcotest.fail e
  | Ok key ->
    let vk1 = Kes.vkey_of_key key in
    let vk2 = Kes.vkey_of_key key in
    Alcotest.(check int) "32 bytes" 32 (Bytes.length vk1);
    Alcotest.(check bool) "deterministic" true (Bytes.equal vk1 vk2)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "KES-Real"
    [ ( "Depth 0",
        [ Alcotest.test_case "sign/verify" `Quick test_depth0_sign_verify;
          Alcotest.test_case "wrong message" `Quick test_depth0_wrong_message ] );
      ( "Depth 1",
        [ Alcotest.test_case "both periods" `Quick test_depth1_both_periods ] );
      ( "Depth 6",
        [ Alcotest.test_case "sign/verify" `Quick test_depth6_sign_verify;
          Alcotest.test_case "later period" `Quick test_depth6_later_period;
          Alcotest.test_case "out of range" `Quick test_depth6_out_of_range ] );
      ( "Lifecycle",
        [ Alcotest.test_case "evolve" `Quick test_evolve;
          Alcotest.test_case "vkey derivation" `Quick test_vkey_derivation ] );
    ]
