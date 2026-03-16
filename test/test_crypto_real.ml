open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let () = Crypto.init ()

(* ================================================================ *)
(* Blake2b cross-check: pure OCaml vs libsodium                      *)
(* ================================================================ *)

let test_blake2b_cross_check_empty () =
  match Crypto.blake2b_256_cross_check Bytes.empty with
  | Ok hash ->
    Alcotest.(check int) "32 bytes" 32 (Bytes.length hash);
    let expected = "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8" in
    Alcotest.(check string) "empty" expected (hex_of_bytes hash)
  | Error e -> Alcotest.fail e

let test_blake2b_cross_check_abc () =
  match Crypto.blake2b_256_cross_check (Bytes.of_string "abc") with
  | Ok hash ->
    let expected = "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319" in
    Alcotest.(check string) "abc" expected (hex_of_bytes hash)
  | Error e -> Alcotest.fail e

let test_blake2b_cross_check_long () =
  let data = Bytes.init 1000 (fun i -> Char.chr (i mod 256)) in
  match Crypto.blake2b_256_cross_check data with
  | Ok hash -> Alcotest.(check int) "32 bytes" 32 (Bytes.length hash)
  | Error e -> Alcotest.fail e

let test_blake2b_sodium_256 () =
  let hash = Crypto.blake2b_256_sodium (Bytes.of_string "test") in
  let pure = Crypto.blake2b_256 (Bytes.of_string "test") in
  Alcotest.(check string) "match" (hex_of_bytes pure) (hex_of_bytes hash)

let test_blake2b_sodium_224 () =
  let hash = Crypto.blake2b_224_sodium (Bytes.of_string "test") in
  let pure = Crypto.blake2b_224 (Bytes.of_string "test") in
  Alcotest.(check string) "match" (hex_of_bytes pure) (hex_of_bytes hash)

(* ================================================================ *)
(* Ed25519: keypair, sign, verify                                    *)
(* ================================================================ *)

let test_ed25519_sign_verify () =
  match Crypto.ed25519_keypair () with
  | Error e -> Alcotest.fail e
  | Ok (pk, sk) ->
    Alcotest.(check int) "pk 32 bytes" 32 (Bytes.length pk);
    Alcotest.(check int) "sk 64 bytes" 64 (Bytes.length sk);
    let message = Bytes.of_string "hello world" in
    match Crypto.ed25519_sign ~secret_key:sk ~message with
    | Error e -> Alcotest.fail e
    | Ok signature ->
      Alcotest.(check int) "sig 64 bytes" 64 (Bytes.length signature);
      match Crypto.ed25519_verify ~public_key:pk ~message ~signature with
      | Ok true -> ()
      | Ok false -> Alcotest.fail "valid signature rejected"
      | Error e -> Alcotest.fail e

let test_ed25519_wrong_signature () =
  match Crypto.ed25519_keypair () with
  | Error e -> Alcotest.fail e
  | Ok (pk, sk) ->
    let message = Bytes.of_string "test message" in
    (match Crypto.ed25519_sign ~secret_key:sk ~message with
     | Error e -> Alcotest.fail e
     | Ok signature ->
       (* Flip one bit in the signature *)
       let bad_sig = Bytes.copy signature in
       Bytes.set_uint8 bad_sig 0 (Bytes.get_uint8 bad_sig 0 lxor 1);
       match Crypto.ed25519_verify ~public_key:pk ~message ~signature:bad_sig with
       | Ok false -> ()  (* correctly rejected *)
       | Ok true -> Alcotest.fail "modified signature accepted"
       | Error e -> Alcotest.fail e)

let test_ed25519_wrong_message () =
  match Crypto.ed25519_keypair () with
  | Error e -> Alcotest.fail e
  | Ok (pk, sk) ->
    let message = Bytes.of_string "original" in
    (match Crypto.ed25519_sign ~secret_key:sk ~message with
     | Error e -> Alcotest.fail e
     | Ok signature ->
       let wrong_msg = Bytes.of_string "modified" in
       match Crypto.ed25519_verify ~public_key:pk ~message:wrong_msg ~signature with
       | Ok false -> ()
       | Ok true -> Alcotest.fail "wrong message accepted"
       | Error e -> Alcotest.fail e)

let test_ed25519_wrong_key () =
  match Crypto.ed25519_keypair (), Crypto.ed25519_keypair () with
  | Ok (_, sk1), Ok (pk2, _) ->
    let message = Bytes.of_string "test" in
    (match Crypto.ed25519_sign ~secret_key:sk1 ~message with
     | Ok signature ->
       (match Crypto.ed25519_verify ~public_key:pk2 ~message ~signature with
        | Ok false -> ()
        | Ok true -> Alcotest.fail "wrong key accepted"
        | Error e -> Alcotest.fail e)
     | Error e -> Alcotest.fail e)
  | _ -> Alcotest.fail "keypair failed"

let test_ed25519_empty_message () =
  match Crypto.ed25519_keypair () with
  | Error e -> Alcotest.fail e
  | Ok (pk, sk) ->
    (match Crypto.ed25519_sign ~secret_key:sk ~message:Bytes.empty with
     | Error e -> Alcotest.fail e
     | Ok signature ->
       match Crypto.ed25519_verify ~public_key:pk ~message:Bytes.empty ~signature with
       | Ok true -> ()
       | Ok false -> Alcotest.fail "empty message rejected"
       | Error e -> Alcotest.fail e)

let test_ed25519_bad_key_size () =
  let bad_pk = Bytes.make 16 '\x00' in
  let msg = Bytes.of_string "test" in
  let sig_ = Bytes.make 64 '\x00' in
  match Crypto.ed25519_verify ~public_key:bad_pk ~message:msg ~signature:sig_ with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for wrong key size"

(* ================================================================ *)
(* VRF/KES stubs still return errors                                 *)
(* ================================================================ *)

let test_vrf_stub () =
  match Crypto.vrf_verify ~public_key:Bytes.empty ~proof:Bytes.empty ~message:Bytes.empty with
  | Error _ -> ()
  | Ok (_, false) -> () (* real VRF rejects invalid input *)
  | Ok (_, true) -> Alcotest.fail "expected stub error or verification failure"

let test_kes_stub () =
  match Crypto.kes_verify ~public_key:Bytes.empty ~period:0 ~message:Bytes.empty ~signature:Bytes.empty with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected stub error"

(* ================================================================ *)
(* Libsodium availability                                            *)
(* ================================================================ *)

let test_sodium_init () =
  Alcotest.(check bool) "libsodium loaded" true !(Crypto.libsodium_available)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Crypto-Real"
    [ ( "Blake2b cross-check",
        [ Alcotest.test_case "empty" `Quick test_blake2b_cross_check_empty;
          Alcotest.test_case "abc" `Quick test_blake2b_cross_check_abc;
          Alcotest.test_case "long" `Quick test_blake2b_cross_check_long;
          Alcotest.test_case "sodium 256" `Quick test_blake2b_sodium_256;
          Alcotest.test_case "sodium 224" `Quick test_blake2b_sodium_224 ] );
      ( "Ed25519",
        [ Alcotest.test_case "sign/verify" `Quick test_ed25519_sign_verify;
          Alcotest.test_case "wrong signature" `Quick test_ed25519_wrong_signature;
          Alcotest.test_case "wrong message" `Quick test_ed25519_wrong_message;
          Alcotest.test_case "wrong key" `Quick test_ed25519_wrong_key;
          Alcotest.test_case "empty message" `Quick test_ed25519_empty_message;
          Alcotest.test_case "bad key size" `Quick test_ed25519_bad_key_size ] );
      ( "Stubs",
        [ Alcotest.test_case "VRF" `Quick test_vrf_stub;
          Alcotest.test_case "KES" `Quick test_kes_stub ] );
      ( "Init",
        [ Alcotest.test_case "sodium loaded" `Quick test_sodium_init ] );
    ]
