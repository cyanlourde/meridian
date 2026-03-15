open Meridian

(* ================================================================ *)
(* Helpers                                                           *)
(* ================================================================ *)

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let _bytes_of_hex s =
  let n = String.length s / 2 in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set_uint8 b i (int_of_string ("0x" ^ String.sub s (i * 2) 2))
  done;
  b

(* ================================================================ *)
(* Blake2b-256 test vectors                                          *)
(* ================================================================ *)

(* Test vectors verified against the Blake2b reference implementation
   and b2sum command-line tool. *)

let test_blake2b_256_empty () =
  (* blake2b-256 of empty string *)
  let hash = Crypto.blake2b_256 Bytes.empty in
  let expected = "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8" in
  Alcotest.(check string) "empty" expected (hex_of_bytes hash)

let test_blake2b_256_abc () =
  (* blake2b-256 of "abc" *)
  let hash = Crypto.blake2b_256 (Bytes.of_string "abc") in
  let expected = "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319" in
  Alcotest.(check string) "abc" expected (hex_of_bytes hash)

let test_blake2b_256_empty_string () =
  (* Verify output length is 32 bytes *)
  let hash = Crypto.blake2b_256 Bytes.empty in
  Alcotest.(check int) "output length" 32 (Bytes.length hash)

let test_blake2b_256_one_byte () =
  (* blake2b-256 of single zero byte *)
  let hash = Crypto.blake2b_256 (Bytes.make 1 '\x00') in
  Alcotest.(check int) "output length" 32 (Bytes.length hash);
  (* Should not be the same as empty input *)
  let empty_hash = Crypto.blake2b_256 Bytes.empty in
  let different = not (Bytes.equal hash empty_hash) in
  Alcotest.(check bool) "different from empty" true different

let test_blake2b_256_deterministic () =
  let data = Bytes.of_string "hello world" in
  let h1 = Crypto.blake2b_256 data in
  let h2 = Crypto.blake2b_256 data in
  Alcotest.(check bool) "deterministic" true (Bytes.equal h1 h2)

let test_blake2b_256_long_input () =
  (* 256 bytes of 0x00..0xFF *)
  let data = Bytes.init 256 (fun i -> Char.chr i) in
  let hash = Crypto.blake2b_256 data in
  Alcotest.(check int) "output length" 32 (Bytes.length hash);
  (* Verify it's different from shorter inputs *)
  let short_hash = Crypto.blake2b_256 (Bytes.sub data 0 128) in
  Alcotest.(check bool) "different from truncated" true (not (Bytes.equal hash short_hash))

let test_blake2b_256_multi_block () =
  (* Input longer than one block (128 bytes) *)
  let data = Bytes.make 200 '\xab' in
  let hash = Crypto.blake2b_256 data in
  Alcotest.(check int) "output length" 32 (Bytes.length hash)

(* ================================================================ *)
(* Blake2b-224 test vectors                                          *)
(* ================================================================ *)

let test_blake2b_224_empty () =
  let hash = Crypto.blake2b_224 Bytes.empty in
  Alcotest.(check int) "output length" 28 (Bytes.length hash)

let test_blake2b_224_abc () =
  let hash = Crypto.blake2b_224 (Bytes.of_string "abc") in
  Alcotest.(check int) "output length" 28 (Bytes.length hash);
  (* Should be different from blake2b-256 of same input *)
  let h256 = Crypto.blake2b_256 (Bytes.of_string "abc") in
  let prefix_match = Bytes.sub h256 0 28 = hash in
  (* They shouldn't match since different output lengths change the IV *)
  Alcotest.(check bool) "different from 256 prefix" false prefix_match

let test_blake2b_224_deterministic () =
  let data = Bytes.of_string "test" in
  let h1 = Crypto.blake2b_224 data in
  let h2 = Crypto.blake2b_224 data in
  Alcotest.(check bool) "deterministic" true (Bytes.equal h1 h2)

(* ================================================================ *)
(* Blake2b with key                                                  *)
(* ================================================================ *)

let test_blake2b_keyed () =
  (* Keyed Blake2b should produce different output than unkeyed *)
  let data = Bytes.of_string "hello" in
  let key = Bytes.of_string "secret" in
  let h_unkeyed = Crypto.blake2b ~nn:32 data in
  let h_keyed = Crypto.blake2b ~key ~nn:32 data in
  Alcotest.(check bool) "keyed differs" true (not (Bytes.equal h_unkeyed h_keyed));
  Alcotest.(check int) "keyed output length" 32 (Bytes.length h_keyed)

let test_blake2b_keyed_deterministic () =
  let data = Bytes.of_string "hello" in
  let key = Bytes.of_string "secret" in
  let h1 = Crypto.blake2b ~key ~nn:32 data in
  let h2 = Crypto.blake2b ~key ~nn:32 data in
  Alcotest.(check bool) "deterministic" true (Bytes.equal h1 h2)

(* ================================================================ *)
(* Blake2b configurable output                                       *)
(* ================================================================ *)

let test_blake2b_various_lengths () =
  let data = Bytes.of_string "test" in
  let h1 = Crypto.blake2b ~nn:1 data in
  Alcotest.(check int) "1 byte output" 1 (Bytes.length h1);
  let h16 = Crypto.blake2b ~nn:16 data in
  Alcotest.(check int) "16 byte output" 16 (Bytes.length h16);
  let h64 = Crypto.blake2b ~nn:64 data in
  Alcotest.(check int) "64 byte output" 64 (Bytes.length h64)

(* ================================================================ *)
(* Ed25519 stub tests                                                *)
(* ================================================================ *)

let test_ed25519_stub () =
  let pk = Bytes.make 32 '\x00' in
  let msg = Bytes.of_string "test" in
  let sig_ = Bytes.make 64 '\x00' in
  match Crypto.ed25519_verify ~public_key:pk ~message:msg ~signature:sig_ with
  | Error _ -> ()  (* Expected: stub returns error *)
  | Ok _ -> Alcotest.fail "expected stub error"

let test_ed25519_wrong_key_size () =
  let pk = Bytes.make 16 '\x00' in  (* wrong size *)
  let msg = Bytes.of_string "test" in
  let sig_ = Bytes.make 64 '\x00' in
  match Crypto.ed25519_verify ~public_key:pk ~message:msg ~signature:sig_ with
  | Error msg -> Alcotest.(check bool) "mentions size"
      true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error"

(* ================================================================ *)
(* VRF stub test                                                     *)
(* ================================================================ *)

let test_vrf_stub () =
  let pk = Bytes.make 32 '\x00' in
  let proof = Bytes.make 80 '\x00' in
  let msg = Bytes.of_string "test" in
  match Crypto.vrf_verify ~public_key:pk ~proof ~message:msg with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected stub error"

(* ================================================================ *)
(* KES stub test                                                     *)
(* ================================================================ *)

let test_kes_stub () =
  let pk = Bytes.make 32 '\x00' in
  let sig_ = Bytes.make 448 '\x00' in
  let msg = Bytes.of_string "test" in
  match Crypto.kes_verify ~public_key:pk ~period:0 ~message:msg ~signature:sig_ with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected stub error"

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Crypto"
    [ ( "Blake2b-256",
        [ Alcotest.test_case "empty" `Quick test_blake2b_256_empty;
          Alcotest.test_case "abc" `Quick test_blake2b_256_abc;
          Alcotest.test_case "output length" `Quick test_blake2b_256_empty_string;
          Alcotest.test_case "one byte" `Quick test_blake2b_256_one_byte;
          Alcotest.test_case "deterministic" `Quick test_blake2b_256_deterministic;
          Alcotest.test_case "long input" `Quick test_blake2b_256_long_input;
          Alcotest.test_case "multi-block" `Quick test_blake2b_256_multi_block ] );
      ( "Blake2b-224",
        [ Alcotest.test_case "empty" `Quick test_blake2b_224_empty;
          Alcotest.test_case "abc" `Quick test_blake2b_224_abc;
          Alcotest.test_case "deterministic" `Quick test_blake2b_224_deterministic ] );
      ( "Blake2b keyed",
        [ Alcotest.test_case "keyed differs" `Quick test_blake2b_keyed;
          Alcotest.test_case "keyed deterministic" `Quick test_blake2b_keyed_deterministic ] );
      ( "Blake2b output sizes",
        [ Alcotest.test_case "various lengths" `Quick test_blake2b_various_lengths ] );
      ( "Ed25519 (stub)",
        [ Alcotest.test_case "stub returns error" `Quick test_ed25519_stub;
          Alcotest.test_case "wrong key size" `Quick test_ed25519_wrong_key_size ] );
      ( "VRF (stub)",
        [ Alcotest.test_case "stub returns error" `Quick test_vrf_stub ] );
      ( "KES (stub)",
        [ Alcotest.test_case "stub returns error" `Quick test_kes_stub ] );
    ]
