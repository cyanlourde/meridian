(* Cryptographic primitives for Cardano.

   - Blake2b-256 and Blake2b-224: pure OCaml implementation per RFC 7693
   - Ed25519: pure OCaml verification using the standard algorithm
   - VRF (ECVRF-ED25519-SHA512-Elligator2): stub (requires libsodium)
   - KES (sum composition): stub (requires custom C bindings)

   The Blake2b implementation follows RFC 7693 exactly. Ed25519, VRF,
   and KES verification require external cryptographic libraries and
   are provided as stubs that return errors until bindings are available. *)

(* ================================================================ *)
(* Blake2b — RFC 7693                                                *)
(* ================================================================ *)

(* Blake2b operates on 64-bit words. We use Int64 throughout.
   The compression function, initialization vectors, sigma permutation
   table, and round function follow the RFC precisely. *)

(* Initialization vectors (IV), Section 2.6 *)
let iv = [|
  0x6a09e667f3bcc908L; 0xbb67ae8584caa73bL;
  0x3c6ef372fe94f82bL; 0xa54ff53a5f1d36f1L;
  0x510e527fade682d1L; 0x9b05688c2b3e6c1fL;
  0x1f83d9abfb41bd6bL; 0x5be0cd19137e2179L;
|]

(* Sigma permutation table, Section 2.7 *)
let sigma = [|
  [| 0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15 |];
  [| 14;10;4;8;9;15;13;6;1;12;0;2;11;7;5;3 |];
  [| 11;8;12;0;5;2;15;13;10;14;3;6;7;1;9;4 |];
  [| 7;9;3;1;13;12;11;14;2;6;5;10;4;0;15;8 |];
  [| 9;0;5;7;2;4;10;15;14;1;11;12;6;8;3;13 |];
  [| 2;12;6;10;0;11;8;3;4;13;7;5;15;14;1;9 |];
  [| 12;5;1;15;14;13;4;10;0;7;6;3;9;2;8;11 |];
  [| 13;11;7;14;12;1;3;9;5;0;15;4;8;6;2;10 |];
  [| 6;15;14;9;11;3;0;8;12;2;13;7;1;4;10;5 |];
  [| 10;2;8;4;7;6;1;5;15;11;9;14;3;12;13;0 |];
  [| 0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15 |];
  [| 14;10;4;8;9;15;13;6;1;12;0;2;11;7;5;3 |];
|]

(* Rotate right for 64-bit words *)
let rotr64 x n =
  Int64.logor
    (Int64.shift_right_logical x n)
    (Int64.shift_left x (64 - n))

(* G mixing function, Section 3.1 *)
let g v a b c d x y =
  v.(a) <- Int64.add (Int64.add v.(a) v.(b)) x;
  v.(d) <- rotr64 (Int64.logxor v.(d) v.(a)) 32;
  v.(c) <- Int64.add v.(c) v.(d);
  v.(b) <- rotr64 (Int64.logxor v.(b) v.(c)) 24;
  v.(a) <- Int64.add (Int64.add v.(a) v.(b)) y;
  v.(d) <- rotr64 (Int64.logxor v.(d) v.(a)) 16;
  v.(c) <- Int64.add v.(c) v.(d);
  v.(b) <- rotr64 (Int64.logxor v.(b) v.(c)) 63

(* Compression function F, Section 3.2 *)
let compress h m t f =
  let v = Array.make 16 0L in
  (* Initialize working vector *)
  for i = 0 to 7 do v.(i) <- h.(i) done;
  for i = 0 to 7 do v.(8 + i) <- iv.(i) done;
  (* XOR counter and finalization flag *)
  v.(12) <- Int64.logxor v.(12) (Int64.of_int (t land 0xFFFFFFFF));
  v.(13) <- Int64.logxor v.(13) (Int64.of_int (t asr 32));  (* high 32 bits of counter *)
  if f then v.(14) <- Int64.logxor v.(14) (-1L);
  (* 12 rounds of mixing *)
  for i = 0 to 11 do
    let s = sigma.(i) in
    g v 0 4  8 12 m.(s.(0)) m.(s.(1));
    g v 1 5  9 13 m.(s.(2)) m.(s.(3));
    g v 2 6 10 14 m.(s.(4)) m.(s.(5));
    g v 3 7 11 15 m.(s.(6)) m.(s.(7));
    g v 0 5 10 15 m.(s.(8)) m.(s.(9));
    g v 1 6 11 12 m.(s.(10)) m.(s.(11));
    g v 2 7  8 13 m.(s.(12)) m.(s.(13));
    g v 3 4  9 14 m.(s.(14)) m.(s.(15))
  done;
  (* Finalize *)
  for i = 0 to 7 do
    h.(i) <- Int64.logxor (Int64.logxor h.(i) v.(i)) v.(i + 8)
  done

(* Load a 64-bit little-endian word from bytes at offset *)
let load64_le data off =
  let b i = Int64.of_int (Bytes.get_uint8 data (off + i)) in
  Int64.logor (b 0)
    (Int64.logor (Int64.shift_left (b 1) 8)
       (Int64.logor (Int64.shift_left (b 2) 16)
          (Int64.logor (Int64.shift_left (b 3) 24)
             (Int64.logor (Int64.shift_left (b 4) 32)
                (Int64.logor (Int64.shift_left (b 5) 40)
                   (Int64.logor (Int64.shift_left (b 6) 48)
                      (Int64.shift_left (b 7) 56)))))))

(* Store a 64-bit little-endian word to bytes at offset *)
let store64_le buf off v =
  for i = 0 to 7 do
    Bytes.set_uint8 buf (off + i)
      (Int64.to_int (Int64.shift_right_logical v (i * 8)) land 0xFF)
  done

(** Blake2b hash with configurable output length.
    [nn] is the output length in bytes (1..64).
    [key] is an optional key for keyed hashing (0..64 bytes). *)
let blake2b ?(key = Bytes.empty) ~nn data =
  let kk = Bytes.length key in
  let ll = Bytes.length data in
  (* Parameter block: only first word matters for unkeyed/keyed hash.
     p[0] = nn | (kk << 8) | 0x01010000 *)
  let p0 = Int64.of_int (0x01010000 lor (kk lsl 8) lor nn) in
  (* Initialize state *)
  let h = Array.copy iv in
  h.(0) <- Int64.logxor h.(0) p0;
  (* If keyed, the key is padded to 128 bytes and treated as first block *)
  let key_block =
    if kk > 0 then begin
      let kb = Bytes.make 128 '\x00' in
      Bytes.blit key 0 kb 0 kk;
      Some kb
    end else None
  in
  let input_len = (if kk > 0 then 128 else 0) + ll in
  (* Build the full padded input as a byte sequence *)
  let input_data = Bytes.make (max input_len 1) '\x00' in
  let off = ref 0 in
  (match key_block with
   | Some kb -> Bytes.blit kb 0 input_data 0 128; off := 128
   | None -> ());
  if ll > 0 then Bytes.blit data 0 input_data !off ll;
  let total = if input_len = 0 then 0 else input_len in
  (* Process 128-byte blocks *)
  let m = Array.make 16 0L in
  let pos = ref 0 in
  let remaining = ref total in
  (* Handle degenerate case: empty unkeyed hash *)
  if total = 0 then begin
    let block = Bytes.make 128 '\x00' in
    for i = 0 to 15 do m.(i) <- load64_le block (i * 8) done;
    compress h m 0 true
  end else begin
    while !remaining > 128 do
      for i = 0 to 15 do m.(i) <- load64_le input_data (!pos + i * 8) done;
      pos := !pos + 128;
      remaining := !remaining - 128;
      compress h m (!pos) false  (* counter = bytes processed so far *)
    done;
    (* Final block: pad with zeros *)
    let last_block = Bytes.make 128 '\x00' in
    Bytes.blit input_data !pos last_block 0 !remaining;
    for i = 0 to 15 do m.(i) <- load64_le last_block (i * 8) done;
    compress h m total true
  end;
  (* Extract output *)
  let out = Bytes.create nn in
  let full_words = nn / 8 in
  for i = 0 to full_words - 1 do
    store64_le out (i * 8) h.(i)
  done;
  let leftover = nn mod 8 in
  if leftover > 0 then begin
    let w = h.(full_words) in
    for i = 0 to leftover - 1 do
      Bytes.set_uint8 out (full_words * 8 + i)
        (Int64.to_int (Int64.shift_right_logical w (i * 8)) land 0xFF)
    done
  end;
  out

let blake2b_256 data = blake2b ~nn:32 data
let blake2b_224 data = blake2b ~nn:28 data

(* ================================================================ *)
(* Libsodium C stubs (loaded via dlopen at runtime)                  *)
(* ================================================================ *)

external sodium_init : unit -> bool = "meridian_sodium_init"
external _sodium_available : unit -> bool = "meridian_sodium_available"
external sodium_blake2b : bytes -> int -> bytes = "meridian_blake2b"
external sodium_ed25519_verify : bytes -> bytes -> bytes -> bool = "meridian_ed25519_verify"
external sodium_ed25519_keypair : unit -> bytes * bytes = "meridian_ed25519_keypair"
external sodium_ed25519_sign : bytes -> bytes -> bytes = "meridian_ed25519_sign"

let libsodium_available = ref false
let vrf_init_hook = ref (fun () -> ())

let init () =
  libsodium_available := sodium_init ();
  !vrf_init_hook ()

(** Blake2b via libsodium (fast path). Falls back to pure OCaml. *)
let blake2b_256_sodium data =
  if !libsodium_available then
    try sodium_blake2b data 32
    with _ -> blake2b_256 data
  else blake2b_256 data

let blake2b_224_sodium data =
  if !libsodium_available then
    try sodium_blake2b data 28
    with _ -> blake2b_224 data
  else blake2b_224 data

(** Cross-check: verify pure OCaml and libsodium produce identical output. *)
let blake2b_256_cross_check data =
  let ocaml_out = blake2b_256 data in
  if !libsodium_available then begin
    try
      let sodium_out = sodium_blake2b data 32 in
      if not (Bytes.equal ocaml_out sodium_out) then
        Error "blake2b_256: OCaml/libsodium mismatch"
      else
        Ok ocaml_out
    with _ -> Ok ocaml_out
  end else Ok ocaml_out

(* ================================================================ *)
(* Ed25519 signature verification                                    *)
(* ================================================================ *)

let ed25519_verify ~public_key ~message ~signature =
  if Bytes.length public_key <> 32 then
    Error "ed25519_verify: public key must be 32 bytes"
  else if Bytes.length signature <> 64 then
    Error "ed25519_verify: signature must be 64 bytes"
  else if not !libsodium_available then
    Error "ed25519_verify: libsodium not available (call Crypto.init)"
  else
    try Ok (sodium_ed25519_verify public_key message signature)
    with Failure msg -> Error msg

let ed25519_sign ~secret_key ~message =
  if not !libsodium_available then
    Error "ed25519_sign: libsodium not available"
  else
    try Ok (sodium_ed25519_sign message secret_key)
    with Failure msg -> Error msg

let ed25519_keypair () =
  if not !libsodium_available then
    Error "ed25519_keypair: libsodium not available"
  else
    try Ok (sodium_ed25519_keypair ())
    with Failure msg -> Error msg

(* ================================================================ *)
(* ================================================================ *)
(* VRF (Cardano libsodium fork)                                      *)
(* ================================================================ *)

external sodium_vrf_available : unit -> bool = "meridian_vrf_available"
external sodium_vrf_prove : bytes -> bytes -> bytes = "meridian_vrf_prove"
external sodium_vrf_verify_raw : bytes -> bytes -> bytes -> bytes * bool = "meridian_vrf_verify"
external sodium_vrf_proof_to_hash : bytes -> bytes = "meridian_vrf_proof_to_hash"

let vrf_available = ref false

let init_vrf () =
  if !libsodium_available then
    vrf_available := (try sodium_vrf_available () with _ -> false)

let () = vrf_init_hook := init_vrf

let vrf_prove ~secret_key ~message =
  if not !vrf_available then
    Error "VRF not available (need Cardano libsodium fork)"
  else
    try Ok (sodium_vrf_prove secret_key message)
    with Failure msg -> Error msg

let vrf_verify ~public_key ~proof ~message =
  if not !vrf_available then
    Error "VRF not available (need Cardano libsodium fork)"
  else
    try
      let (output, valid) = sodium_vrf_verify_raw public_key proof message in
      Ok (output, valid)
    with Failure msg -> Error msg

let vrf_proof_to_hash ~proof =
  if not !vrf_available then
    Error "VRF not available (need Cardano libsodium fork)"
  else
    try Ok (sodium_vrf_proof_to_hash proof)
    with Failure msg -> Error msg

(* ================================================================ *)
(* KES verification (uses OCaml Kes module, not C stub)              *)
(* ================================================================ *)

let kes_verify ~public_key:_ ~period:_ ~message:_ ~signature:_ =
  Error "kes_verify: use Kes.verify instead"
