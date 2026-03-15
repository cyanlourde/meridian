open Meridian

let bytes_of_hex s =
  let s = String.to_seq s |> Seq.filter (fun c -> c <> ' ') |> String.of_seq in
  let n = String.length s / 2 in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set_uint8 b i (int_of_string ("0x" ^ String.sub s (i * 2) 2))
  done;
  b

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    b;
  Buffer.contents buf

let bytes_testable =
  Alcotest.testable
    (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b))
    Bytes.equal

let check_encode msg expected_hex value =
  let encoded = Cbor.encode value in
  Alcotest.check bytes_testable msg (bytes_of_hex expected_hex) encoded

let check_roundtrip msg value =
  let encoded = Cbor.encode value in
  match Cbor.decode encoded with
  | Ok decoded -> Alcotest.check cbor_testable msg value decoded
  | Error e -> Alcotest.fail (Printf.sprintf "decode failed: %s" e)

and cbor_testable =
  let rec equal a b =
    match (a, b) with
    | Cbor.Uint a, Cbor.Uint b -> Int64.equal a b
    | Cbor.Nint a, Cbor.Nint b -> Int64.equal a b
    | Cbor.Bytes a, Cbor.Bytes b -> Bytes.equal a b
    | Cbor.Text a, Cbor.Text b -> String.equal a b
    | Cbor.Array a, Cbor.Array b ->
      List.length a = List.length b && List.for_all2 equal a b
    | Cbor.Map a, Cbor.Map b ->
      List.length a = List.length b
      && List.for_all2 (fun (k1, v1) (k2, v2) -> equal k1 k2 && equal v1 v2) a b
    | Cbor.Tag (t1, v1), Cbor.Tag (t2, v2) -> Int64.equal t1 t2 && equal v1 v2
    | Cbor.Bool a, Cbor.Bool b -> Bool.equal a b
    | Cbor.Null, Cbor.Null -> true
    | Cbor.Undefined, Cbor.Undefined -> true
    | Cbor.Float a, Cbor.Float b ->
      Float.equal a b || (Float.is_nan a && Float.is_nan b)
    | Cbor.Simple a, Cbor.Simple b -> Int.equal a b
    | Cbor.IndefiniteBytes a, Cbor.IndefiniteBytes b ->
      List.length a = List.length b
      && List.for_all2 Bytes.equal a b
    | Cbor.IndefiniteText a, Cbor.IndefiniteText b ->
      List.length a = List.length b
      && List.for_all2 String.equal a b
    | Cbor.IndefiniteArray a, Cbor.IndefiniteArray b ->
      List.length a = List.length b && List.for_all2 equal a b
    | Cbor.IndefiniteMap a, Cbor.IndefiniteMap b ->
      List.length a = List.length b
      && List.for_all2 (fun (k1, v1) (k2, v2) -> equal k1 k2 && equal v1 v2) a b
    | _ -> false
  in
  let rec pp fmt = function
    | Cbor.Uint n -> Format.fprintf fmt "Uint(%Ld)" n
    | Cbor.Nint n -> Format.fprintf fmt "Nint(%Ld)" n
    | Cbor.Bytes b -> Format.fprintf fmt "Bytes(%s)" (hex_of_bytes b)
    | Cbor.Text s -> Format.fprintf fmt "Text(%S)" s
    | Cbor.Array items ->
      Format.fprintf fmt "Array[%a]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp)
        items
    | Cbor.Map pairs ->
      Format.fprintf fmt "Map{%a}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (k, v) -> Format.fprintf fmt "%a: %a" pp k pp v))
        pairs
    | Cbor.Tag (t, v) -> Format.fprintf fmt "Tag(%Ld, %a)" t pp v
    | Cbor.Bool b -> Format.fprintf fmt "Bool(%b)" b
    | Cbor.Null -> Format.fprintf fmt "Null"
    | Cbor.Undefined -> Format.fprintf fmt "Undefined"
    | Cbor.Float f -> Format.fprintf fmt "Float(%g)" f
    | Cbor.Simple n -> Format.fprintf fmt "Simple(%d)" n
    | Cbor.IndefiniteBytes _ -> Format.fprintf fmt "IndefiniteBytes(...)"
    | Cbor.IndefiniteText _ -> Format.fprintf fmt "IndefiniteText(...)"
    | Cbor.IndefiniteArray items ->
      Format.fprintf fmt "IndefiniteArray[%a]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp)
        items
    | Cbor.IndefiniteMap _ -> Format.fprintf fmt "IndefiniteMap(...)"
  in
  Alcotest.testable pp equal

(* ---- Major Type 0: Unsigned integers ---- *)

let test_uint_small () =
  check_encode "uint 0" "00" (Cbor.Uint 0L);
  check_encode "uint 1" "01" (Cbor.Uint 1L);
  check_encode "uint 10" "0a" (Cbor.Uint 10L);
  check_encode "uint 23" "17" (Cbor.Uint 23L)

let test_uint_one_byte () =
  check_encode "uint 24" "1818" (Cbor.Uint 24L);
  check_encode "uint 25" "1819" (Cbor.Uint 25L);
  check_encode "uint 100" "1864" (Cbor.Uint 100L);
  check_encode "uint 255" "18ff" (Cbor.Uint 255L)

let test_uint_two_bytes () =
  check_encode "uint 256" "190100" (Cbor.Uint 256L);
  check_encode "uint 500" "1901f4" (Cbor.Uint 500L);
  check_encode "uint 1000" "1903e8" (Cbor.Uint 1000L);
  check_encode "uint 65535" "19ffff" (Cbor.Uint 65535L)

let test_uint_four_bytes () =
  check_encode "uint 65536" "1a00010000" (Cbor.Uint 65536L);
  check_encode "uint 1000000" "1a000f4240" (Cbor.Uint 1000000L)

let test_uint_eight_bytes () =
  check_encode "uint 2^32" "1b0000000100000000" (Cbor.Uint 4294967296L);
  check_encode "uint large"
    "1b000000e8d4a51000"
    (Cbor.Uint 1000000000000L)

(* ---- Major Type 1: Negative integers ---- *)

let test_nint () =
  check_encode "nint -1" "20" (Cbor.Nint (-1L));
  check_encode "nint -10" "29" (Cbor.Nint (-10L));
  check_encode "nint -24" "37" (Cbor.Nint (-24L));
  check_encode "nint -25" "3818" (Cbor.Nint (-25L));
  check_encode "nint -100" "3863" (Cbor.Nint (-100L));
  check_encode "nint -500" "3901f3" (Cbor.Nint (-500L));
  check_encode "nint -1000" "3903e7" (Cbor.Nint (-1000L))

(* ---- Major Type 2: Byte strings ---- *)

let test_bytes () =
  check_encode "empty bytes" "40" (Cbor.Bytes Bytes.empty);
  check_encode "4 bytes" "4401020304"
    (Cbor.Bytes (bytes_of_hex "01020304"))

(* ---- Major Type 3: Text strings ---- *)

let test_text () =
  check_encode "empty text" "60" (Cbor.Text "");
  check_encode "text 'a'" "6161" (Cbor.Text "a");
  check_encode "text 'IETF'" "6449455446" (Cbor.Text "IETF");
  check_encode "text with escapes" "62225c" (Cbor.Text "\"\\")

(* ---- Major Type 4: Arrays ---- *)

let test_array () =
  check_encode "empty array" "80" (Cbor.Array []);
  check_encode "array [1,2,3]" "83010203"
    (Cbor.Array [ Cbor.Uint 1L; Cbor.Uint 2L; Cbor.Uint 3L ]);
  check_encode "nested array" "8301820203820405"
    (Cbor.Array
       [ Cbor.Uint 1L;
         Cbor.Array [ Cbor.Uint 2L; Cbor.Uint 3L ];
         Cbor.Array [ Cbor.Uint 4L; Cbor.Uint 5L ] ])

let test_array_25_items () =
  (* 25 items: ai=24 means 1 extra length byte. 0x98=array major+24, 0x19=25 *)
  let items = List.init 25 (fun i -> Cbor.Uint (Int64.of_int i)) in
  let encoded = Cbor.encode (Cbor.Array items) in
  Alcotest.(check int) "25-item first byte" 0x98 (Bytes.get_uint8 encoded 0);
  Alcotest.(check int) "25-item length byte" 25 (Bytes.get_uint8 encoded 1)

(* ---- Major Type 5: Maps ---- *)

let test_map () =
  check_encode "empty map" "a0" (Cbor.Map []);
  check_encode "map {1:2, 3:4}" "a201020304"
    (Cbor.Map [ (Cbor.Uint 1L, Cbor.Uint 2L); (Cbor.Uint 3L, Cbor.Uint 4L) ])

(* ---- Major Type 6: Tags ---- *)

let test_tag () =
  (* Tag 1 wrapping integer (epoch time) *)
  check_encode "tag 1" "c11a514b67b0"
    (Cbor.Tag (1L, Cbor.Uint 1363896240L));
  (* Tag 0 wrapping text *)
  check_encode "tag 0" "c074323031332d30332d32315432303a30343a30305a"
    (Cbor.Tag (0L, Cbor.Text "2013-03-21T20:04:00Z"))

(* ---- Major Type 7: Simple values and floats ---- *)

let test_simple_values () =
  check_encode "false" "f4" (Cbor.Bool false);
  check_encode "true" "f5" (Cbor.Bool true);
  check_encode "null" "f6" Cbor.Null;
  check_encode "undefined" "f7" Cbor.Undefined;
  check_encode "simple 16" "f0" (Cbor.Simple 16);
  check_encode "simple 255" "f8ff" (Cbor.Simple 255)

let test_float64 () =
  check_encode "float 1.1" "fb3ff199999999999a" (Cbor.Float 1.1);
  check_encode "float 1.0e300" "fb7e37e43c8800759c" (Cbor.Float 1.0e300);
  check_encode "float -4.1" "fbc010666666666666" (Cbor.Float (-4.1))

(* ---- Indefinite-length encoding ---- *)

let test_indefinite_bytes () =
  (* 0x5F, chunk "01 02", chunk "03 04 05", 0xFF *)
  let value =
    Cbor.IndefiniteBytes [ bytes_of_hex "0102"; bytes_of_hex "030405" ]
  in
  check_encode "indefinite bytes" "5f42010243030405ff" value

let test_indefinite_text () =
  let value = Cbor.IndefiniteText [ "strea"; "ming" ] in
  check_encode "indefinite text" "7f657374726561646d696e67ff" value

let test_indefinite_array () =
  let value =
    Cbor.IndefiniteArray [ Cbor.Uint 1L; Cbor.Uint 2L; Cbor.Uint 3L ]
  in
  check_encode "indefinite array" "9f010203ff" value

let test_indefinite_map () =
  let value =
    Cbor.IndefiniteMap [ (Cbor.Uint 1L, Cbor.Uint 2L) ]
  in
  check_encode "indefinite map" "bf0102ff" value

(* ---- Round-trip tests ---- *)

let test_roundtrip_uint () =
  check_roundtrip "rt uint 0" (Cbor.Uint 0L);
  check_roundtrip "rt uint 23" (Cbor.Uint 23L);
  check_roundtrip "rt uint 24" (Cbor.Uint 24L);
  check_roundtrip "rt uint 255" (Cbor.Uint 255L);
  check_roundtrip "rt uint 256" (Cbor.Uint 256L);
  check_roundtrip "rt uint 65535" (Cbor.Uint 65535L);
  check_roundtrip "rt uint 65536" (Cbor.Uint 65536L);
  check_roundtrip "rt uint large" (Cbor.Uint 1000000000000L)

let test_roundtrip_nint () =
  check_roundtrip "rt nint -1" (Cbor.Nint (-1L));
  check_roundtrip "rt nint -24" (Cbor.Nint (-24L));
  check_roundtrip "rt nint -25" (Cbor.Nint (-25L));
  check_roundtrip "rt nint -1000" (Cbor.Nint (-1000L))

let test_roundtrip_bytes () =
  check_roundtrip "rt empty bytes" (Cbor.Bytes Bytes.empty);
  check_roundtrip "rt bytes"
    (Cbor.Bytes (bytes_of_hex "deadbeef"))

let test_roundtrip_text () =
  check_roundtrip "rt empty text" (Cbor.Text "");
  check_roundtrip "rt text" (Cbor.Text "hello world")

let test_roundtrip_array () =
  check_roundtrip "rt empty array" (Cbor.Array []);
  check_roundtrip "rt nested"
    (Cbor.Array
       [ Cbor.Uint 1L;
         Cbor.Array [ Cbor.Text "a"; Cbor.Null ];
         Cbor.Map [ (Cbor.Uint 0L, Cbor.Bool true) ] ])

let test_roundtrip_map () =
  check_roundtrip "rt empty map" (Cbor.Map []);
  check_roundtrip "rt map"
    (Cbor.Map
       [ (Cbor.Text "key", Cbor.Uint 42L);
         (Cbor.Uint 1L, Cbor.Array [ Cbor.Bool false ]) ])

let test_roundtrip_tag () =
  check_roundtrip "rt tag" (Cbor.Tag (1L, Cbor.Uint 1000L))

let test_roundtrip_simple () =
  check_roundtrip "rt false" (Cbor.Bool false);
  check_roundtrip "rt true" (Cbor.Bool true);
  check_roundtrip "rt null" Cbor.Null;
  check_roundtrip "rt undefined" Cbor.Undefined;
  check_roundtrip "rt simple 32" (Cbor.Simple 32);
  check_roundtrip "rt float" (Cbor.Float 3.14)

(* ---- Canonical encoding ---- *)

let test_canonical_map_ordering () =
  (* Keys should be sorted by encoded byte order *)
  let unordered =
    Cbor.Map
      [ (Cbor.Uint 10L, Cbor.Uint 1L);
        (Cbor.Uint 1L, Cbor.Uint 2L);
        (Cbor.Text "z", Cbor.Uint 3L);
        (Cbor.Text "a", Cbor.Uint 4L) ]
  in
  let enc1 = Cbor.encode_canonical unordered in
  let enc2 = Cbor.encode_canonical unordered in
  Alcotest.check bytes_testable "canonical is deterministic" enc1 enc2;
  (* Verify ordering: uint 1 (0x01) < uint 10 (0x0a) < text "a" (0x6161) < text "z" (0x617a) *)
  match Cbor.decode enc1 with
  | Error e -> Alcotest.fail e
  | Ok (Cbor.Map pairs) ->
    let keys =
      List.map
        (fun (k, _) ->
          match k with
          | Cbor.Uint n -> Printf.sprintf "uint:%Ld" n
          | Cbor.Text s -> Printf.sprintf "text:%s" s
          | _ -> "?")
        pairs
    in
    Alcotest.(check (list string))
      "map key order"
      [ "uint:1"; "uint:10"; "text:a"; "text:z" ]
      keys
  | Ok _ -> Alcotest.fail "expected map"

let test_canonical_deterministic () =
  let value =
    Cbor.Map
      [ (Cbor.Uint 3L, Cbor.Text "c");
        (Cbor.Uint 1L, Cbor.Text "a");
        (Cbor.Uint 2L, Cbor.Text "b") ]
  in
  let enc1 = Cbor.encode_canonical value in
  let enc2 = Cbor.encode_canonical value in
  Alcotest.check bytes_testable "deterministic output" enc1 enc2

let test_canonical_shortest_int () =
  (* Canonical encoding should use shortest integer form *)
  let enc = Cbor.encode_canonical (Cbor.Uint 23L) in
  Alcotest.(check int) "uint 23 is 1 byte" 1 (Bytes.length enc);
  let enc = Cbor.encode_canonical (Cbor.Uint 24L) in
  Alcotest.(check int) "uint 24 is 2 bytes" 2 (Bytes.length enc)

let test_canonical_float () =
  (* 0.0 should encode as float16 in canonical mode *)
  let enc = Cbor.encode_canonical (Cbor.Float 0.0) in
  Alcotest.(check int) "float 0.0 canonical = 3 bytes (f16)" 3 (Bytes.length enc);
  Alcotest.(check int) "float16 marker" 0xF9 (Bytes.get_uint8 enc 0);
  (* infinity should encode as float16 *)
  let enc = Cbor.encode_canonical (Cbor.Float infinity) in
  Alcotest.(check int) "infinity canonical = 3 bytes" 3 (Bytes.length enc);
  (* 1.1 cannot fit in float16 or float32, must be float64 *)
  let enc = Cbor.encode_canonical (Cbor.Float 1.1) in
  Alcotest.(check int) "float 1.1 canonical = 9 bytes (f64)" 9 (Bytes.length enc)

let test_canonical_indefinite_to_definite () =
  (* Canonical encoding should convert indefinite to definite *)
  let indef = Cbor.IndefiniteArray [ Cbor.Uint 1L; Cbor.Uint 2L ] in
  let defin = Cbor.Array [ Cbor.Uint 1L; Cbor.Uint 2L ] in
  let enc_indef = Cbor.encode_canonical indef in
  let enc_defin = Cbor.encode_canonical defin in
  Alcotest.check bytes_testable "indefinite->definite array" enc_defin enc_indef

(* ---- Edge cases ---- *)

let test_empty_structures () =
  check_roundtrip "empty array" (Cbor.Array []);
  check_roundtrip "empty map" (Cbor.Map []);
  check_roundtrip "empty bytes" (Cbor.Bytes Bytes.empty);
  check_roundtrip "empty text" (Cbor.Text "")

let test_large_integers () =
  check_roundtrip "max int32" (Cbor.Uint 2147483647L);
  check_roundtrip "max uint32" (Cbor.Uint 4294967295L);
  check_roundtrip "large uint64" (Cbor.Uint 1152921504606846975L);
  check_roundtrip "large nint" (Cbor.Nint (-2147483648L))

let test_nested_structures () =
  let deeply_nested =
    Cbor.Array
      [ Cbor.Array
          [ Cbor.Array
              [ Cbor.Array
                  [ Cbor.Map
                      [ ( Cbor.Text "deep",
                          Cbor.Tag
                            ( 1L,
                              Cbor.Array
                                [ Cbor.Uint 1L; Cbor.Null; Cbor.Bool true ] )
                        ) ] ] ] ] ]
  in
  check_roundtrip "deeply nested" deeply_nested

let test_decode_error () =
  (* Empty input *)
  (match Cbor.decode Bytes.empty with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "expected error on empty input");
  (* Truncated input *)
  (match Cbor.decode (bytes_of_hex "19") with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "expected error on truncated input");
  (* Trailing data *)
  match Cbor.decode (bytes_of_hex "0000") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error on trailing data"

let test_helpers () =
  Alcotest.check cbor_testable "encode_uint" (Cbor.Uint 42L) (Cbor.encode_uint 42L);
  Alcotest.check cbor_testable "encode_text" (Cbor.Text "hi") (Cbor.encode_text "hi");
  Alcotest.check cbor_testable "encode_bytes"
    (Cbor.Bytes (Bytes.of_string "ab"))
    (Cbor.encode_bytes (Bytes.of_string "ab"));
  Alcotest.check cbor_testable "encode_array"
    (Cbor.Array [ Cbor.Uint 1L ])
    (Cbor.encode_array [ Cbor.Uint 1L ]);
  Alcotest.check cbor_testable "encode_map"
    (Cbor.Map [ (Cbor.Uint 1L, Cbor.Uint 2L) ])
    (Cbor.encode_map [ (Cbor.Uint 1L, Cbor.Uint 2L) ]);
  Alcotest.check cbor_testable "encode_tag"
    (Cbor.Tag (1L, Cbor.Null))
    (Cbor.encode_tag 1L Cbor.Null)

let () =
  Alcotest.run "CBOR"
    [ ( "Major type 0 (uint)",
        [ Alcotest.test_case "small" `Quick test_uint_small;
          Alcotest.test_case "1-byte" `Quick test_uint_one_byte;
          Alcotest.test_case "2-byte" `Quick test_uint_two_bytes;
          Alcotest.test_case "4-byte" `Quick test_uint_four_bytes;
          Alcotest.test_case "8-byte" `Quick test_uint_eight_bytes ] );
      ( "Major type 1 (nint)",
        [ Alcotest.test_case "negative ints" `Quick test_nint ] );
      ( "Major type 2 (bytes)",
        [ Alcotest.test_case "byte strings" `Quick test_bytes ] );
      ( "Major type 3 (text)",
        [ Alcotest.test_case "text strings" `Quick test_text ] );
      ( "Major type 4 (array)",
        [ Alcotest.test_case "arrays" `Quick test_array;
          Alcotest.test_case "25-item array" `Quick test_array_25_items ] );
      ( "Major type 5 (map)",
        [ Alcotest.test_case "maps" `Quick test_map ] );
      ( "Major type 6 (tag)",
        [ Alcotest.test_case "tags" `Quick test_tag ] );
      ( "Major type 7 (simple/float)",
        [ Alcotest.test_case "simple values" `Quick test_simple_values;
          Alcotest.test_case "float64" `Quick test_float64 ] );
      ( "Indefinite-length",
        [ Alcotest.test_case "bytes" `Quick test_indefinite_bytes;
          Alcotest.test_case "text" `Quick test_indefinite_text;
          Alcotest.test_case "array" `Quick test_indefinite_array;
          Alcotest.test_case "map" `Quick test_indefinite_map ] );
      ( "Round-trip",
        [ Alcotest.test_case "uint" `Quick test_roundtrip_uint;
          Alcotest.test_case "nint" `Quick test_roundtrip_nint;
          Alcotest.test_case "bytes" `Quick test_roundtrip_bytes;
          Alcotest.test_case "text" `Quick test_roundtrip_text;
          Alcotest.test_case "array" `Quick test_roundtrip_array;
          Alcotest.test_case "map" `Quick test_roundtrip_map;
          Alcotest.test_case "tag" `Quick test_roundtrip_tag;
          Alcotest.test_case "simple/float" `Quick test_roundtrip_simple ] );
      ( "Canonical encoding",
        [ Alcotest.test_case "map ordering" `Quick test_canonical_map_ordering;
          Alcotest.test_case "deterministic" `Quick test_canonical_deterministic;
          Alcotest.test_case "shortest int" `Quick test_canonical_shortest_int;
          Alcotest.test_case "shortest float" `Quick test_canonical_float;
          Alcotest.test_case "indef->definite" `Quick test_canonical_indefinite_to_definite ] );
      ( "Edge cases",
        [ Alcotest.test_case "empty structures" `Quick test_empty_structures;
          Alcotest.test_case "large integers" `Quick test_large_integers;
          Alcotest.test_case "nested" `Quick test_nested_structures;
          Alcotest.test_case "decode errors" `Quick test_decode_error ] );
      ( "Helpers",
        [ Alcotest.test_case "constructors" `Quick test_helpers ] );
    ]
