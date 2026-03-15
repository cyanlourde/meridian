type cbor_value =
  | Uint of int64
  | Nint of int64
  | Bytes of bytes
  | Text of string
  | Array of cbor_value list
  | Map of (cbor_value * cbor_value) list
  | Tag of int64 * cbor_value
  | Bool of bool
  | Null
  | Undefined
  | Float of float
  | Simple of int
  | IndefiniteBytes of bytes list
  | IndefiniteText of string list
  | IndefiniteArray of cbor_value list
  | IndefiniteMap of (cbor_value * cbor_value) list

let ( let* ) = Result.bind

(* ---------- Float16 (IEEE 754 binary16) support ---------- *)

let half_to_float h =
  let sign = (h lsr 15) land 1 in
  let exp = (h lsr 10) land 0x1F in
  let mant = h land 0x3FF in
  let f =
    if exp = 0 then
      if mant = 0 then 0.0
      else ldexp (float_of_int mant) (-24)
    else if exp = 31 then
      if mant = 0 then infinity else nan
    else ldexp (float_of_int (0x400 lor mant)) (exp - 25)
  in
  if sign = 1 then Float.neg f else f

let float_to_half f =
  let bits = Int64.bits_of_float f in
  let sign = Int64.to_int (Int64.shift_right_logical bits 63) in
  let exp = Int64.to_int (Int64.logand (Int64.shift_right_logical bits 52) 0x7FFL) in
  let mant = Int64.logand bits 0xFFFFFFFFFFFFFL in
  if exp = 0x7FF then
    if mant = 0L then Some ((sign lsl 15) lor 0x7C00)
    else Some ((sign lsl 15) lor 0x7E00)
  else if exp = 0 && mant = 0L then Some (sign lsl 15)
  else
    let unbiased = exp - 1023 in
    if unbiased < -24 then None
    else if unbiased < -14 then
      let shift = -14 - unbiased in
      let full_mant = Int64.logor mant 0x10000000000000L in
      let half_mant =
        Int64.to_int (Int64.shift_right_logical full_mant (42 + shift))
      in
      let mask = Int64.sub (Int64.shift_left 1L (42 + shift)) 1L in
      if Int64.logand full_mant mask <> 0L then None
      else Some ((sign lsl 15) lor half_mant)
    else if unbiased <= 15 then
      let half_exp = unbiased + 15 in
      let half_mant = Int64.to_int (Int64.shift_right_logical mant 42) in
      if Int64.logand mant 0x3FFFFFFFFFFL <> 0L then None
      else Some ((sign lsl 15) lor (half_exp lsl 10) lor half_mant)
    else None

(* ---------- Unsigned int64 comparison ---------- *)

let uint64_lt a b =
  Int64.compare (Int64.add a Int64.min_int) (Int64.add b Int64.min_int) < 0

(* ---------- Encoding ---------- *)

let write_byte buf b = Buffer.add_uint8 buf b

let write_be16 buf v =
  write_byte buf ((v lsr 8) land 0xFF);
  write_byte buf (v land 0xFF)

let write_be32 buf v =
  write_byte buf (Int64.to_int (Int64.shift_right_logical v 24) land 0xFF);
  write_byte buf (Int64.to_int (Int64.shift_right_logical v 16) land 0xFF);
  write_byte buf (Int64.to_int (Int64.shift_right_logical v 8) land 0xFF);
  write_byte buf (Int64.to_int v land 0xFF)

let write_be64 buf v =
  for i = 7 downto 0 do
    write_byte buf
      (Int64.to_int (Int64.shift_right_logical v (i * 8)) land 0xFF)
  done

let write_be32_i32 buf v =
  write_byte buf (Int32.to_int (Int32.shift_right_logical v 24) land 0xFF);
  write_byte buf (Int32.to_int (Int32.shift_right_logical v 16) land 0xFF);
  write_byte buf (Int32.to_int (Int32.shift_right_logical v 8) land 0xFF);
  write_byte buf (Int32.to_int v land 0xFF)

let write_argument buf major arg =
  let initial = major lsl 5 in
  if uint64_lt arg 24L then
    write_byte buf (initial lor Int64.to_int arg)
  else if uint64_lt arg 0x100L then begin
    write_byte buf (initial lor 24);
    write_byte buf (Int64.to_int arg)
  end else if uint64_lt arg 0x10000L then begin
    write_byte buf (initial lor 25);
    write_be16 buf (Int64.to_int arg)
  end else if uint64_lt arg 0x100000000L then begin
    write_byte buf (initial lor 26);
    write_be32 buf arg
  end else begin
    write_byte buf (initial lor 27);
    write_be64 buf arg
  end

let encode_float64 buf f =
  write_byte buf 0xFB;
  write_be64 buf (Int64.bits_of_float f)

let encode_float_canonical buf f =
  match float_to_half f with
  | Some h when half_to_float h = f || (Float.is_nan f && Float.is_nan (half_to_float h)) ->
    write_byte buf 0xF9;
    write_be16 buf h
  | _ ->
    let bits32 = Int32.bits_of_float f in
    let back = Int32.float_of_bits bits32 in
    if back = f || (Float.is_nan f && Float.is_nan back) then begin
      write_byte buf 0xFA;
      write_be32_i32 buf bits32
    end else
      encode_float64 buf f

let rec do_encode ~canonical buf v =
  let v =
    if canonical then
      match v with
      | IndefiniteBytes chunks -> Bytes (Bytes.concat Bytes.empty chunks)
      | IndefiniteText chunks -> Text (String.concat "" chunks)
      | IndefiniteArray items -> Array items
      | IndefiniteMap pairs -> Map pairs
      | _ -> v
    else v
  in
  match v with
  | Uint n -> write_argument buf 0 n
  | Nint n ->
    let arg = Int64.sub (Int64.neg n) 1L in
    write_argument buf 1 arg
  | Bytes b ->
    write_argument buf 2 (Int64.of_int (Bytes.length b));
    Buffer.add_bytes buf b
  | Text s ->
    write_argument buf 3 (Int64.of_int (String.length s));
    Buffer.add_string buf s
  | Array items ->
    write_argument buf 4 (Int64.of_int (List.length items));
    List.iter (do_encode ~canonical buf) items
  | Map pairs ->
    let pairs =
      if canonical then
        let key_bytes k =
          let kb = Buffer.create 16 in
          do_encode ~canonical:true kb k;
          Buffer.to_bytes kb
        in
        List.sort
          (fun (k1, _) (k2, _) ->
            Bytes.compare (key_bytes k1) (key_bytes k2))
          pairs
      else pairs
    in
    write_argument buf 5 (Int64.of_int (List.length pairs));
    List.iter
      (fun (k, v) ->
        do_encode ~canonical buf k;
        do_encode ~canonical buf v)
      pairs
  | Tag (tag, inner) ->
    write_argument buf 6 tag;
    do_encode ~canonical buf inner
  | Bool false -> write_byte buf 0xF4
  | Bool true -> write_byte buf 0xF5
  | Null -> write_byte buf 0xF6
  | Undefined -> write_byte buf 0xF7
  | Float f ->
    if canonical then encode_float_canonical buf f else encode_float64 buf f
  | Simple n ->
    if n <= 23 then write_byte buf (0xE0 lor n)
    else begin
      write_byte buf 0xF8;
      write_byte buf n
    end
  | IndefiniteBytes chunks ->
    write_byte buf 0x5F;
    List.iter
      (fun b ->
        write_argument buf 2 (Int64.of_int (Bytes.length b));
        Buffer.add_bytes buf b)
      chunks;
    write_byte buf 0xFF
  | IndefiniteText chunks ->
    write_byte buf 0x7F;
    List.iter
      (fun s ->
        write_argument buf 3 (Int64.of_int (String.length s));
        Buffer.add_string buf s)
      chunks;
    write_byte buf 0xFF
  | IndefiniteArray items ->
    write_byte buf 0x9F;
    List.iter (do_encode ~canonical buf) items;
    write_byte buf 0xFF
  | IndefiniteMap pairs ->
    write_byte buf 0xBF;
    List.iter
      (fun (k, v) ->
        do_encode ~canonical buf k;
        do_encode ~canonical buf v)
      pairs;
    write_byte buf 0xFF

let encode v =
  let buf = Buffer.create 64 in
  do_encode ~canonical:false buf v;
  Buffer.to_bytes buf

let encode_canonical v =
  let buf = Buffer.create 64 in
  do_encode ~canonical:true buf v;
  Buffer.to_bytes buf

(* ---------- Decoding ---------- *)

let read_byte data pos =
  if pos >= Bytes.length data then Error "unexpected end of input"
  else Ok (Bytes.get_uint8 data pos, pos + 1)

let read_bytes data pos n =
  if pos + n > Bytes.length data then Error "unexpected end of input"
  else Ok (Bytes.sub data pos n, pos + n)

let read_be16 data pos =
  if pos + 2 > Bytes.length data then Error "unexpected end of input"
  else
    let b0 = Bytes.get_uint8 data pos in
    let b1 = Bytes.get_uint8 data (pos + 1) in
    Ok ((b0 lsl 8) lor b1, pos + 2)

let read_be32 data pos =
  if pos + 4 > Bytes.length data then Error "unexpected end of input"
  else
    let v = ref 0L in
    for i = 0 to 3 do
      v :=
        Int64.logor (Int64.shift_left !v 8)
          (Int64.of_int (Bytes.get_uint8 data (pos + i)))
    done;
    Ok (!v, pos + 4)

let read_be64 data pos =
  if pos + 8 > Bytes.length data then Error "unexpected end of input"
  else
    let v = ref 0L in
    for i = 0 to 7 do
      v :=
        Int64.logor (Int64.shift_left !v 8)
          (Int64.of_int (Bytes.get_uint8 data (pos + i)))
    done;
    Ok (!v, pos + 8)

let read_argument data pos ai =
  if ai <= 23 then Ok (Int64.of_int ai, pos)
  else if ai = 24 then
    let* b, pos = read_byte data pos in
    Ok (Int64.of_int b, pos)
  else if ai = 25 then
    let* v, pos = read_be16 data pos in
    Ok (Int64.of_int v, pos)
  else if ai = 26 then read_be32 data pos
  else if ai = 27 then read_be64 data pos
  else Error (Printf.sprintf "invalid additional info: %d" ai)

let rec decode_value data pos =
  let* initial, pos = read_byte data pos in
  let major = initial lsr 5 in
  let ai = initial land 0x1F in
  match major with
  | 0 ->
    let* arg, pos = read_argument data pos ai in
    Ok (Uint arg, pos)
  | 1 ->
    let* arg, pos = read_argument data pos ai in
    Ok (Nint (Int64.neg (Int64.add arg 1L)), pos)
  | 2 ->
    if ai = 31 then decode_indefinite_bytes data pos []
    else
      let* len, pos = read_argument data pos ai in
      let* b, pos = read_bytes data pos (Int64.to_int len) in
      Ok (Bytes b, pos)
  | 3 ->
    if ai = 31 then decode_indefinite_text data pos []
    else
      let* len, pos = read_argument data pos ai in
      let* b, pos = read_bytes data pos (Int64.to_int len) in
      Ok (Text (Bytes.to_string b), pos)
  | 4 ->
    if ai = 31 then decode_indefinite_array data pos []
    else
      let* len, pos = read_argument data pos ai in
      decode_n_items data pos (Int64.to_int len) []
  | 5 ->
    if ai = 31 then decode_indefinite_map data pos []
    else
      let* len, pos = read_argument data pos ai in
      decode_n_pairs data pos (Int64.to_int len) []
  | 6 ->
    let* tag, pos = read_argument data pos ai in
    let* v, pos = decode_value data pos in
    Ok (Tag (tag, v), pos)
  | 7 -> decode_major7 data pos ai
  | _ -> Error "invalid major type"

and decode_n_items data pos n acc =
  if n = 0 then Ok (Array (List.rev acc), pos)
  else
    let* v, pos = decode_value data pos in
    decode_n_items data pos (n - 1) (v :: acc)

and decode_n_pairs data pos n acc =
  if n = 0 then Ok (Map (List.rev acc), pos)
  else
    let* k, pos = decode_value data pos in
    let* v, pos = decode_value data pos in
    decode_n_pairs data pos (n - 1) ((k, v) :: acc)

and decode_indefinite_bytes data pos acc =
  let* b, pos' = read_byte data pos in
  if b = 0xFF then Ok (IndefiniteBytes (List.rev acc), pos')
  else
    let major = b lsr 5 in
    let ai = b land 0x1F in
    if major <> 2 then
      Error "non-byte-string chunk in indefinite-length byte string"
    else if ai = 31 then Error "nested indefinite-length byte string"
    else
      let* len, pos = read_argument data pos' ai in
      let* chunk, pos = read_bytes data pos (Int64.to_int len) in
      decode_indefinite_bytes data pos (chunk :: acc)

and decode_indefinite_text data pos acc =
  let* b, pos' = read_byte data pos in
  if b = 0xFF then Ok (IndefiniteText (List.rev acc), pos')
  else
    let major = b lsr 5 in
    let ai = b land 0x1F in
    if major <> 3 then
      Error "non-text-string chunk in indefinite-length text string"
    else if ai = 31 then Error "nested indefinite-length text string"
    else
      let* len, pos = read_argument data pos' ai in
      let* chunk, pos = read_bytes data pos (Int64.to_int len) in
      decode_indefinite_text data pos (Bytes.to_string chunk :: acc)

and decode_indefinite_array data pos acc =
  let* b, _ = read_byte data pos in
  if b = 0xFF then Ok (IndefiniteArray (List.rev acc), pos + 1)
  else
    let* v, pos = decode_value data pos in
    decode_indefinite_array data pos (v :: acc)

and decode_indefinite_map data pos acc =
  let* b, _ = read_byte data pos in
  if b = 0xFF then Ok (IndefiniteMap (List.rev acc), pos + 1)
  else
    let* k, pos = decode_value data pos in
    let* v, pos = decode_value data pos in
    decode_indefinite_map data pos ((k, v) :: acc)

and decode_major7 data pos ai =
  if ai <= 19 then Ok (Simple ai, pos)
  else if ai = 20 then Ok (Bool false, pos)
  else if ai = 21 then Ok (Bool true, pos)
  else if ai = 22 then Ok (Null, pos)
  else if ai = 23 then Ok (Undefined, pos)
  else if ai = 24 then
    let* v, pos = read_byte data pos in
    if v < 32 then Error "simple value 0-31 in two-byte form is not well-formed"
    else Ok (Simple v, pos)
  else if ai = 25 then
    let* v, pos = read_be16 data pos in
    Ok (Float (half_to_float v), pos)
  else if ai = 26 then
    let* b, pos = read_bytes data pos 4 in
    let bits = ref 0l in
    for i = 0 to 3 do
      bits :=
        Int32.logor (Int32.shift_left !bits 8)
          (Int32.of_int (Bytes.get_uint8 b i))
    done;
    Ok (Float (Int32.float_of_bits !bits), pos)
  else if ai = 27 then
    let* b, pos = read_bytes data pos 8 in
    let bits = ref 0L in
    for i = 0 to 7 do
      bits :=
        Int64.logor (Int64.shift_left !bits 8)
          (Int64.of_int (Bytes.get_uint8 b i))
    done;
    Ok (Float (Int64.float_of_bits !bits), pos)
  else if ai = 31 then Error "unexpected break code outside indefinite-length item"
  else Error (Printf.sprintf "reserved additional info %d in major type 7" ai)

let decode data =
  let* v, pos = decode_value data 0 in
  if pos = Bytes.length data then Ok v
  else Error (Printf.sprintf "trailing data at position %d" pos)

(* ---------- Helper constructors ---------- *)

let encode_uint n = Uint n
let encode_bytes b = Bytes b
let encode_text s = Text s
let encode_array items = Array items
let encode_map pairs = Map pairs
let encode_tag tag v = Tag (tag, v)
