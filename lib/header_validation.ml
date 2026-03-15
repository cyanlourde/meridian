(* Block header validation per Cardano formal ledger spec.

   Individual checks that take a decoded block header and return
   Ok () or Error with a descriptive message. *)

type validation_error = string

let validate_slot_number ~prev_slot ~slot =
  if Int64.compare slot prev_slot > 0 then Ok ()
  else Error (Printf.sprintf "slot %Ld not greater than prev slot %Ld" slot prev_slot)

let validate_prev_hash ~expected_hash ~header =
  match header.Block_decoder.bh_prev_hash with
  | None ->
    if Bytes.length expected_hash = 0 then Ok ()
    else Error "header has no prev_hash but expected one"
  | Some prev ->
    if Bytes.equal prev expected_hash then Ok ()
    else Error (Printf.sprintf "prev_hash mismatch")

let validate_block_number ~prev_block_number ~header =
  match header.Block_decoder.bh_era with
  | Byron -> Ok ()  (* Byron uses different numbering *)
  | _ ->
    let expected = Int64.add prev_block_number 1L in
    if Int64.equal header.bh_block_number expected then Ok ()
    else Error (Printf.sprintf "block_number %Ld expected %Ld"
                  header.bh_block_number expected)

let validate_protocol_version ~header =
  let (major, _minor) = header.Block_decoder.bh_protocol_version in
  match header.bh_era with
  | Byron -> Ok ()
  | Shelley -> if major >= 2L && major <= 3L then Ok ()
    else Error (Printf.sprintf "shelley proto version %Ld out of range" major)
  | Allegra -> if major >= 3L && major <= 4L then Ok ()
    else Error (Printf.sprintf "allegra proto version %Ld out of range" major)
  | Mary -> if major >= 4L && major <= 5L then Ok ()
    else Error (Printf.sprintf "mary proto version %Ld out of range" major)
  | Alonzo -> if major >= 5L && major <= 7L then Ok ()
    else Error (Printf.sprintf "alonzo proto version %Ld out of range" major)
  | Babbage -> if major >= 7L && major <= 9L then Ok ()
    else Error (Printf.sprintf "babbage proto version %Ld out of range" major)
  | Conway -> if major >= 9L && major <= 15L then Ok ()
    else Error (Printf.sprintf "conway proto version %Ld out of range" major)

let validate_block_body_hash ~header ~raw_body_cbor =
  let computed = Crypto.blake2b_256 raw_body_cbor in
  if Bytes.length header.Block_decoder.bh_body_hash = 0 then Ok ()
  else if Bytes.equal header.bh_body_hash computed then Ok ()
  else Error "block body hash mismatch"

let validate_header_size ~raw_header_cbor ~max_size =
  let sz = Bytes.length raw_header_cbor in
  if sz <= max_size then Ok ()
  else Error (Printf.sprintf "header size %d exceeds max %d" sz max_size)

let max_block_size_for_era = function
  | Block_decoder.Byron -> 2000000
  | Shelley | Allegra | Mary -> 65536
  | Alonzo -> 73728
  | Babbage | Conway -> 90112

let validate_block_size ~raw_block_cbor ~era =
  let sz = Bytes.length raw_block_cbor in
  let max_sz = max_block_size_for_era era in
  if sz <= max_sz then Ok ()
  else Error (Printf.sprintf "block size %d exceeds max %d for %s"
                sz max_sz (Block_decoder.era_name era))

(** Run all applicable header checks. Returns list of all failures. *)
let validate_header ~prev_slot ~prev_block_number ~prev_hash
    ~header ~raw_header_cbor ~raw_body_cbor =
  let checks = [
    validate_slot_number ~prev_slot ~slot:header.Block_decoder.bh_slot;
    validate_prev_hash ~expected_hash:prev_hash ~header;
    validate_block_number ~prev_block_number ~header;
    validate_protocol_version ~header;
    validate_block_body_hash ~header ~raw_body_cbor;
    validate_header_size ~raw_header_cbor ~max_size:1100;
  ] in
  let errors = List.filter_map (function Error e -> Some e | Ok () -> None) checks in
  if errors = [] then Ok () else Error errors
