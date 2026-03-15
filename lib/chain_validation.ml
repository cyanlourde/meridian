(* Chain integrity validation over stored block sequences.

   Iterates through stored blocks and validates each header
   against its predecessor. *)

type chain_error = {
  ce_slot : int64;
  ce_errors : string list;
}

(** Validate that a genesis/first block has acceptable prev_hash. *)
let validate_genesis_block header =
  match header.Block_decoder.bh_prev_hash with
  | None -> Ok ()
  | Some h ->
    let all_zero = Bytes.for_all (fun c -> c = '\x00') h in
    if all_zero then Ok ()
    else Error "genesis block prev_hash is not all zeros"

(** Validate a range of stored blocks.
    Returns list of (slot, errors) for any failures found. *)
let validate_chain store ~from_slot ~to_slot =
  let errors = ref [] in
  let recent = Store.get_recent_blocks store ~count:(Store.block_count store) in
  let blocks_in_range = List.filter (fun (slot, _hash) ->
    Int64.compare slot from_slot >= 0 && Int64.compare slot to_slot <= 0
  ) recent in
  let prev_slot = ref 0L in
  let prev_block_number = ref 0L in
  let prev_hash = ref Bytes.empty in
  let first = ref true in
  List.iter (fun (slot, _hash) ->
    match Store.get_block_by_slot store ~slot with
    | None -> ()
    | Some cbor_bytes ->
      match Block_decoder.decode_block cbor_bytes with
      | Error e ->
        errors := { ce_slot = slot; ce_errors = [e] } :: !errors
      | Ok block ->
        let header = block.db_header in
        if !first then begin
          first := false;
          (match validate_genesis_block header with
           | Error e -> errors := { ce_slot = slot; ce_errors = [e] } :: !errors
           | Ok () -> ());
          prev_slot := header.bh_slot;
          prev_block_number := header.bh_block_number;
          prev_hash := Crypto.blake2b_256 cbor_bytes
        end else begin
          let result = Header_validation.validate_header
            ~prev_slot:!prev_slot
            ~prev_block_number:!prev_block_number
            ~prev_hash:!prev_hash
            ~header
            ~raw_header_cbor:cbor_bytes
            ~raw_body_cbor:cbor_bytes in
          (match result with
           | Error errs ->
             errors := { ce_slot = slot; ce_errors = errs } :: !errors
           | Ok () -> ());
          prev_slot := header.bh_slot;
          prev_block_number := header.bh_block_number;
          prev_hash := Crypto.blake2b_256 cbor_bytes
        end
  ) blocks_in_range;
  List.rev !errors

(** Validate the most recent N blocks from tip. Quick integrity check. *)
let validate_chain_tip store ~count =
  let recent = Store.get_recent_blocks store ~count in
  match recent with
  | [] -> []
  | (first_slot, _) :: _ ->
    let last_slot = fst (List.nth recent (List.length recent - 1)) in
    validate_chain store ~from_slot:first_slot ~to_slot:last_slot

(** Find gaps where consecutive stored blocks have mismatched prev_hash.
    Returns list of (slot, "gap") for any breaks found. *)
let find_chain_breaks store ~from_slot ~to_slot =
  let breaks = ref [] in
  let recent = Store.get_recent_blocks store ~count:(Store.block_count store) in
  let blocks_in_range = List.filter (fun (slot, _) ->
    Int64.compare slot from_slot >= 0 && Int64.compare slot to_slot <= 0
  ) recent in
  let prev_hash = ref Bytes.empty in
  let first = ref true in
  List.iter (fun (slot, _hash) ->
    match Store.get_block_by_slot store ~slot with
    | None -> ()
    | Some cbor_bytes ->
      if !first then begin
        first := false;
        prev_hash := Crypto.blake2b_256 cbor_bytes
      end else begin
        match Block_decoder.decode_block cbor_bytes with
        | Error _ -> ()
        | Ok block ->
          (match block.db_header.bh_prev_hash with
           | Some ph when not (Bytes.equal ph !prev_hash) ->
             breaks := { ce_slot = slot; ce_errors = ["prev_hash break"] } :: !breaks
           | _ -> ());
          prev_hash := Crypto.blake2b_256 cbor_bytes
      end
  ) blocks_in_range;
  List.rev !breaks
