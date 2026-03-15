(* Mithril snapshot unpacking and import into Meridian store.

   Mithril snapshots contain the Cardano immutable DB — chunk files
   with concatenated CBOR-encoded blocks. We extract them, parse each
   block, and store into our content-addressed block store. *)

(* ================================================================ *)
(* Snapshot unpacking                                                *)
(* ================================================================ *)

(** Unpack a snapshot archive (tar.gz, tar.zst, or tar).
    Detects format from extension. *)
let unpack_snapshot ~archive_path ~dest_dir =
  if not (Sys.file_exists archive_path) then
    Error "archive not found"
  else begin
    (try Unix.mkdir dest_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let cmd =
      if Filename.check_suffix archive_path ".tar.zst" ||
         Filename.check_suffix archive_path ".zst" then
        Printf.sprintf "tar -xf %s -C %s 2>&1"
          (Filename.quote archive_path) (Filename.quote dest_dir)
      else if Filename.check_suffix archive_path ".tar.gz" ||
              Filename.check_suffix archive_path ".tgz" then
        Printf.sprintf "tar -xzf %s -C %s 2>&1"
          (Filename.quote archive_path) (Filename.quote dest_dir)
      else
        Printf.sprintf "tar -xf %s -C %s 2>&1"
          (Filename.quote archive_path) (Filename.quote dest_dir)
    in
    let ret = Sys.command cmd in
    if ret = 0 then Ok ()
    else Error (Printf.sprintf "unpack failed (exit %d)" ret)
  end

(* ================================================================ *)
(* Immutable DB chunk parsing                                        *)
(* ================================================================ *)

(** Read a chunk file and split into individual CBOR-encoded blocks.
    Each block is a complete CBOR value. We use incremental parsing:
    try decoding from the current offset, advance by the decoded length. *)
let parse_chunk_file ~path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let st = Unix.fstat fd in
  let data = Bytes.create st.Unix.st_size in
  let rec go off rem =
    if rem = 0 then () else let n = Unix.read fd data off rem in go (off+n) (rem-n)
  in
  go 0 st.st_size;
  Unix.close fd;
  (* Split concatenated CBOR values *)
  let blocks = ref [] in
  let off = ref 0 in
  let len = Bytes.length data in
  while !off < len do
    (* Try increasing sizes until CBOR decodes successfully *)
    let found = ref false in
    let try_size = ref 1 in
    while not !found && !off + !try_size <= len do
      let sub = Bytes.sub data !off !try_size in
      match Cbor.decode sub with
      | Ok _ ->
        blocks := sub :: !blocks;
        off := !off + !try_size;
        found := true
      | Error _ ->
        incr try_size
    done;
    if not !found then
      off := len  (* skip remaining *)
  done;
  List.rev !blocks

(* ================================================================ *)
(* Import blocks into store                                          *)
(* ================================================================ *)

type import_stats = {
  mutable chunks_processed : int;
  mutable blocks_imported : int;
  mutable bytes_total : int;
  mutable errors : int;
}

(** Import blocks from parsed CBOR bytes into the store.
    Returns (slot, hash) for each successfully imported block. *)
let import_block_bytes store cbor_bytes =
  match Block_decoder.decode_block cbor_bytes with
  | Error _ -> None
  | Ok block ->
    let slot = block.db_header.bh_slot in
    let hash = Crypto.blake2b_256 cbor_bytes in
    (match Store.store_block store ~slot ~hash ~cbor_bytes with
     | Ok () -> Some (slot, hash)
     | Error _ -> None)

(** Import all chunk files from an immutable DB directory.
    Chunk files are named like 00000.chunk, 00001.chunk, etc. *)
let import_immutable_db ~immutable_dir ~store ~on_progress =
  let stats = { chunks_processed = 0; blocks_imported = 0;
                bytes_total = 0; errors = 0 } in
  if not (Sys.file_exists immutable_dir) then
    Error "immutable directory not found"
  else begin
    let files = Sys.readdir immutable_dir in
    let chunk_files = Array.to_list files
      |> List.filter (fun f -> Filename.check_suffix f ".chunk")
      |> List.sort String.compare in
    List.iter (fun chunk_name ->
      let path = Filename.concat immutable_dir chunk_name in
      let chunk_blocks = parse_chunk_file ~path in
      stats.chunks_processed <- stats.chunks_processed + 1;
      List.iter (fun cbor_bytes ->
        stats.bytes_total <- stats.bytes_total + Bytes.length cbor_bytes;
        match import_block_bytes store cbor_bytes with
        | Some _ -> stats.blocks_imported <- stats.blocks_imported + 1
        | None -> stats.errors <- stats.errors + 1
      ) chunk_blocks;
      on_progress stats
    ) chunk_files;
    Ok stats
  end

(** Rebuild ledger state by replaying stored blocks. *)
let rebuild_ledger ~store ~ledger ~from_slot ~to_slot ~on_progress =
  let recent = Store.get_recent_blocks store ~count:(Store.block_count store) in
  let blocks_in_range = List.filter (fun (slot, _) ->
    Int64.compare slot from_slot >= 0 && Int64.compare slot to_slot <= 0
  ) recent in
  let count = ref 0 in
  List.iter (fun (slot, _hash) ->
    match Store.get_block_by_slot store ~slot with
    | None -> ()
    | Some cbor_bytes ->
      match Block_decoder.decode_block cbor_bytes with
      | Error _ -> ()
      | Ok block ->
        ignore (Ledger_state.apply_block ledger block);
        incr count;
        if !count mod 1000 = 0 then on_progress !count
  ) blocks_in_range;
  !count
