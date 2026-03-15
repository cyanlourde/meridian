(* On-disk block store for Meridian.

   Append-only, content-addressed block storage with crash-safe writes.

   Layout:
     <base_dir>/
       blocks/<xx>/<hash>.block   — raw CBOR block bytes (xx = first hex byte)
       chain.index                — append-only: 8-byte slot + 32-byte hash per entry
       store.meta                 — JSON-ish metadata (tip, count, timestamp)

   All writes use temp-file + atomic rename to avoid partial data on crash. *)

(* ================================================================ *)
(* Hex helpers                                                       *)
(* ================================================================ *)

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type block_entry = {
  slot : int64;
  hash : bytes;  (** 32-byte block hash *)
}

type store = {
  base_dir : string;
  blocks_dir : string;
  index_path : string;
  meta_path : string;
  mutable index : block_entry array;
  (** Sorted by slot (ascending), then hash for ties *)
  mutable block_count : int;
  (** Cache of most recent k blocks for fast access *)
  recent_cache : (string, bytes) Hashtbl.t;  (** hash_hex -> cbor_bytes *)
  mutable cache_order : string list;  (** Most recent first *)
  cache_max : int;  (** k = 2160 *)
}

(* ================================================================ *)
(* Filesystem helpers                                                *)
(* ================================================================ *)

let ensure_dir path =
  if not (Sys.file_exists path) then
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let ensure_dir_p path =
  let rec go dir =
    if dir = "" || dir = "/" || dir = "." then ()
    else if Sys.file_exists dir then ()
    else begin
      go (Filename.dirname dir);
      (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    end
  in
  go path

(** Write bytes atomically: write to .tmp, then rename. *)
let atomic_write path data =
  let tmp = path ^ ".tmp" in
  let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let len = Bytes.length data in
  let rec go off remaining =
    if remaining = 0 then ()
    else
      let n = Unix.write fd data off remaining in
      go (off + n) (remaining - n)
  in
  (try go 0 len with e -> Unix.close fd; raise e);
  Unix.close fd;
  Unix.rename tmp path

(** Append bytes to a file atomically by copying + rename.
    For the index file, we write the full updated content. *)
let append_to_file path data =
  let existing =
    if Sys.file_exists path then begin
      let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
      let st = Unix.fstat fd in
      let buf = Bytes.create st.Unix.st_size in
      let rec go off remaining =
        if remaining = 0 then ()
        else let n = Unix.read fd buf off remaining in go (off + n) (remaining - n)
      in
      go 0 st.st_size;
      Unix.close fd;
      buf
    end else Bytes.empty
  in
  let combined = Bytes.create (Bytes.length existing + Bytes.length data) in
  Bytes.blit existing 0 combined 0 (Bytes.length existing);
  Bytes.blit data 0 combined (Bytes.length existing) (Bytes.length data);
  atomic_write path combined

(* ================================================================ *)
(* Block path                                                        *)
(* ================================================================ *)

let block_path store hash =
  let hex = hex_of_bytes hash in
  let prefix = String.sub hex 0 2 in
  let dir = Filename.concat store.blocks_dir prefix in
  (dir, Filename.concat dir (hex ^ ".block"))

(* ================================================================ *)
(* Index I/O                                                         *)
(* ================================================================ *)

(* Each index entry: 8 bytes slot (big-endian) + 32 bytes hash = 40 bytes *)
let entry_size = 40

let encode_entry slot hash =
  let buf = Bytes.create entry_size in
  for i = 0 to 7 do
    Bytes.set_uint8 buf i
      (Int64.to_int (Int64.shift_right_logical slot ((7 - i) * 8)) land 0xFF)
  done;
  Bytes.blit hash 0 buf 8 32;
  buf

let decode_entry buf off =
  let slot = ref 0L in
  for i = 0 to 7 do
    slot := Int64.logor (Int64.shift_left !slot 8)
              (Int64.of_int (Bytes.get_uint8 buf (off + i)))
  done;
  let hash = Bytes.sub buf (off + 8) 32 in
  { slot = !slot; hash }

let load_index path =
  if not (Sys.file_exists path) then [||]
  else begin
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    let st = Unix.fstat fd in
    let buf = Bytes.create st.Unix.st_size in
    let rec go off remaining =
      if remaining = 0 then ()
      else let n = Unix.read fd buf off remaining in go (off + n) (remaining - n)
    in
    go 0 st.st_size;
    Unix.close fd;
    let count = st.st_size / entry_size in
    Array.init count (fun i -> decode_entry buf (i * entry_size))
  end

(* ================================================================ *)
(* Metadata I/O                                                      *)
(* ================================================================ *)

let write_meta store =
  let tip_slot, tip_hash =
    if store.block_count = 0 then (0L, Bytes.make 32 '\x00')
    else
      let e = store.index.(store.block_count - 1) in
      (e.slot, e.hash)
  in
  let content = Printf.sprintf
    "tip_slot=%Ld\ntip_hash=%s\nblock_count=%d\nupdated=%f\n"
    tip_slot (hex_of_bytes tip_hash) store.block_count (Unix.gettimeofday ()) in
  atomic_write store.meta_path (Bytes.of_string content)

(* ================================================================ *)
(* Cache management                                                  *)
(* ================================================================ *)

let cache_add store hash_hex cbor_bytes =
  if not (Hashtbl.mem store.recent_cache hash_hex) then begin
    Hashtbl.replace store.recent_cache hash_hex cbor_bytes;
    store.cache_order <- hash_hex :: store.cache_order;
    (* Evict oldest if over capacity *)
    if List.length store.cache_order > store.cache_max then begin
      let rev = List.rev store.cache_order in
      let evict = List.hd rev in
      Hashtbl.remove store.recent_cache evict;
      store.cache_order <- List.rev (List.tl rev)
    end
  end

(* ================================================================ *)
(* Public API                                                        *)
(* ================================================================ *)

(** Initialize the store. Creates directories if needed, loads existing index. *)
let init ?(cache_max = 2160) ~base_dir () =
  ensure_dir_p base_dir;
  let blocks_dir = Filename.concat base_dir "blocks" in
  ensure_dir blocks_dir;
  let index_path = Filename.concat base_dir "chain.index" in
  let meta_path = Filename.concat base_dir "store.meta" in
  let index = load_index index_path in
  let s = {
    base_dir; blocks_dir; index_path; meta_path;
    index; block_count = Array.length index;
    recent_cache = Hashtbl.create (min cache_max 256);
    cache_order = [];
    cache_max;
  } in
  s

(** Store a block. Writes the block file, appends to index, updates metadata.
    Returns Ok () on success. Idempotent: storing the same block twice is a no-op. *)
let store_block store ~slot ~hash ~cbor_bytes =
  let (dir, path) = block_path store hash in
  if Sys.file_exists path then Ok ()  (* already stored *)
  else begin
    ensure_dir dir;
    atomic_write path cbor_bytes;
    (* Append to index *)
    let entry_bytes = encode_entry slot hash in
    append_to_file store.index_path entry_bytes;
    (* Update in-memory index *)
    let entry = { slot; hash } in
    let new_index = Array.make (store.block_count + 1) entry in
    Array.blit store.index 0 new_index 0 store.block_count;
    new_index.(store.block_count) <- entry;
    store.index <- new_index;
    store.block_count <- store.block_count + 1;
    (* Update cache *)
    cache_add store (hex_of_bytes hash) cbor_bytes;
    (* Update metadata *)
    write_meta store;
    Ok ()
  end

(** Retrieve a block by its hash. Checks cache first, then disk. *)
let get_block store ~hash =
  let hex = hex_of_bytes hash in
  match Hashtbl.find_opt store.recent_cache hex with
  | Some data -> Some data
  | None ->
    let (_, path) = block_path store hash in
    if not (Sys.file_exists path) then None
    else begin
      let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
      let st = Unix.fstat fd in
      let buf = Bytes.create st.Unix.st_size in
      let rec go off remaining =
        if remaining = 0 then ()
        else let n = Unix.read fd buf off remaining in go (off + n) (remaining - n)
      in
      go 0 st.st_size;
      Unix.close fd;
      cache_add store hex buf;
      Some buf
    end

(** Retrieve a block by slot number. Looks up hash in index, then fetches block. *)
let get_block_by_slot store ~slot =
  let found = ref None in
  for i = store.block_count - 1 downto 0 do
    if !found = None && Int64.equal store.index.(i).slot slot then
      found := Some store.index.(i).hash
  done;
  match !found with
  | None -> None
  | Some hash -> get_block store ~hash

(** Check if a block exists in the store. *)
let has_block store ~hash =
  let (_, path) = block_path store hash in
  Sys.file_exists path

(** Get the current tip (highest slot in the index). *)
let tip store =
  if store.block_count = 0 then None
  else
    let e = store.index.(store.block_count - 1) in
    Some (e.slot, e.hash)

(** Total number of blocks stored. *)
let block_count store = store.block_count

(** Get the last N block hashes from tip. *)
let get_recent_blocks store ~count =
  let n = min count store.block_count in
  let start = store.block_count - n in
  Array.to_list (Array.sub store.index start n)
  |> List.map (fun e -> (e.slot, e.hash))

(** Get chain points suitable for MsgFindIntersect.
    Returns tip, tip-10, tip-100, tip-1000, tip-2160, origin. *)
let get_chain_points store =
  if store.block_count = 0 then [Chain_sync.Origin]
  else
    let offsets = [0; 10; 100; 1000; 2160] in
    let points = List.filter_map (fun off ->
      let idx = store.block_count - 1 - off in
      if idx >= 0 then
        let e = store.index.(idx) in
        Some (Chain_sync.Point (e.slot, e.hash))
      else None
    ) offsets in
    points @ [Chain_sync.Origin]
