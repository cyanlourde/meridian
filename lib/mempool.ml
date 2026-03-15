(* Transaction mempool.

   Holds pending transactions validated against the current UTXO set.
   Transactions are removed when included in a block, evicted when
   their inputs are consumed, or expired after max_age_slots. *)

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type mempool_entry = {
  raw_cbor : bytes;
  decoded : Tx_decoder.decoded_tx;
  added_at : float;
  size_bytes : int;
  fee : int64;
}

type t = {
  mutable entries : (string, mempool_entry) Hashtbl.t;
  (** tx_hash_hex -> entry *)
  mutable total_size : int;
  max_size_bytes : int;
  max_tx_count : int;
}

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

(* ================================================================ *)
(* Public API                                                        *)
(* ================================================================ *)

let create ?(max_size_bytes = 16_777_216) ?(max_tx_count = 4096) () =
  { entries = Hashtbl.create 256; total_size = 0;
    max_size_bytes; max_tx_count }

let size t =
  (Hashtbl.length t.entries, t.total_size)

let has_tx t ~tx_hash =
  Hashtbl.mem t.entries (hex_of_bytes tx_hash)

let get_tx t ~tx_hash =
  Hashtbl.find_opt t.entries (hex_of_bytes tx_hash)

let add_tx t ~tx_hash ~raw_cbor ~decoded ~fee =
  let hex = hex_of_bytes tx_hash in
  if Hashtbl.mem t.entries hex then
    Error "duplicate transaction"
  else if Hashtbl.length t.entries >= t.max_tx_count then
    Error "mempool full (tx count)"
  else
    let sz = Bytes.length raw_cbor in
    if t.total_size + sz > t.max_size_bytes then
      Error "mempool full (size)"
    else begin
      let entry = { raw_cbor; decoded; added_at = Unix.gettimeofday ();
                    size_bytes = sz; fee } in
      Hashtbl.replace t.entries hex entry;
      t.total_size <- t.total_size + sz;
      Ok ()
    end

let remove_tx t ~tx_hash =
  let hex = hex_of_bytes tx_hash in
  match Hashtbl.find_opt t.entries hex with
  | None -> ()
  | Some entry ->
    Hashtbl.remove t.entries hex;
    t.total_size <- t.total_size - entry.size_bytes

(** Remove all transactions that appear in a block's tx list. *)
let remove_confirmed t ~block_tx_hashes =
  let removed = ref 0 in
  List.iter (fun tx_hash ->
    let hex = hex_of_bytes tx_hash in
    match Hashtbl.find_opt t.entries hex with
    | None -> ()
    | Some entry ->
      Hashtbl.remove t.entries hex;
      t.total_size <- t.total_size - entry.size_bytes;
      incr removed
  ) block_tx_hashes;
  !removed

(** Re-validate all mempool transactions against updated UTXO.
    Evict any that are now invalid. Returns list of evicted tx hashes. *)
let revalidate_all t ~utxo ~current_slot =
  let to_evict = ref [] in
  Hashtbl.iter (fun hex entry ->
    let errors = Utxo.validate_tx ~utxo ~current_slot entry.decoded in
    if errors <> [] then
      to_evict := hex :: !to_evict
  ) t.entries;
  List.iter (fun hex ->
    match Hashtbl.find_opt t.entries hex with
    | None -> ()
    | Some entry ->
      Hashtbl.remove t.entries hex;
      t.total_size <- t.total_size - entry.size_bytes
  ) !to_evict;
  !to_evict

(** Expire transactions older than max_age_slots. *)
let expire t ~current_time ~max_age_seconds =
  let cutoff = current_time -. max_age_seconds in
  let to_expire = ref [] in
  Hashtbl.iter (fun hex entry ->
    if entry.added_at < cutoff then
      to_expire := hex :: !to_expire
  ) t.entries;
  List.iter (fun hex ->
    match Hashtbl.find_opt t.entries hex with
    | None -> ()
    | Some entry ->
      Hashtbl.remove t.entries hex;
      t.total_size <- t.total_size - entry.size_bytes
  ) !to_expire;
  List.length !to_expire

(** Get all transactions ordered by fee density (fee/size) descending. *)
let get_all t =
  let entries = Hashtbl.fold (fun _hex entry acc -> entry :: acc) t.entries [] in
  List.sort (fun a b ->
    let density_a = Int64.to_float a.fee /. float_of_int (max 1 a.size_bytes) in
    let density_b = Int64.to_float b.fee /. float_of_int (max 1 b.size_bytes) in
    compare density_b density_a  (* descending *)
  ) entries

(** Get a snapshot of current mempool state (for tx monitor). *)
type snapshot = {
  snap_tx_count : int;
  snap_total_size : int;
  snap_tx_hashes : bytes list;
}

let get_snapshot t =
  let hashes = Hashtbl.fold (fun hex _entry acc ->
    (* Convert hex back to bytes — simplified, store raw hash instead *)
    Bytes.of_string hex :: acc
  ) t.entries [] in
  { snap_tx_count = Hashtbl.length t.entries;
    snap_total_size = t.total_size;
    snap_tx_hashes = hashes }
