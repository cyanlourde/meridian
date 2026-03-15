(* Persistent ledger state: UTXO set + metadata + snapshots.

   Reference: Shelley formal ledger spec (SL-D5), Section 9

   Wraps the UTXO set with slot/epoch tracking, protocol parameters,
   and disk snapshot support for crash recovery. *)

(* ================================================================ *)
(* Protocol parameters for validation                                *)
(* ================================================================ *)

type protocol_params = {
  min_fee_a : int64;
  min_fee_b : int64;
  min_utxo_value : int64;
  max_tx_size : int;
  max_block_size : int;
  key_deposit : int64;
  pool_deposit : int64;
}

let shelley_params = {
  min_fee_a = 44L;
  min_fee_b = 155381L;
  min_utxo_value = 1000000L;
  max_tx_size = 16384;
  max_block_size = 65536;
  key_deposit = 2000000L;
  pool_deposit = 500000000L;
}

(* ================================================================ *)
(* Ledger state                                                      *)
(* ================================================================ *)

type block_error = {
  be_slot : int64;
  be_tx_index : int;
  be_errors : Utxo.validation_error list;
}

type t = {
  utxo : Utxo.utxo_set;
  mutable slot : int64;
  mutable epoch : int64;
  mutable block_count : int;
  mutable tx_count : int;
  mutable validation_errors : int;
  params : protocol_params;
  skip_validation_before_slot : int64;
  (** Skip UTXO validation for bootstrap region (genesis delegation UTXOs
      not in our initial set). Set to 0 to validate everything. *)
}

let create ?(params = shelley_params) ?(skip_validation_before_slot = 100L) () = {
  utxo = Utxo.create ();
  slot = 0L;
  epoch = 0L;
  block_count = 0;
  tx_count = 0;
  validation_errors = 0;
  params;
  skip_validation_before_slot;
}

let utxo_count t = Utxo.size t.utxo
let total_lovelace t = Utxo.total_lovelace t.utxo
let tip t = (t.slot, t.block_count)
let utxo t = t.utxo

type utxo_stats = {
  us_count : int;
  us_total_lovelace : int64;
}

let utxo_stats t = {
  us_count = Utxo.size t.utxo;
  us_total_lovelace = Utxo.total_lovelace t.utxo;
}

(* ================================================================ *)
(* Apply a decoded block                                             *)
(* ================================================================ *)

(** Compute tx hash from CBOR value. *)
let tx_hash_of_cbor cbor =
  let bytes = Cbor.encode cbor in
  Crypto.blake2b_256 bytes

(** Apply a decoded block to the ledger state.
    Returns list of block_error for any invalid transactions.
    Valid transactions are applied; invalid ones are skipped. *)
let apply_block t (block : Block_decoder.decoded_block) =
  let slot = block.db_header.bh_slot in
  let errors = ref [] in
  let invalid_set = block.db_invalid_tx_indices in
  List.iteri (fun tx_idx tx_cbor ->
    match (try Tx_decoder.decode_transaction ~era:block.db_era tx_cbor
           with _ -> Error "decode exception") with
    | Error _e -> ()  (* skip malformed tx *)
    | Ok tx ->
      (* Set is_valid from block's invalid tx list (Alonzo+) *)
      let tx = if List.mem tx_idx invalid_set then
        { tx with Tx_decoder.dt_is_valid = false } else tx in
      if Int64.compare slot t.skip_validation_before_slot >= 0 then begin
        let errs = Utxo.validate_tx
          ~min_fee_a:t.params.min_fee_a
          ~min_fee_b:t.params.min_fee_b
          ~min_utxo_value:t.params.min_utxo_value
          ~key_deposit:t.params.key_deposit
          ~pool_deposit:t.params.pool_deposit
          ~utxo:t.utxo ~current_slot:slot tx in
        if errs <> [] then begin
          errors := { be_slot = slot; be_tx_index = tx_idx; be_errors = errs } :: !errors;
          t.validation_errors <- t.validation_errors + List.length errs
        end
      end;
      let tx_hash = tx_hash_of_cbor tx_cbor in
      Utxo.apply_tx t.utxo ~tx_hash tx;
      t.tx_count <- t.tx_count + 1
  ) block.db_tx_raw;
  t.slot <- slot;
  t.block_count <- t.block_count + 1;
  t.epoch <- Epoch.slot_to_epoch Epoch.preview_epoch_params slot;
  List.rev !errors

(* ================================================================ *)
(* Snapshot format                                                   *)
(* ================================================================ *)

(* Snapshot binary format:
   Header (56 bytes):
     8 bytes: magic "MRDNSNAP"
     4 bytes: version (1)
     8 bytes: slot (big-endian)
     8 bytes: block_count (big-endian)
     8 bytes: tx_count (big-endian)
     8 bytes: utxo_count (big-endian)
     8 bytes: total_lovelace (big-endian)
     4 bytes: validation_errors (big-endian)

   Per UTxO entry:
     32 bytes: tx_hash
     4 bytes: tx_index (big-endian)
     8 bytes: lovelace (big-endian)
     2 bytes: address length (big-endian)
     N bytes: address
     1 byte: flags (bit 0=multi_asset, bit 1=datum, bit 2=script_ref) *)

let write_be64 buf off v =
  for i = 0 to 7 do
    Bytes.set_uint8 buf (off + i)
      (Int64.to_int (Int64.shift_right_logical v ((7 - i) * 8)) land 0xFF)
  done

let read_be64 buf off =
  let v = ref 0L in
  for i = 0 to 7 do
    v := Int64.logor (Int64.shift_left !v 8)
           (Int64.of_int (Bytes.get_uint8 buf (off + i)))
  done;
  !v

let write_be32 buf off v =
  Bytes.set_uint8 buf off ((v lsr 24) land 0xFF);
  Bytes.set_uint8 buf (off+1) ((v lsr 16) land 0xFF);
  Bytes.set_uint8 buf (off+2) ((v lsr 8) land 0xFF);
  Bytes.set_uint8 buf (off+3) (v land 0xFF)

let read_be32 buf off =
  (Bytes.get_uint8 buf off lsl 24) lor
  (Bytes.get_uint8 buf (off+1) lsl 16) lor
  (Bytes.get_uint8 buf (off+2) lsl 8) lor
  Bytes.get_uint8 buf (off+3)

let write_be16 buf off v =
  Bytes.set_uint8 buf off ((v lsr 8) land 0xFF);
  Bytes.set_uint8 buf (off+1) (v land 0xFF)

let read_be16 buf off =
  (Bytes.get_uint8 buf off lsl 8) lor Bytes.get_uint8 buf (off+1)

let magic = "MRDNSNAP"
let snapshot_version = 1
let header_size = 56

(** Snapshot ledger state to disk. Atomic write via temp file + rename. *)
let snapshot t ~path =
  let utxo_count = Utxo.size t.utxo in
  (* Estimate size: header + entries *)
  let est_size = header_size + utxo_count * 80 in
  let buf = Buffer.create est_size in
  (* Header *)
  let hdr = Bytes.create header_size in
  Bytes.blit_string magic 0 hdr 0 8;
  write_be32 hdr 8 snapshot_version;
  write_be64 hdr 12 t.slot;
  write_be64 hdr 20 (Int64.of_int t.block_count);
  write_be64 hdr 28 (Int64.of_int t.tx_count);
  write_be64 hdr 36 (Int64.of_int utxo_count);
  write_be64 hdr 44 (Utxo.total_lovelace t.utxo);
  write_be32 hdr 52 t.validation_errors;
  Buffer.add_bytes buf hdr;
  (* Entries *)
  Utxo.iter (fun txin txout ->
    let entry = Bytes.create (32 + 4 + 8 + 2 + Bytes.length txout.Utxo.TxOut.address + 1) in
    Bytes.blit txin.Utxo.TxIn.tx_hash 0 entry 0 32;
    write_be32 entry 32 txin.tx_index;
    write_be64 entry 36 (Multi_asset.lovelace_of txout.value);
    let alen = Bytes.length txout.address in
    write_be16 entry 44 alen;
    Bytes.blit txout.address 0 entry 46 alen;
    let flags =
      (if not (Multi_asset.is_lovelace_only txout.value) then 1 else 0) lor
      (if txout.has_datum then 2 else 0) lor
      (if txout.has_script_ref then 4 else 0) in
    Bytes.set_uint8 entry (46 + alen) flags;
    Buffer.add_bytes buf entry
  ) t.utxo;
  (* Atomic write *)
  let tmp = path ^ ".tmp" in
  let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let data = Buffer.to_bytes buf in
  let len = Bytes.length data in
  let rec go off remaining =
    if remaining = 0 then ()
    else let n = Unix.write fd data off remaining in go (off + n) (remaining - n)
  in
  (try go 0 len with e -> Unix.close fd; raise e);
  Unix.close fd;
  Unix.rename tmp path

(** Restore ledger state from a snapshot file. *)
let restore ~path =
  if not (Sys.file_exists path) then
    Error "snapshot file not found"
  else begin
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    let st = Unix.fstat fd in
    let data = Bytes.create st.Unix.st_size in
    let rec go off remaining =
      if remaining = 0 then ()
      else let n = Unix.read fd data off remaining in go (off + n) (remaining - n)
    in
    go 0 st.st_size;
    Unix.close fd;
    if st.st_size < header_size then Error "snapshot too small"
    else if Bytes.sub_string data 0 8 <> magic then Error "bad snapshot magic"
    else begin
      let _version = read_be32 data 8 in
      let slot = read_be64 data 12 in
      let block_count = Int64.to_int (read_be64 data 20) in
      let tx_count = Int64.to_int (read_be64 data 28) in
      let utxo_count = Int64.to_int (read_be64 data 36) in
      let _total = read_be64 data 44 in
      let validation_errors = read_be32 data 52 in
      let t = create () in
      t.slot <- slot;
      t.block_count <- block_count;
      t.tx_count <- tx_count;
      t.validation_errors <- validation_errors;
      t.epoch <- Epoch.slot_to_epoch Epoch.preview_epoch_params slot;
      (* Read entries *)
      let off = ref header_size in
      for _ = 1 to utxo_count do
        if !off + 47 <= st.st_size then begin
          let tx_hash = Bytes.sub data !off 32 in
          let tx_index = read_be32 data (!off + 32) in
          let lovelace = read_be64 data (!off + 36) in
          let alen = read_be16 data (!off + 44) in
          let address = if !off + 46 + alen <= st.st_size then
            Bytes.sub data (!off + 46) alen else Bytes.empty in
          let flags = if !off + 46 + alen < st.st_size then
            Bytes.get_uint8 data (!off + 46 + alen) else 0 in
          let txin = Utxo.TxIn.{ tx_hash; tx_index } in
          let txout = Utxo.TxOut.{
            address;
            value = Multi_asset.of_lovelace lovelace;
            (* Multi-asset data not persisted in snapshot v1 *)
            has_datum = flags land 2 <> 0;
            has_script_ref = flags land 4 <> 0;
          } in
          Utxo.add t.utxo txin txout;
          off := !off + 47 + alen
        end
      done;
      Ok t
    end
  end
