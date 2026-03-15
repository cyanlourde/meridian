(* Meridian sync — unified sync pipeline with full validation.

   Usage: sync [--data-dir DIR] [--batch-size N] [--full-validation]
               [--max-blocks N] [host] [port]
   Default: preview-node.play.dev.cardano.org:3001, ./meridian-data, batch 50 *)

open Meridian

let stop = ref false

let () =
  Crypto.init ();
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\nInterrupted, shutting down...\n%!";
    stop := true));

  (* Parse args *)
  let data_dir = ref "./meridian-data" in
  let batch_size = ref 50 in
  let full_validation = ref false in
  let max_blocks = ref 0 in  (* 0 = unlimited *)
  let args = ref (Array.to_list Sys.argv |> List.tl) in
  let rec parse = function
    | "--data-dir" :: dir :: rest -> data_dir := dir; parse rest
    | "--batch-size" :: n :: rest -> batch_size := int_of_string n; parse rest
    | "--full-validation" :: rest -> full_validation := true; parse rest
    | "--max-blocks" :: n :: rest -> max_blocks := int_of_string n; parse rest
    | rest -> rest
  in
  args := parse !args;
  let host = match !args with h :: _ -> h | [] -> "preview-node.play.dev.cardano.org" in
  let port = match !args with _ :: p :: _ -> int_of_string p | _ -> 3001 in

  Printf.printf "Meridian sync\n%!";
  Printf.printf "  node:            %s:%d\n%!" host port;
  Printf.printf "  data-dir:        %s\n%!" !data_dir;
  Printf.printf "  batch-size:      %d\n%!" !batch_size;
  Printf.printf "  full-validation: %b\n%!" !full_validation;
  Printf.printf "  libsodium:       %b\n%!" !(Crypto.libsodium_available);
  if !max_blocks > 0 then
    Printf.printf "  max-blocks:      %d\n%!" !max_blocks;

  let store = Store.init ~base_dir:!data_dir () in
  Printf.printf "  disk:            %d blocks\n%!" (Store.block_count store);

  (* Init ledger if full validation *)
  let ledger = if !full_validation then begin
    let snapshot_path = Filename.concat !data_dir "ledger.snapshot" in
    match Ledger_state.restore ~path:snapshot_path with
    | Ok ls ->
      let (slot, blocks) = Ledger_state.tip ls in
      Printf.printf "  ledger:          restored (slot %Ld, %d blocks, %d utxo)\n%!"
        slot blocks (Ledger_state.utxo_count ls);
      Some ls
    | Error _ ->
      Printf.printf "  ledger:          new (empty)\n%!";
      Some (Ledger_state.create ())
  end else None in

  (* Validation log *)
  let log_fd = if !full_validation then begin
    let path = Filename.concat !data_dir "validation.log" in
    let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o644 in
    Some fd
  end else None in
  let log msg =
    match log_fd with
    | Some fd ->
      let line = Printf.sprintf "[%s] %s\n"
        (let t = Unix.gettimeofday () in
         let lt = Unix.localtime t in
         Printf.sprintf "%02d:%02d:%02d" lt.tm_hour lt.tm_min lt.tm_sec)
        msg in
      let b = Bytes.of_string line in
      ignore (Unix.write fd b 0 (Bytes.length b))
    | None -> ()
  in

  let magic = Handshake.preview_magic in
  let versions = List.map (fun v ->
    (v, Handshake.default_params ~network_magic:magic)
  ) [13L; 14L; 15L] in

  let start_time = Unix.gettimeofday () in

  match Network.connect ~timeout_s:10.0 ~host ~port () with
  | Error e -> Printf.eprintf "Connection failed: %s\n%!" e; exit 1
  | Ok net ->
    Printf.printf "Connected to %s\n%!" (Network.remote_addr net);

    (match Network.perform_handshake net ~versions with
     | Error e -> Printf.eprintf "Handshake failed: %s\n%!" e;
       Network.close net; exit 1
     | Ok (version, _) -> Printf.printf "Handshake OK: version %Ld\n%!" version);

    Network.start_keep_alive_responder net;

    (* Validation stats *)
    let total_blocks = ref 0 in
    let total_txs = ref 0 in
    let header_errors = ref 0 in
    let crypto_errors = ref 0 in
    let ledger_errors = ref 0 in
    let opcerts_verified = ref 0 in
    let prev_slot = ref 0L in
    let prev_block_number = ref 0L in
    let prev_hash = ref Bytes.empty in
    let current_era = ref "unknown" in
    let snapshot_interval = 10000 in

    let config = Sync_pipeline.default_config
      ~batch_size:!batch_size
      ~on_block:(fun bi ->
        total_blocks := !total_blocks + 1;
        if !max_blocks > 0 && !total_blocks >= !max_blocks then
          stop := true;

        (* Full validation *)
        if !full_validation then begin
          match Store.get_block store ~hash:bi.bi_hash with
          | None -> ()
          | Some cbor_bytes ->
            match Block_decoder.decode_block cbor_bytes with
            | Error e ->
              log (Printf.sprintf "slot %Ld: decode error: %s" bi.bi_slot e)
            | Ok block ->
              let hdr = block.db_header in
              current_era := Block_decoder.era_name block.db_era;
              total_txs := !total_txs + block.db_tx_count;

              (* 1. Header validation *)
              if !total_blocks > 1 then begin
                let r = Header_validation.validate_slot_number
                  ~prev_slot:!prev_slot ~slot:hdr.bh_slot in
                (match r with Error e ->
                  incr header_errors;
                  log (Printf.sprintf "slot %Ld: header: %s" bi.bi_slot e)
                | Ok () -> ())
              end;

              (* 2. Crypto validation (Shelley+ only) *)
              (match block.db_era with
               | Byron -> ()
               | _ ->
                 match Block_validator.validate_block_crypto block with
                 | Ok () ->
                   (match hdr.bh_opcert with Some _ -> incr opcerts_verified | None -> ())
                 | Error errs ->
                   List.iter (fun e ->
                     incr crypto_errors;
                     log (Printf.sprintf "slot %Ld: crypto: %s" bi.bi_slot
                       (Block_validator.error_to_string e))
                   ) errs);

              (* 3. Ledger validation *)
              (match ledger with
               | None -> ()
               | Some ls ->
                 let errs = Ledger_state.apply_block ls block in
                 List.iter (fun (e : Ledger_state.block_error) ->
                   List.iter (fun ve ->
                     incr ledger_errors;
                     log (Printf.sprintf "slot %Ld tx%d: ledger: %s"
                       e.be_slot e.be_tx_index (Utxo.error_to_string ve))
                   ) e.be_errors
                 ) errs;
                 (* Snapshot periodically *)
                 let (_, lblocks) = Ledger_state.tip ls in
                 if lblocks > 0 && lblocks mod snapshot_interval = 0 then begin
                   let path = Filename.concat !data_dir "ledger.snapshot" in
                   Ledger_state.snapshot ls ~path;
                   log (Printf.sprintf "snapshot at block %d" lblocks)
                 end);

              prev_slot := hdr.bh_slot;
              prev_block_number := hdr.bh_block_number;
              prev_hash := Crypto.blake2b_256 cbor_bytes
        end;

        if !total_blocks <= 5 || !total_blocks mod 500 = 0 then
          Printf.printf "  [%d] slot %Ld era=%s txs=%d\n%!"
            !total_blocks bi.bi_slot !current_era !total_txs)
      ~on_progress:(fun p ->
        let (_, ka_sent, _) = Network.keep_alive_stats net in
        if !full_validation then begin
          let utxo_n = match ledger with Some ls -> Ledger_state.utxo_count ls | None -> 0 in
          Printf.printf "[batch] synced %d | slot %Ld/%Ld | %.0f b/s | utxo %d | err h%d c%d l%d | ka %d\n%!"
            p.blocks_synced p.current_slot p.tip_slot
            p.blocks_per_sec utxo_n
            !header_errors !crypto_errors !ledger_errors ka_sent
        end else
          Printf.printf "[batch] synced %d | slot %Ld/%Ld | %.0f b/s | disk %d | ka %d\n%!"
            p.blocks_synced p.current_slot p.tip_slot
            p.blocks_per_sec p.disk_blocks ka_sent)
      ~should_stop:(fun () -> !stop) () in

    let result = Sync_pipeline.start ~net ~store ~config in

    (* Final snapshot *)
    (match ledger with
     | Some ls ->
       let path = Filename.concat !data_dir "ledger.snapshot" in
       Ledger_state.snapshot ls ~path
     | None -> ());

    let elapsed = Unix.gettimeofday () -. start_time in
    let (ka_recv, ka_sent, _) = Network.keep_alive_stats net in
    Printf.printf "\n--- Summary ---\n%!";
    Printf.printf "Blocks synced: %d\n%!" !total_blocks;
    Printf.printf "Transactions:  %d\n%!" !total_txs;
    Printf.printf "Disk blocks:   %d\n%!" (Store.block_count store);
    Printf.printf "Time:          %.1fs\n%!" elapsed;
    Printf.printf "Speed:         %.0f blk/s\n%!"
      (if elapsed > 0.0 then float_of_int !total_blocks /. elapsed else 0.0);
    Printf.printf "Keep-alive:    %d recv, %d sent\n%!" ka_recv ka_sent;
    if !full_validation then begin
      Printf.printf "Opcerts OK:    %d\n%!" !opcerts_verified;
      Printf.printf "Header errors: %d\n%!" !header_errors;
      Printf.printf "Crypto errors: %d\n%!" !crypto_errors;
      Printf.printf "Ledger errors: %d\n%!" !ledger_errors;
      (match ledger with
       | Some ls ->
         Printf.printf "UTXO set:      %d entries, %Ld lovelace\n%!"
           (Ledger_state.utxo_count ls) (Ledger_state.total_lovelace ls)
       | None -> ())
    end;
    (match result with
     | Ok Sync_pipeline.Completed -> Printf.printf "Status:        completed\n%!"
     | Ok Stopped -> Printf.printf "Status:        stopped\n%!"
     | Ok (Disconnected (e, slot)) ->
       Printf.printf "Status:        disconnected at slot %Ld (%s)\n%!" slot e
     | Error e -> Printf.printf "Status:        error: %s\n%!" e);

    (match log_fd with Some fd -> Unix.close fd | None -> ());
    Network.stop_keep_alive_responder net;
    (match Network.chain_sync_done net with Ok () -> () | Error _ -> ());
    (match Network.block_fetch_done net with Ok () -> () | Error _ -> ());
    Network.close net;
    Printf.printf "Done.\n%!"
