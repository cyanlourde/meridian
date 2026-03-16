(* Meridian sync — unified sync pipeline with auto-reconnect.

   Usage: sync [--data-dir DIR] [--batch-size N] [--full-validation]
               [--max-blocks N] [--network NAME] [--follow MINS] [host] [port]
   Default: preview-node.play.dev.cardano.org:3001, ./meridian-data, batch 50 *)

open Meridian

let stop = ref false

let () =
  Crypto.init ();
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\nInterrupted, shutting down...\n%!";
    stop := true));
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  (* Parse args *)
  let data_dir = ref "" in
  let batch_size = ref 50 in
  let full_validation = ref false in
  let max_blocks = ref 0 in
  let network = ref "preview" in
  let follow_mins = ref 0 in
  let args = ref (Array.to_list Sys.argv |> List.tl) in
  let rec parse = function
    | "--data-dir" :: dir :: rest -> data_dir := dir; parse rest
    | "--batch-size" :: n :: rest -> batch_size := int_of_string n; parse rest
    | "--full-validation" :: rest -> full_validation := true; parse rest
    | "--max-blocks" :: n :: rest -> max_blocks := int_of_string n; parse rest
    | "--network" :: name :: rest -> network := name; parse rest
    | "--follow" :: n :: rest -> follow_mins := int_of_string n; parse rest
    | rest -> rest
  in
  args := parse !args;
  let (default_host, default_port) = Genesis.default_node_for_network !network in
  let host = match !args with h :: _ -> h | [] -> default_host in
  let port = match !args with _ :: p :: _ -> int_of_string p | _ -> default_port in
  if !data_dir = "" then data_dir := Printf.sprintf "./meridian-%s" !network;

  Printf.printf "Meridian sync\n%!";
  Printf.printf "  network:         %s\n%!" !network;
  Printf.printf "  node:            %s:%d\n%!" host port;
  Printf.printf "  data-dir:        %s\n%!" !data_dir;
  Printf.printf "  batch-size:      %d\n%!" !batch_size;
  Printf.printf "  full-validation: %b\n%!" !full_validation;
  Printf.printf "  libsodium:       %b\n%!" !(Crypto.libsodium_available);
  if !max_blocks > 0 then
    Printf.printf "  max-blocks:      %d\n%!" !max_blocks;
  if !follow_mins > 0 then
    Printf.printf "  follow:          %d min\n%!" !follow_mins;

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

  let magic = match !network with
    | "mainnet" -> Handshake.mainnet_magic
    | "preprod" -> Handshake.preprod_magic
    | _ -> Handshake.preview_magic in
  let versions = List.map (fun v ->
    (v, Handshake.default_params ~network_magic:magic)
  ) [13L; 14L; 15L] in

  let start_time = Unix.gettimeofday () in

  (* Global stats (survive reconnections) *)
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
  let tip_reached_time = ref 0.0 in
  let reconnect_count = ref 0 in
  let last_ka_recv = ref 0 in
  let last_ka_sent = ref 0 in

  (* Auto-reconnect loop *)
  let running = ref true in
  while !running && not !stop do
    if !reconnect_count > 0 then begin
      Printf.printf "\n--- Reconnecting (#%d, disk %d blocks) ---\n%!"
        !reconnect_count (Store.block_count store);
      Unix.sleepf 2.0  (* back off slightly *)
    end;

    match Network.connect ~timeout_s:10.0 ~host ~port () with
    | Error e ->
      Printf.eprintf "Connection failed: %s\n%!" e;
      if !reconnect_count >= 50 then begin
        Printf.eprintf "Too many reconnection failures, giving up.\n%!";
        running := false
      end else begin
        incr reconnect_count;
        Unix.sleepf 5.0
      end
    | Ok net ->
      Printf.printf "Connected to %s\n%!" (Network.remote_addr net);

      let handshake_ok = match Network.perform_handshake net ~versions with
        | Error e ->
          Printf.eprintf "Handshake failed: %s\n%!" e;
          Network.close net; false
        | Ok (version, _) ->
          Printf.printf "Handshake OK: version %Ld\n%!" version; true
      in
      if handshake_ok then begin
        Network.start_keep_alive_responder net;

        let config = Sync_pipeline.default_config
          ~batch_size:!batch_size
          ~on_block:(fun bi ->
            total_blocks := !total_blocks + 1;
            if !max_blocks > 0 && !total_blocks >= !max_blocks then
              stop := true;

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

                  if !total_blocks > 1 then begin
                    let r = Header_validation.validate_slot_number
                      ~prev_slot:!prev_slot ~slot:hdr.bh_slot in
                    (match r with Error e ->
                      incr header_errors;
                      log (Printf.sprintf "slot %Ld: header: %s" bi.bi_slot e)
                    | Ok () -> ())
                  end;

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
            let (ka_r, ka_s, _) = Network.keep_alive_stats net in
            let _ka_recv_total = !last_ka_recv + ka_r in
            let ka_sent_total = !last_ka_sent + ka_s in
            if !tip_reached_time = 0.0 &&
               p.tip_slot > 0L &&
               Int64.sub p.tip_slot p.current_slot < 100L then begin
              tip_reached_time := Unix.gettimeofday ();
              Printf.printf "=== TIP REACHED at slot %Ld (tip %Ld) ===\n%!"
                p.current_slot p.tip_slot;
              if !follow_mins > 0 then
                Printf.printf "Following chain for %d minutes...\n%!" !follow_mins
            end;
            if !full_validation then begin
              let utxo_n = match ledger with Some ls -> Ledger_state.utxo_count ls | None -> 0 in
              Printf.printf "[batch] synced %d | slot %Ld/%Ld | %.0f b/s | utxo %d | err h%d c%d l%d | ka %d\n%!"
                p.blocks_synced p.current_slot p.tip_slot
                p.blocks_per_sec utxo_n
                !header_errors !crypto_errors !ledger_errors ka_sent_total
            end else
              Printf.printf "[batch] synced %d | slot %Ld/%Ld | %.0f b/s | disk %d | ka %d\n%!"
                p.blocks_synced p.current_slot p.tip_slot
                p.blocks_per_sec p.disk_blocks ka_sent_total)
          ~should_stop:(fun () ->
            if !stop then true
            else if !follow_mins > 0 && !tip_reached_time > 0.0 then
              Unix.gettimeofday () -. !tip_reached_time > float_of_int !follow_mins *. 60.0
            else false) () in

        let result = Sync_pipeline.start ~net ~store ~config in

        (* Accumulate keep-alive stats *)
        let (ka_r, ka_s, _) = Network.keep_alive_stats net in
        last_ka_recv := !last_ka_recv + ka_r;
        last_ka_sent := !last_ka_sent + ka_s;

        Network.stop_keep_alive_responder net;
        (try ignore (Network.chain_sync_done net) with _ -> ());
        (try ignore (Network.block_fetch_done net) with _ -> ());
        Network.close net;

        match result with
        | Ok Sync_pipeline.Completed ->
          Printf.printf "Sync completed (at tip).\n%!";
          running := false
        | Ok Stopped ->
          Printf.printf "Sync stopped by user/timeout.\n%!";
          running := false
        | Ok (Disconnected (e, slot)) ->
          Printf.printf "Disconnected at slot %Ld: %s\n%!" slot e;
          incr reconnect_count
        | Error e ->
          Printf.printf "Sync error: %s\n%!" e;
          incr reconnect_count
      end else
        incr reconnect_count
  done;

  (* Flush any pending metadata *)
  Store.flush_meta store;

  (* Final snapshot *)
  (match ledger with
   | Some ls ->
     let path = Filename.concat !data_dir "ledger.snapshot" in
     Ledger_state.snapshot ls ~path
   | None -> ());

  let elapsed = Unix.gettimeofday () -. start_time in
  Printf.printf "\n=== Final Summary ===\n%!";
  Printf.printf "Blocks synced:   %d\n%!" !total_blocks;
  Printf.printf "Transactions:    %d\n%!" !total_txs;
  Printf.printf "Disk blocks:     %d\n%!" (Store.block_count store);
  Printf.printf "Time:            %.1fs (%.1f min)\n%!" elapsed (elapsed /. 60.0);
  Printf.printf "Avg speed:       %.0f blk/s\n%!"
    (if elapsed > 0.0 then float_of_int !total_blocks /. elapsed else 0.0);
  Printf.printf "Reconnections:   %d\n%!" !reconnect_count;
  Printf.printf "Keep-alive:      %d recv, %d sent\n%!" !last_ka_recv !last_ka_sent;
  if !full_validation then begin
    Printf.printf "Opcerts OK:      %d\n%!" !opcerts_verified;
    Printf.printf "Header errors:   %d\n%!" !header_errors;
    Printf.printf "Crypto errors:   %d\n%!" !crypto_errors;
    Printf.printf "Ledger errors:   %d\n%!" !ledger_errors;
    (match ledger with
     | Some ls ->
       Printf.printf "UTXO set:        %d entries, %Ld lovelace\n%!"
         (Ledger_state.utxo_count ls) (Ledger_state.total_lovelace ls)
     | None -> ())
  end;
  (match log_fd with Some fd -> Unix.close fd | None -> ());
  Printf.printf "Done.\n%!"
