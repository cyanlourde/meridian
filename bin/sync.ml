(* Meridian sync — unified sync pipeline.

   Usage: sync [--data-dir DIR] [--batch-size N] [host] [port]
   Default: preview-node.play.dev.cardano.org:3001, ./meridian-data, batch 50 *)

open Meridian

let hex_short b =
  let buf = Buffer.create 16 in
  let n = min 8 (Bytes.length b) in
  for i = 0 to n - 1 do
    Buffer.add_string buf (Printf.sprintf "%02x" (Bytes.get_uint8 b i))
  done;
  Buffer.contents buf

let stop = ref false

let () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\nInterrupted, shutting down...\n%!";
    stop := true));

  (* Parse args *)
  let data_dir = ref "./meridian-data" in
  let batch_size = ref 50 in
  let args = ref (Array.to_list Sys.argv |> List.tl) in
  let rec parse = function
    | "--data-dir" :: dir :: rest -> data_dir := dir; parse rest
    | "--batch-size" :: n :: rest -> batch_size := int_of_string n; parse rest
    | rest -> rest
  in
  args := parse !args;
  let host = match !args with h :: _ -> h | [] -> "preview-node.play.dev.cardano.org" in
  let port = match !args with _ :: p :: _ -> int_of_string p | _ -> 3001 in

  Printf.printf "Meridian sync\n%!";
  Printf.printf "  node:       %s:%d\n%!" host port;
  Printf.printf "  data-dir:   %s\n%!" !data_dir;
  Printf.printf "  batch-size: %d\n%!" !batch_size;

  let store = Store.init ~base_dir:!data_dir () in
  Printf.printf "  disk:       %d blocks\n%!" (Store.block_count store);

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

    let total_blocks = ref 0 in
    let total_bytes = ref 0 in

    let config = Sync_pipeline.default_config
      ~batch_size:!batch_size
      ~on_block:(fun bi ->
        total_blocks := !total_blocks + 1;
        total_bytes := !total_bytes + bi.bi_size;
        if !total_blocks <= 5 || !total_blocks mod 1000 = 0 then
          Printf.printf "  [%d] slot %Ld hash %s..\n%!"
            !total_blocks bi.bi_slot (hex_short bi.bi_hash))
      ~on_progress:(fun p ->
        let (_, ka_sent, _) = Network.keep_alive_stats net in
        Printf.printf "[batch] synced %d | slot %Ld / %Ld | %.0f blk/s | disk %d | ka %d\n%!"
          p.blocks_synced p.current_slot p.tip_slot
          p.blocks_per_sec p.disk_blocks ka_sent)
      ~should_stop:(fun () -> !stop) () in

    let result = Sync_pipeline.start ~net ~store ~config in

    let elapsed = Unix.gettimeofday () -. start_time in
    let (ka_recv, ka_sent, _) = Network.keep_alive_stats net in
    Printf.printf "\n--- Summary ---\n%!";
    Printf.printf "Blocks synced: %d\n%!" !total_blocks;
    Printf.printf "Disk blocks:   %d\n%!" (Store.block_count store);
    Printf.printf "Time:          %.1fs\n%!" elapsed;
    Printf.printf "Speed:         %.0f blk/s\n%!"
      (if elapsed > 0.0 then float_of_int !total_blocks /. elapsed else 0.0);
    Printf.printf "Keep-alive:    %d recv, %d sent\n%!" ka_recv ka_sent;
    (match result with
     | Ok Sync_pipeline.Completed -> Printf.printf "Status:        completed (at tip)\n%!"
     | Ok Stopped -> Printf.printf "Status:        stopped (user interrupt)\n%!"
     | Ok (Disconnected (e, slot)) ->
       Printf.printf "Status:        disconnected at slot %Ld (%s)\n%!" slot e
     | Error e -> Printf.printf "Status:        error: %s\n%!" e);

    Network.stop_keep_alive_responder net;
    (match Network.chain_sync_done net with Ok () -> () | Error _ -> ());
    (match Network.block_fetch_done net with Ok () -> () | Error _ -> ());
    Network.close net;
    Printf.printf "Done.\n%!"
