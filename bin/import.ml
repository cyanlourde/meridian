(* Meridian import — bootstrap from a Mithril snapshot.

   Usage:
     import snapshot --data-dir DIR                    (latest snapshot)
     import snapshot --digest HASH --data-dir DIR      (specific snapshot)
     import snapshot --file PATH --data-dir DIR        (local archive) *)

open Meridian

let () =
  Crypto.init ();
  let data_dir = ref "./meridian-data" in
  let digest = ref "" in
  let file_path = ref "" in
  let args = ref (Array.to_list Sys.argv |> List.tl) in
  let rec parse = function
    | "--data-dir" :: dir :: rest -> data_dir := dir; parse rest
    | "--digest" :: d :: rest -> digest := d; parse rest
    | "--file" :: f :: rest -> file_path := f; parse rest
    | rest -> rest
  in
  args := parse !args;

  match !args with
  | ["snapshot"] ->
    Printf.printf "Meridian Mithril Import\n%!";
    Printf.printf "  data-dir: %s\n%!" !data_dir;

    let store = Store.init ~base_dir:!data_dir () in
    if Store.block_count store > 0 then begin
      Printf.printf "Store already has %d blocks. Use --data-dir with empty dir.\n%!"
        (Store.block_count store);
      exit 0
    end;

    let archive_path = if !file_path <> "" then begin
      Printf.printf "Using local archive: %s\n%!" !file_path;
      !file_path
    end else begin
      (* Fetch from aggregator *)
      Printf.printf "Fetching snapshot list from aggregator...\n%!";
      match Mithril_client.list_snapshots () with
      | Error e ->
        Printf.eprintf "Failed to list snapshots: %s\n%!" e; exit 1
      | Ok [] ->
        Printf.eprintf "No snapshots available\n%!"; exit 1
      | Ok snapshots ->
        let snap = if !digest <> "" then
          match List.find_opt (fun s -> s.Mithril_client.digest = !digest) snapshots with
          | Some s -> s
          | None -> Printf.eprintf "Digest not found\n%!"; exit 1
        else
          List.hd snapshots  (* latest *)
        in
        Printf.printf "Snapshot: epoch %Ld, immutable %Ld, %Ld bytes\n%!"
          snap.beacon_epoch snap.beacon_immutable snap.size;
        Printf.printf "Digest: %s\n%!" snap.digest;

        (* Download *)
        let url = match snap.locations with
          | loc :: _ -> loc
          | [] -> Printf.eprintf "No download locations\n%!"; exit 1
        in
        let dest = Filename.concat !data_dir "snapshot.tar.zst" in
        (try Unix.mkdir !data_dir 0o755 with Unix.Unix_error (Unix.EEXIST,_,_) -> ());
        Printf.printf "Downloading from %s...\n%!" (String.sub url 0 (min 80 (String.length url)));
        (match Mithril_client.download_snapshot ~url ~dest with
         | Error e -> Printf.eprintf "Download failed: %s\n%!" e; exit 1
         | Ok () -> Printf.printf "Download complete\n%!");

        (* Verify digest *)
        Printf.printf "Verifying digest...\n%!";
        (match Mithril_verify.verify_snapshot_digest
                 ~expected_digest:snap.digest ~snapshot_path:dest with
         | Ok () -> Printf.printf "Digest verified OK\n%!"
         | Error e -> Printf.printf "Digest verification: %s (continuing anyway)\n%!" e);

        dest
    end in

    (* Unpack *)
    let unpack_dir = Filename.concat !data_dir "immutable-import" in
    Printf.printf "Unpacking snapshot...\n%!";
    (match Mithril_import.unpack_snapshot ~archive_path ~dest_dir:unpack_dir with
     | Error e -> Printf.eprintf "Unpack failed: %s\n%!" e; exit 1
     | Ok () -> Printf.printf "Unpacked to %s\n%!" unpack_dir);

    (* Find immutable directory *)
    let immutable_dir =
      let candidate = Filename.concat unpack_dir "immutable" in
      if Sys.file_exists candidate then candidate
      else
        (* Search one level deeper *)
        let entries = try Sys.readdir unpack_dir with _ -> [||] in
        let found = Array.to_list entries |> List.find_opt (fun e ->
          Sys.file_exists (Filename.concat (Filename.concat unpack_dir e) "immutable")
        ) in
        match found with
        | Some sub -> Filename.concat (Filename.concat unpack_dir sub) "immutable"
        | None -> Printf.eprintf "Cannot find immutable directory in snapshot\n%!"; exit 1
    in
    Printf.printf "Immutable DB: %s\n%!" immutable_dir;

    (* Import *)
    Printf.printf "Importing blocks...\n%!";
    let start_time = Unix.gettimeofday () in
    (match Mithril_import.import_immutable_db ~immutable_dir ~store
             ~on_progress:(fun stats ->
               Printf.printf "\r  chunks: %d, blocks: %d, bytes: %d%!"
                 stats.chunks_processed stats.blocks_imported stats.bytes_total)
    with
     | Error e -> Printf.eprintf "\nImport failed: %s\n%!" e; exit 1
     | Ok stats ->
       let elapsed = Unix.gettimeofday () -. start_time in
       Printf.printf "\nImport complete: %d blocks from %d chunks in %.1fs\n%!"
         stats.blocks_imported stats.chunks_processed elapsed;
       Printf.printf "  Errors: %d\n%!" stats.errors);

    Printf.printf "Store: %d blocks\n%!" (Store.block_count store);
    (match Store.tip store with
     | Some (slot, _) -> Printf.printf "Tip: slot %Ld\n%!" slot
     | None -> Printf.printf "Tip: (empty)\n%!");

    Printf.printf "Done. Run 'sync --data-dir %s' to continue live sync.\n%!" !data_dir

  | _ ->
    Printf.printf "Usage:\n";
    Printf.printf "  import snapshot [--data-dir DIR] [--digest HASH] [--file PATH]\n"
