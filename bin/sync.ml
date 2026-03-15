(* Meridian sync — sync block headers from a Cardano node with on-disk storage.

   Usage: sync [--data-dir DIR] [host] [port] [max_headers]
   Default: preview-node.play.dev.cardano.org:3001, 10000 headers, ./meridian-data *)

open Meridian

let stop = ref false

let () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\nInterrupted, shutting down...\n%!";
    stop := true));

  (* Parse args: optional --data-dir, then positional host port max *)
  let data_dir = ref "./meridian-data" in
  let args = Array.to_list Sys.argv |> List.tl in
  let args = match args with
    | "--data-dir" :: dir :: rest -> data_dir := dir; rest
    | _ -> args
  in
  let host = match args with h :: _ -> h | [] -> "preview-node.play.dev.cardano.org" in
  let port = match args with _ :: p :: _ -> int_of_string p | _ -> 3001 in
  let max_headers = match args with _ :: _ :: m :: _ -> int_of_string m | _ -> 10000 in

  Printf.printf "Connecting to %s:%d...\n%!" host port;
  Printf.printf "Data directory: %s\n%!" !data_dir;

  (* Init store *)
  let store = Store.init ~base_dir:!data_dir () in
  Printf.printf "Store: %d blocks on disk\n%!" (Store.block_count store);

  let magic = Handshake.preview_magic in
  let versions = List.map (fun v ->
    (v, Handshake.default_params ~network_magic:magic)
  ) [13L; 14L; 15L] in

  match Network.connect ~timeout_s:10.0 ~host ~port () with
  | Error e -> Printf.eprintf "Connection failed: %s\n%!" e; exit 1
  | Ok net ->
    Printf.printf "Connected to %s\n%!" (Network.remote_addr net);

    (match Network.perform_handshake net ~versions with
     | Error e -> Printf.eprintf "Handshake failed: %s\n%!" e;
       Network.close net; exit 1
     | Ok (version, _) -> Printf.printf "Handshake OK: version %Ld\n%!" version);

    Network.start_keep_alive_responder net;

    (* Use stored chain points for FindIntersect if we have data *)
    let chain_points = Store.get_chain_points store in
    Printf.printf "Finding intersection (%d points)...\n%!" (List.length chain_points);
    (match Network.find_intersection net ~points:chain_points with
     | Error e -> Printf.eprintf "FindIntersect failed: %s\n%!" e;
       Network.close net; exit 1
     | Ok (intersect, tip) ->
       let tip_slot = match tip.Chain_sync.tip_point with
         | Origin -> 0L | Point (s, _) -> s in
       (match intersect with
        | Some Origin -> Printf.printf "Intersection: origin\n%!"
        | Some (Point (s, _)) -> Printf.printf "Intersection: slot %Ld\n%!" s
        | None -> Printf.printf "No intersection (starting from genesis)\n%!");
       Printf.printf "Node tip: slot %Ld, block %Ld\n%!" tip_slot tip.tip_block_number);

    Printf.printf "Syncing (max %d new headers)...\n%!" max_headers;
    let count = ref 0 in
    let stored = ref 0 in
    let running = ref true in
    while !running && not !stop && !count < max_headers do
      match Network.request_next net with
      | Error e -> Printf.eprintf "Sync error: %s\n%!" e; running := false
      | Ok (Roll_forward { header; tip }) ->
        incr count;
        (* Extract point and store header as a block *)
        (match Network.extract_point_from_header header with
         | Ok (Chain_sync.Point (slot, hash)) ->
           let cbor_bytes = Cbor.encode header in
           (match Store.store_block store ~slot ~hash ~cbor_bytes with
            | Ok () -> incr stored
            | Error _ -> ())
         | _ -> ());
        if !count <= 10 || !count mod 1000 = 0 || !count = max_headers then begin
          let (_ka_recv, ka_sent, _) = Network.keep_alive_stats net in
          let tip_slot = match tip.Chain_sync.tip_point with
            | Origin -> 0L | Point (s, _) -> s in
          Printf.printf "[%d] tip slot %Ld, block %Ld | disk: %d blocks (ka: %d)\n%!"
            !count tip_slot tip.tip_block_number (Store.block_count store) ka_sent
        end
      | Ok (Roll_backward { point; tip = _ }) ->
        Printf.printf "Rollback to %s\n%!" (match point with
          | Origin -> "origin" | Point (s, _) -> Printf.sprintf "slot %Ld" s)
      | Ok Await_reply ->
        Printf.printf "At tip, waiting for new block...\n%!";
        (match Network.await_next net with
         | Error e -> Printf.eprintf "Await error: %s\n%!" e; running := false
         | Ok (Roll_forward { header; tip = _ }) ->
           incr count;
           (match Network.extract_point_from_header header with
            | Ok (Chain_sync.Point (slot, hash)) ->
              let cbor_bytes = Cbor.encode header in
              ignore (Store.store_block store ~slot ~hash ~cbor_bytes)
            | _ -> ());
           Printf.printf "[%d] NEW BLOCK\n%!" !count
         | Ok (Roll_backward _) -> Printf.printf "Rollback\n%!"
         | Ok Await_reply -> Printf.eprintf "Double AwaitReply\n%!"; running := false)
    done;

    let (ka_recv, ka_sent, _) = Network.keep_alive_stats net in
    Printf.printf "Synced %d headers, stored %d blocks (total on disk: %d)\n%!"
      !count !stored (Store.block_count store);
    Printf.printf "Keep-alive: %d received, %d sent\n%!" ka_recv ka_sent;

    Network.stop_keep_alive_responder net;
    (match Network.chain_sync_done net with Ok () -> () | Error _ -> ());
    Network.close net;
    Printf.printf "Done.\n%!"
