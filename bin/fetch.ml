(* Meridian fetch — download full block bodies with on-disk storage.

   Usage: fetch [--data-dir DIR] [host] [port] [max_blocks]
   Default: preview-node.play.dev.cardano.org:3001, 500 blocks, ./meridian-data *)

open Meridian

let stop = ref false

let () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\nInterrupted, shutting down...\n%!";
    stop := true));

  let data_dir = ref "./meridian-data" in
  let args = Array.to_list Sys.argv |> List.tl in
  let args = match args with
    | "--data-dir" :: dir :: rest -> data_dir := dir; rest
    | _ -> args
  in
  let host = match args with h :: _ -> h | [] -> "preview-node.play.dev.cardano.org" in
  let port = match args with _ :: p :: _ -> int_of_string p | _ -> 3001 in
  let max_blocks = match args with _ :: _ :: m :: _ -> int_of_string m | _ -> 500 in

  Printf.printf "Connecting to %s:%d...\n%!" host port;
  Printf.printf "Data directory: %s\n%!" !data_dir;

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

    let chain_points = Store.get_chain_points store in
    Printf.printf "Collecting %d block points via chain-sync (%d stored points)...\n%!"
      max_blocks (List.length chain_points);
    (match Network.find_intersection net ~points:chain_points with
     | Error e -> Printf.eprintf "FindIntersect failed: %s\n%!" e;
       Network.close net; exit 1
     | Ok (_, tip) ->
       Printf.printf "Node tip: block %Ld\n%!" tip.Chain_sync.tip_block_number);

    let points = ref [] in
    let header_count = ref 0 in
    let collecting = ref true in
    while !collecting && not !stop && !header_count < max_blocks do
      match Network.request_next net with
      | Error e ->
        Printf.eprintf "Chain-sync error: %s\n%!" e; collecting := false
      | Ok (Roll_forward { header; tip = _ }) ->
        incr header_count;
        (match Network.extract_point_from_header header with
         | Ok (Chain_sync.Point _ as pt) -> points := pt :: !points
         | _ -> ());
        if !header_count mod 100 = 0 then
          Printf.printf "  collected %d headers\n%!" !header_count
      | Ok (Roll_backward _) -> ()
      | Ok Await_reply ->
        Printf.printf "  at tip after %d headers\n%!" !header_count;
        collecting := false
    done;

    let points = List.rev !points in
    let num_points = List.length points in
    Printf.printf "Collected %d block points\n%!" num_points;

    if num_points < 2 then begin
      Printf.printf "Need at least 2 points for block-fetch.\n%!";
      Network.close net; exit 0
    end;

    let batch_size = 10 in
    let total_fetched = ref 0 in
    let total_bytes = ref 0 in
    let total_stored = ref 0 in
    let fetching = ref true in
    let points_arr = Array.of_list points in
    let batch_idx = ref 0 in
    let i = ref 0 in
    while !fetching && not !stop && !i < Array.length points_arr - 1 do
      let batch_end = min (Array.length points_arr - 1) (!i + batch_size - 1) in
      let from_pt = points_arr.(!i) in
      let to_pt = points_arr.(batch_end) in
      incr batch_idx;
      (match Network.request_range net ~from_point:from_pt ~to_point:to_pt with
       | Error e ->
         Printf.eprintf "RequestRange failed: %s\n%!" e; fetching := false
       | Ok No_blocks ->
         i := batch_end + 1
       | Ok Batch_started ->
         let block_in_batch = ref 0 in
         let streaming = ref true in
         while !streaming && not !stop do
           match Network.recv_block net with
           | Error e ->
             Printf.eprintf "Block recv error: %s\n%!" e;
             streaming := false; fetching := false
           | Ok None -> streaming := false
           | Ok (Some block_bytes) ->
             incr total_fetched;
             incr block_in_batch;
             let sz = Bytes.length block_bytes in
             total_bytes := !total_bytes + sz;
             (* Store block using the corresponding point *)
             let pt_idx = !i + !block_in_batch - 1 in
             if pt_idx < Array.length points_arr then begin
               match points_arr.(pt_idx) with
               | Chain_sync.Point (slot, hash) ->
                 (match Store.store_block store ~slot ~hash ~cbor_bytes:block_bytes with
                  | Ok () -> incr total_stored
                  | Error _ -> ())
               | _ -> ()
             end;
             if !total_fetched <= 5 || !total_fetched mod 100 = 0 then
               Printf.printf "  block %d: %d bytes (disk: %d)\n%!"
                 !total_fetched sz (Store.block_count store)
         done;
         i := batch_end + 1);
    done;

    let (ka_recv, ka_sent, _) = Network.keep_alive_stats net in
    Printf.printf "\nSummary: %d blocks fetched, %d stored, %d bytes (ka: %d/%d, disk: %d)\n%!"
      !total_fetched !total_stored !total_bytes ka_recv ka_sent (Store.block_count store);

    Network.stop_keep_alive_responder net;
    (match Network.block_fetch_done net with Ok () -> () | Error _ -> ());
    (match Network.chain_sync_done net with Ok () -> () | Error _ -> ());
    Network.close net;
    Printf.printf "Done.\n%!"
