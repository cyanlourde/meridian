(* Meridian fetch — download full block bodies from a Cardano node.

   Usage: fetch [host] [port] [max_blocks]
   Default: preview-node.play.dev.cardano.org:3001, 100 blocks

   1. Connects and handshakes
   2. Runs chain-sync to collect block points (slot + hash)
   3. Uses block-fetch to download full blocks in batches
   4. Prints block info and summary *)

open Meridian

let stop = ref false

let () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\nInterrupted, shutting down...\n%!";
    stop := true));

  let host = if Array.length Sys.argv > 1 then Sys.argv.(1)
             else "preview-node.play.dev.cardano.org" in
  let port = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2)
             else 3001 in
  let max_blocks = if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3)
                   else 100 in

  Printf.printf "Connecting to %s:%d...\n%!" host port;

  let magic = Handshake.preview_magic in
  let versions = List.map (fun v ->
    (v, Handshake.default_params ~network_magic:magic)
  ) [13L; 14L; 15L] in

  match Network.connect ~timeout_s:10.0 ~host ~port () with
  | Error e -> Printf.eprintf "Connection failed: %s\n%!" e; exit 1
  | Ok net ->
    Printf.printf "Connected to %s\n%!" (Network.remote_addr net);

    (* Handshake *)
    (match Network.perform_handshake net ~versions with
     | Error e -> Printf.eprintf "Handshake failed: %s\n%!" e;
       Network.close net; exit 1
     | Ok (version, _) -> Printf.printf "Handshake OK: version %Ld\n%!" version);

    (* Chain-sync: collect block points from headers *)
    Printf.printf "Collecting %d block points via chain-sync...\n%!" max_blocks;
    (match Network.find_intersection net ~points:[Chain_sync.Origin] with
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
        (* Extract actual block point from the header *)
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

    (* Block-fetch: download in batches *)
    let batch_size = 10 in
    let total_fetched = ref 0 in
    let total_bytes = ref 0 in
    let fetching = ref true in
    let points_arr = Array.of_list points in
    let i = ref 0 in
    while !fetching && not !stop && !i < Array.length points_arr - 1 do
      let batch_end = min (Array.length points_arr - 1) (!i + batch_size - 1) in
      let from_pt = points_arr.(!i) in
      let to_pt = points_arr.(batch_end) in
      let from_slot = match from_pt with
        | Chain_sync.Point (s, _) -> s | Origin -> 0L in
      let to_slot = match to_pt with
        | Chain_sync.Point (s, _) -> s | Origin -> 0L in
      (match Network.request_range net ~from_point:from_pt ~to_point:to_pt with
       | Error e ->
         Printf.eprintf "RequestRange failed: %s\n%!" e;
         fetching := false
       | Ok No_blocks ->
         Printf.printf "No blocks for range slot %Ld..%Ld\n%!" from_slot to_slot;
         i := batch_end + 1
       | Ok Batch_started ->
         let streaming = ref true in
         while !streaming && not !stop do
           match Network.recv_block net with
           | Error e ->
             Printf.eprintf "Block recv error: %s\n%!" e;
             streaming := false; fetching := false
           | Ok None -> streaming := false  (* BatchDone *)
           | Ok (Some block_bytes) ->
             incr total_fetched;
             let sz = Bytes.length block_bytes in
             total_bytes := !total_bytes + sz;
             if !total_fetched <= 5 || !total_fetched mod 50 = 0 then
               Printf.printf "  block %d: slot %Ld..%Ld (%d bytes)\n%!"
                 !total_fetched from_slot to_slot sz
         done;
         i := batch_end + 1);
    done;

    Printf.printf "\nSummary: %d blocks fetched, %d bytes total\n%!"
      !total_fetched !total_bytes;

    (* Clean shutdown *)
    (match Network.block_fetch_done net with Ok () -> () | Error _ -> ());
    (match Network.chain_sync_done net with Ok () -> () | Error _ -> ());
    Network.close net;
    Printf.printf "Done.\n%!"
