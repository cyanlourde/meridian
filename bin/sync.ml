(* Meridian sync — sync block headers from a Cardano node.

   Usage: sync [host] [port] [max_headers]
   Default: preview-node.play.dev.cardano.org:3001, 10000 headers *)

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
  let max_headers = if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3)
                    else 10000 in

  Printf.printf "Connecting to %s:%d...\n%!" host port;

  let magic = Handshake.preview_magic in
  let versions = List.map (fun v ->
    (v, Handshake.default_params ~network_magic:magic)
  ) [13L; 14L; 15L] in

  match Network.connect ~timeout_s:10.0 ~host ~port () with
  | Error e ->
    Printf.eprintf "Connection failed: %s\n%!" e; exit 1
  | Ok net ->
    Printf.printf "Connected to %s\n%!" (Network.remote_addr net);

    (* Handshake *)
    (match Network.perform_handshake net ~versions with
     | Error e ->
       Printf.eprintf "Handshake failed: %s\n%!" e;
       Network.close net; exit 1
     | Ok (version, _params) ->
       Printf.printf "Handshake OK: version %Ld\n%!" version);

    (* Enable keep-alive responder *)
    Network.start_keep_alive_responder net;
    Printf.printf "Keep-alive responder enabled\n%!";

    (* Find intersection from origin *)
    Printf.printf "Finding intersection from genesis...\n%!";
    (match Network.find_intersection net ~points:[Chain_sync.Origin] with
     | Error e ->
       Printf.eprintf "FindIntersect failed: %s\n%!" e;
       Network.close net; exit 1
     | Ok (_, tip) ->
       let tip_slot = match tip.Chain_sync.tip_point with
         | Origin -> 0L | Point (s, _) -> s
       in
       Printf.printf "Node tip: slot %Ld, block %Ld\n%!"
         tip_slot tip.tip_block_number);

    (* Sync loop *)
    Printf.printf "Syncing headers (max %d)...\n%!" max_headers;
    let count = ref 0 in
    let running = ref true in
    while !running && not !stop && !count < max_headers do
      match Network.request_next net with
      | Error e ->
        Printf.eprintf "Sync error: %s\n%!" e;
        running := false
      | Ok (Roll_forward { header = _; tip }) ->
        incr count;
        let slot = match tip.Chain_sync.tip_point with
          | Origin -> 0L | Point (s, _) -> s
        in
        if !count <= 10 || !count mod 1000 = 0 || !count = max_headers then begin
          let (_ka_recv, ka_sent, _) = Network.keep_alive_stats net in
          Printf.printf "[%d] tip slot %Ld, tip block %Ld (keep-alive: %d pings answered)\n%!"
            !count slot tip.tip_block_number ka_sent
        end
      | Ok (Roll_backward { point; tip = _ }) ->
        let pt_str = match point with
          | Origin -> "origin"
          | Point (s, _) -> Printf.sprintf "slot %Ld" s
        in
        Printf.printf "Rollback to %s\n%!" pt_str
      | Ok Await_reply ->
        Printf.printf "At tip, waiting for new block...\n%!";
        (match Network.await_next net with
         | Error e ->
           Printf.eprintf "Await error: %s\n%!" e;
           running := false
         | Ok (Roll_forward { header = _; tip }) ->
           incr count;
           let slot = match tip.Chain_sync.tip_point with
             | Origin -> 0L | Point (s, _) -> s
           in
           Printf.printf "[%d] NEW BLOCK tip slot %Ld, tip block %Ld\n%!"
             !count slot tip.tip_block_number
         | Ok (Roll_backward { point; tip = _ }) ->
           Printf.printf "Rollback to %s\n%!" (match point with
             | Origin -> "origin" | Point (s, _) -> Printf.sprintf "slot %Ld" s)
         | Ok Await_reply ->
           Printf.eprintf "Unexpected double AwaitReply\n%!";
           running := false)
    done;

    let (ka_recv, ka_sent, _) = Network.keep_alive_stats net in
    Printf.printf "Synced %d headers (keep-alive: %d pings received, %d responses sent)\n%!"
      !count ka_recv ka_sent;

    (* Clean shutdown *)
    Network.stop_keep_alive_responder net;
    (match Network.chain_sync_done net with Ok () -> () | Error _ -> ());
    Network.close net;
    Printf.printf "Done.\n%!"
