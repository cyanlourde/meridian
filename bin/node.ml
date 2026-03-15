(* Meridian node — full node with sync + local server.

   Usage: node [--data-dir DIR] [--socket PATH] [host] [port]
   Default: preview testnet, ./meridian-data, ./meridian-data/node.socket *)

open Meridian

let stop = ref false

let () =
  Crypto.init ();
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\nShutting down...\n%!";
    stop := true));

  let data_dir = ref "./meridian-data" in
  let socket_path = ref "" in
  let args = ref (Array.to_list Sys.argv |> List.tl) in
  let rec parse = function
    | "--data-dir" :: d :: rest -> data_dir := d; parse rest
    | "--socket" :: s :: rest -> socket_path := s; parse rest
    | rest -> rest
  in
  args := parse !args;
  if !socket_path = "" then
    socket_path := Filename.concat !data_dir "node.socket";
  let host = match !args with h :: _ -> h | [] -> "preview-node.play.dev.cardano.org" in
  let port = match !args with _ :: p :: _ -> int_of_string p | _ -> 3001 in

  Printf.printf "Meridian Node\n%!";
  Printf.printf "  data-dir: %s\n%!" !data_dir;
  Printf.printf "  socket:   %s\n%!" !socket_path;
  Printf.printf "  remote:   %s:%d\n%!" host port;

  let store = Store.init ~base_dir:!data_dir () in
  Printf.printf "  blocks:   %d\n%!" (Store.block_count store);

  let ledger =
    let snap = Filename.concat !data_dir "ledger.snapshot" in
    match Ledger_state.restore ~path:snap with
    | Ok ls ->
      Printf.printf "  ledger:   restored (%d utxo)\n%!" (Ledger_state.utxo_count ls); ls
    | Error _ ->
      Printf.printf "  ledger:   new\n%!";
      Genesis.init_ledger Genesis.preview_genesis
  in

  (* Start local socket server *)
  let listener = Unix_listener.create ~socket_path:!socket_path in
  Printf.printf "Listening on %s\n%!" !socket_path;

  let ctx = Local_server.{ store; ledger; genesis = Genesis.preview_genesis } in

  (* Accept one client at a time (simple single-threaded server) *)
  (* In a real node this would be concurrent, but OCaml threads
     require more infrastructure. For now, alternate between
     checking for local clients and running sync batches. *)
  Printf.printf "Ready. Connect with: cardano-cli query tip --socket-path %s\n%!" !socket_path;

  while not !stop do
    Unix.sleepf 0.1;
    match Unix_listener.accept listener with
    | Some client_fd ->
      Printf.printf "Client connected\n%!";
      Local_server.handle_client ctx client_fd;
      Printf.printf "Client disconnected\n%!"
    | None -> ()
  done;

  (* Shutdown *)
  let snap = Filename.concat !data_dir "ledger.snapshot" in
  Ledger_state.snapshot ledger ~path:snap;
  Unix_listener.close listener;
  Printf.printf "Done.\n%!"
