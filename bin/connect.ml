(* Meridian connect — perform handshake with a Cardano node.

   Usage: connect [host] [port]
   Default: preview-node.play.dev.cardano.org:3001 *)

open Meridian

let () =
  let host = if Array.length Sys.argv > 1 then Sys.argv.(1)
             else "preview-node.play.dev.cardano.org" in
  let port = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2)
             else 3001 in

  Printf.printf "Connecting to %s:%d...\n%!" host port;

  (* Node-to-node protocol versions for Cardano.
     Preview testnet uses network magic 2.
     We propose versions 13-15 which are currently supported. *)
  let magic = Handshake.preview_magic in
  let versions = List.map (fun v ->
    (v, Handshake.default_params ~network_magic:magic)
  ) [13L; 14L; 15L] in

  match Network.connect ~timeout_s:10.0 ~host ~port () with
  | Error e ->
    Printf.eprintf "Connection failed: %s\n%!" e;
    exit 1
  | Ok net ->
    Printf.printf "Connected to %s\n%!" (Network.remote_addr net);
    match Network.perform_handshake net ~versions with
    | Ok (version, params) ->
      Printf.printf "Handshake OK: version %Ld, network magic %Ld\n%!"
        version params.Handshake.network_magic;
      Network.close net;
      Printf.printf "Done.\n%!"
    | Error e ->
      Printf.eprintf "Handshake failed: %s\n%!" e;
      Network.close net;
      exit 1
