(* Meridian validate — chain validation CLI tool.

   Usage:
     validate chain [--data-dir DIR]
     validate tip [--data-dir DIR]
     validate block <slot> [--data-dir DIR] *)

open Meridian

let () =
  let data_dir = ref "./meridian-data" in
  let args = ref (Array.to_list Sys.argv |> List.tl) in
  let rec parse = function
    | "--data-dir" :: dir :: rest -> data_dir := dir; parse rest
    | rest -> rest
  in
  args := parse !args;

  let store = Store.init ~base_dir:!data_dir () in
  let count = Store.block_count store in
  Printf.printf "Store: %d blocks\n%!" count;

  let start_time = Unix.gettimeofday () in

  match !args with
  | ["chain"] ->
    Printf.printf "Validating entire chain...\n%!";
    (match Store.tip store with
     | None -> Printf.printf "Empty store, nothing to validate.\n%!"
     | Some (tip_slot, _) ->
       let errors = Chain_validation.validate_chain store ~from_slot:0L ~to_slot:tip_slot in
       let elapsed = Unix.gettimeofday () -. start_time in
       Printf.printf "Checked %d blocks in %.1fs\n%!" count elapsed;
       if errors = [] then begin
         Printf.printf "Chain valid. No errors.\n%!"; exit 0
       end else begin
         Printf.printf "%d blocks with errors:\n%!" (List.length errors);
         List.iter (fun e ->
           Printf.printf "  slot %Ld: %s\n%!" e.Chain_validation.ce_slot
             (String.concat "; " e.ce_errors)
         ) errors;
         exit 1
       end)

  | ["tip"] ->
    Printf.printf "Validating last 100 blocks...\n%!";
    let errors = Chain_validation.validate_chain_tip store ~count:100 in
    let elapsed = Unix.gettimeofday () -. start_time in
    Printf.printf "Checked in %.1fs\n%!" elapsed;
    if errors = [] then begin
      Printf.printf "Tip valid.\n%!"; exit 0
    end else begin
      Printf.printf "%d errors:\n%!" (List.length errors);
      List.iter (fun e ->
        Printf.printf "  slot %Ld: %s\n%!" e.Chain_validation.ce_slot
          (String.concat "; " e.ce_errors)
      ) errors;
      exit 1
    end

  | ["block"; slot_str] ->
    let slot = Int64.of_string slot_str in
    (match Store.get_block_by_slot store ~slot with
     | None -> Printf.printf "No block at slot %Ld\n%!" slot; exit 1
     | Some cbor_bytes ->
       match Block_decoder.decode_block_header cbor_bytes with
       | Error e -> Printf.printf "Decode error: %s\n%!" e; exit 1
       | Ok header ->
         Printf.printf "Block at slot %Ld (%s era)\n%!" slot
           (Block_decoder.era_name header.bh_era);
         (* Check size *)
         (match Header_validation.validate_block_size
                  ~raw_block_cbor:cbor_bytes ~era:header.bh_era with
          | Ok () -> Printf.printf "  Size: %d bytes (OK)\n%!" (Bytes.length cbor_bytes)
          | Error e -> Printf.printf "  Size: %s\n%!" e);
         Printf.printf "  Proto: %Ld.%Ld\n%!" (fst header.bh_protocol_version)
           (snd header.bh_protocol_version);
         (match Header_validation.validate_protocol_version ~header with
          | Ok () -> Printf.printf "  Proto version: OK\n%!"
          | Error e -> Printf.printf "  Proto version: %s\n%!" e);
         exit 0)

  | _ ->
    Printf.printf "Usage:\n";
    Printf.printf "  validate chain [--data-dir DIR]\n";
    Printf.printf "  validate tip [--data-dir DIR]\n";
    Printf.printf "  validate block <slot> [--data-dir DIR]\n"
