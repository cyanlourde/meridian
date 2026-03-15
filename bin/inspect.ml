(* Meridian inspect — examine stored blocks and transactions.

   Usage:
     inspect block <slot>     — decode and print block at slot
     inspect header <slot>    — print just header fields
     inspect tx <slot> <idx>  — decode a specific transaction

   Requires --data-dir to point to a valid store directory. *)

open Meridian

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) b;
  Buffer.contents buf

let hex_short b =
  let hex = hex_of_bytes b in
  if String.length hex > 16 then String.sub hex 0 16 ^ "..." else hex

let () =
  let data_dir = ref "./meridian-data" in
  let args = ref (Array.to_list Sys.argv |> List.tl) in
  let rec parse = function
    | "--data-dir" :: dir :: rest -> data_dir := dir; parse rest
    | rest -> rest
  in
  args := parse !args;

  let store = Store.init ~base_dir:!data_dir () in
  Printf.printf "Store: %d blocks\n%!" (Store.block_count store);

  match !args with
  | ["block"; slot_str] ->
    let slot = Int64.of_string slot_str in
    (match Store.get_block_by_slot store ~slot with
     | None -> Printf.printf "No block at slot %Ld\n%!" slot
     | Some cbor_bytes ->
       Printf.printf "Block at slot %Ld (%d bytes)\n%!" slot (Bytes.length cbor_bytes);
       match Block_decoder.decode_block cbor_bytes with
       | Error e -> Printf.printf "Decode error: %s\n%!" e
       | Ok block ->
         let h = block.db_header in
         Printf.printf "  Era:      %s\n%!" (Block_decoder.era_name block.db_era);
         Printf.printf "  Slot:     %Ld\n%!" h.bh_slot;
         Printf.printf "  Height:   %Ld\n%!" h.bh_block_number;
         Printf.printf "  Prev:     %s\n%!" (match h.bh_prev_hash with
           | Some h -> hex_short h | None -> "(none)");
         Printf.printf "  Issuer:   %s\n%!" (hex_short h.bh_issuer_vkey);
         Printf.printf "  Proto:    %Ld.%Ld\n%!" (fst h.bh_protocol_version)
           (snd h.bh_protocol_version);
         Printf.printf "  Txs:      %d\n%!" block.db_tx_count;
         (* Decode each tx *)
         let total_inputs = ref 0 in
         let total_outputs = ref 0 in
         let total_fees = ref 0L in
         List.iter (fun tx_cbor ->
           match Tx_decoder.decode_transaction ~era:block.db_era tx_cbor with
           | Ok tx ->
             total_inputs := !total_inputs + List.length tx.dt_inputs;
             total_outputs := !total_outputs + List.length tx.dt_outputs;
             total_fees := Int64.add !total_fees tx.dt_fee
           | Error _ -> ()
         ) block.db_tx_raw;
         Printf.printf "  Inputs:   %d\n%!" !total_inputs;
         Printf.printf "  Outputs:  %d\n%!" !total_outputs;
         Printf.printf "  Fees:     %Ld lovelace\n%!" !total_fees)

  | ["header"; slot_str] ->
    let slot = Int64.of_string slot_str in
    (match Store.get_block_by_slot store ~slot with
     | None -> Printf.printf "No block at slot %Ld\n%!" slot
     | Some cbor_bytes ->
       match Block_decoder.decode_block_header cbor_bytes with
       | Error e -> Printf.printf "Decode error: %s\n%!" e
       | Ok h ->
         Printf.printf "Slot:     %Ld\n%!" h.bh_slot;
         Printf.printf "Height:   %Ld\n%!" h.bh_block_number;
         Printf.printf "Era:      %s\n%!" (Block_decoder.era_name h.bh_era);
         Printf.printf "Issuer:   %s\n%!" (hex_of_bytes h.bh_issuer_vkey);
         Printf.printf "Prev:     %s\n%!" (match h.bh_prev_hash with
           | Some h -> hex_of_bytes h | None -> "(none)");
         Printf.printf "Proto:    %Ld.%Ld\n%!" (fst h.bh_protocol_version)
           (snd h.bh_protocol_version))

  | ["tx"; slot_str; idx_str] ->
    let slot = Int64.of_string slot_str in
    let idx = int_of_string idx_str in
    (match Store.get_block_by_slot store ~slot with
     | None -> Printf.printf "No block at slot %Ld\n%!" slot
     | Some cbor_bytes ->
       match Block_decoder.decode_block cbor_bytes with
       | Error e -> Printf.printf "Block decode error: %s\n%!" e
       | Ok block ->
         if idx >= block.db_tx_count then
           Printf.printf "Tx index %d out of range (block has %d txs)\n%!" idx block.db_tx_count
         else
           let tx_cbor = List.nth block.db_tx_raw idx in
           match Tx_decoder.decode_transaction ~era:block.db_era tx_cbor with
           | Error e -> Printf.printf "Tx decode error: %s\n%!" e
           | Ok tx ->
             Printf.printf "Transaction %d in slot %Ld (%s era)\n%!"
               idx slot (Block_decoder.era_name block.db_era);
             Printf.printf "  Fee:        %Ld lovelace\n%!" tx.dt_fee;
             (match tx.dt_ttl with
              | Some ttl -> Printf.printf "  TTL:        %Ld\n%!" ttl
              | None -> ());
             Printf.printf "  Inputs:     %d\n%!" (List.length tx.dt_inputs);
             List.iteri (fun i inp ->
               Printf.printf "    [%d] %s#%Ld\n%!" i (hex_short inp.Tx_decoder.ti_tx_hash) inp.Tx_decoder.ti_index
             ) tx.dt_inputs;
             Printf.printf "  Outputs:    %d\n%!" (List.length tx.dt_outputs);
             List.iteri (fun i out ->
               let addr_info = match Address.decode_address out.Tx_decoder.to_address with
                 | Ok a -> Address.addr_type_name a.addr_type
                 | Error _ -> "?" in
               Printf.printf "    [%d] %Ld lovelace (%s)%s%s\n%!" i out.Tx_decoder.to_lovelace addr_info
                 (if out.Tx_decoder.to_has_multi_asset then " +assets" else "")
                 (if out.Tx_decoder.to_has_datum then " +datum" else "")
             ) tx.dt_outputs;
             if tx.dt_cert_count > 0 then
               Printf.printf "  Certs:      %d\n%!" tx.dt_cert_count;
             if tx.dt_mint then
               Printf.printf "  Mint:       yes\n%!")

  | _ ->
    Printf.printf "Usage:\n";
    Printf.printf "  inspect [--data-dir DIR] block <slot>\n";
    Printf.printf "  inspect [--data-dir DIR] header <slot>\n";
    Printf.printf "  inspect [--data-dir DIR] tx <slot> <idx>\n"
