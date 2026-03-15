(* Server side of node-to-node block-fetch (protocol ID 3).

   Serves full block bodies to peers from the Store. *)

let ( let* ) = Result.bind

let send mux msg =
  let payload = Block_fetch.to_bytes msg in
  Mux.send_segment mux ~protocol_id:Miniprotocol.block_fetch ~timestamp:0l payload

(** Serve a block range request. *)
let handle_request_range ~store ~mux ~from_point ~to_point =
  let from_slot = match from_point with
    | Chain_sync.Point (s, _) -> s | Origin -> 0L in
  let to_slot = match to_point with
    | Chain_sync.Point (s, _) -> s | Origin -> 0L in
  (* Collect blocks in the range *)
  let recent = Store.get_recent_blocks store ~count:(Store.block_count store) in
  let blocks_in_range = List.filter (fun (slot, _) ->
    Int64.compare slot from_slot >= 0 && Int64.compare slot to_slot <= 0
  ) recent in
  if blocks_in_range = [] then
    let* () = send mux MsgNoBlocks in Ok 0
  else begin
    let* () = send mux MsgStartBatch in
    let sent = ref 0 in
    List.iter (fun (slot, _hash) ->
      match Store.get_block_by_slot store ~slot with
      | None -> ()
      | Some cbor_bytes ->
        ignore (send mux (MsgBlock cbor_bytes));
        incr sent
    ) blocks_in_range;
    let* () = send mux MsgBatchDone in
    Ok !sent
  end

(** Handle one block-fetch message. *)
let handle ~store ~mux ~recv_payload =
  let* payload = recv_payload () in
  let* msg = Block_fetch.of_bytes payload in
  match msg with
  | MsgRequestRange (from_pt, to_pt) ->
    handle_request_range ~store ~mux ~from_point:from_pt ~to_point:to_pt
  | MsgClientDone -> Ok 0
  | _ -> Error "block_fetch_server: unexpected message"
