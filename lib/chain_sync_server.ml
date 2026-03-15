(* Server side of node-to-node chain-sync (protocol ID 2).

   Serves block headers to peers from the Store. *)

let ( let* ) = Result.bind

(** Send a chain-sync message on the mux. *)
let send mux msg =
  let payload = Chain_sync.to_bytes msg in
  Mux.send_segment mux ~protocol_id:Miniprotocol.chain_sync ~timestamp:0l payload

(** Receive a chain-sync message from the mux segment payload. *)
let decode_msg payload = Chain_sync.of_bytes payload

(** Find the best intersection between client points and our chain. *)
let find_intersection store points =
  (* Check non-Origin points first (newest to oldest), then Origin as fallback *)
  let non_origin = List.filter (fun pt -> pt <> Chain_sync.Origin) points in
  match List.find_map (fun pt ->
    match pt with
    | Chain_sync.Point (_slot, hash) ->
      if Store.has_block store ~hash then Some pt else None
    | _ -> None
  ) non_origin with
  | Some pt -> Some pt
  | None ->
    if List.exists (fun pt -> pt = Chain_sync.Origin) points then
      Some Chain_sync.Origin
    else None

(** Get the current tip from the store. *)
let get_tip store =
  match Store.tip store with
  | Some (slot, hash) ->
    Chain_sync.{ tip_point = Point (slot, hash);
                 tip_block_number = Int64.of_int (Store.block_count store) }
  | None ->
    Chain_sync.{ tip_point = Origin; tip_block_number = 0L }

(** Serve chain-sync to a peer. Reads from the mux, responds with blocks. *)
let handle ~store ~mux ~recv_payload =
  let tip = get_tip store in
  (* Wait for FindIntersect *)
  let* payload = recv_payload () in
  let* msg = decode_msg payload in
  (match msg with
   | MsgFindIntersect points ->
     let intersect = find_intersection store points in
     (match intersect with
      | Some pt -> send mux (MsgIntersectFound (pt, tip))
      | None -> send mux (MsgIntersectNotFound tip))
   | _ -> Error "expected MsgFindIntersect");
  |> Result.map (fun () ->
    (* Return the number of messages served *)
    1)
