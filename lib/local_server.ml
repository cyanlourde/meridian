(* Local node-to-client protocol server.

   Serves wallet queries over a Unix domain socket. Implements the
   server side of: local handshake, local chain-sync, local state
   query, local tx submission, and local tx monitor. *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Server context                                                    *)
(* ================================================================ *)

type context = {
  store : Store.store;
  ledger : Ledger_state.t;
  genesis : Genesis.genesis_config;
}

(* ================================================================ *)
(* Mux-level send/recv for server (Responder mode)                   *)
(* ================================================================ *)

let send_msg mux ~protocol_id payload =
  Mux.send_segment mux ~protocol_id ~timestamp:0l payload

let recv_msg mux =
  Mux.recv_segment mux

(* ================================================================ *)
(* Local handshake server                                            *)
(* ================================================================ *)

let handle_handshake mux =
  let* (_hdr, payload) = recv_msg mux in
  let* msg = Handshake.of_bytes payload in
  (* Node-to-client versions 16-19 for Conway *)
  let supported = List.map (fun v ->
    (v, Handshake.default_params ~network_magic:2L)
  ) [16L; 17L; 18L; 19L] in
  let response = Handshake.negotiate ~supported msg in
  let response_bytes = Handshake.to_bytes response in
  send_msg mux ~protocol_id:Miniprotocol.handshake response_bytes

(* ================================================================ *)
(* Local state query server                                          *)
(* ================================================================ *)

let encode_utxo_result utxos =
  let entries = List.map (fun (txin, (txout : Utxo.TxOut.t)) ->
    let key = Cbor.Array [Cbor.Bytes txin.Utxo.TxIn.tx_hash;
                          Cbor.Uint (Int64.of_int txin.tx_index)] in
    let value = Cbor.Array [
      Cbor.Bytes txout.address;
      if Multi_asset.is_lovelace_only txout.value then
        Cbor.Uint (Multi_asset.lovelace_of txout.value)
      else
        Cbor.Array [Cbor.Uint (Multi_asset.lovelace_of txout.value); Cbor.Map []]
    ] in
    (key, value)
  ) utxos in
  Cbor.Map entries

let handle_state_query_msg ctx query_cbor =
  match query_cbor with
  (* GetCurrentEra = [0, [query_type]] or just a tagged value *)
  | Cbor.Array [Cbor.Uint 0L] ->
    (* Return current era tag *)
    let era_tag = match Store.tip ctx.store with
      | None -> 0L
      | Some (slot, _) ->
        match Store.get_block_by_slot ctx.store ~slot with
        | None -> 0L
        | Some cbor_bytes ->
          match Block_decoder.decode_block cbor_bytes with
          | Ok block -> (match block.db_era with
            | Byron -> 0L | Shelley -> 1L | Allegra -> 2L | Mary -> 3L
            | Alonzo -> 4L | Babbage -> 5L | Conway -> 6L)
          | Error _ -> 0L
    in
    Ok (Cbor.Uint era_tag)

  | Cbor.Array [Cbor.Uint 1L] ->
    (* GetEpochNo *)
    let (slot, _) = Ledger_state.tip ctx.ledger in
    let epoch = Epoch.slot_to_epoch Epoch.preview_epoch_params slot in
    Ok (Cbor.Uint epoch)

  | Cbor.Array [Cbor.Uint 2L; Cbor.Array addresses] ->
    (* GetUTxOByAddress *)
    let utxo = Ledger_state.utxo ctx.ledger in
    let results = List.concat_map (fun addr_cbor ->
      match addr_cbor with
      | Cbor.Bytes addr -> Utxo.find_by_address utxo ~address:addr
      | _ -> []
    ) addresses in
    Ok (encode_utxo_result results)

  | Cbor.Array [Cbor.Uint 3L; Cbor.Array txins] ->
    (* GetUTxOByTxIn *)
    let utxo = Ledger_state.utxo ctx.ledger in
    let inputs = List.filter_map (fun cbor ->
      match cbor with
      | Cbor.Array [Cbor.Bytes hash; Cbor.Uint idx] ->
        Some Utxo.TxIn.{ tx_hash = hash; tx_index = Int64.to_int idx }
      | _ -> None
    ) txins in
    let results = List.filter_map (fun (txin, opt) ->
      match opt with Some txout -> Some (txin, txout) | None -> None
    ) (Utxo.find_by_txins utxo inputs) in
    Ok (encode_utxo_result results)

  | Cbor.Array [Cbor.Uint 4L] ->
    (* GetProtocolParameters *)
    let p = Ledger_state.shelley_params in
    Ok (Cbor.Map [
      (Cbor.Uint 0L, Cbor.Uint p.min_fee_a);
      (Cbor.Uint 1L, Cbor.Uint p.min_fee_b);
      (Cbor.Uint 5L, Cbor.Uint p.key_deposit);
      (Cbor.Uint 6L, Cbor.Uint p.pool_deposit);
    ])

  | _ ->
    (* Unknown query — return null *)
    Ok Cbor.Null

(* ================================================================ *)
(* Client handler — process messages from one client connection      *)
(* ================================================================ *)

(** Handle one client connection. Runs until client disconnects. *)
let handle_client ctx client_fd =
  let mux = Mux.create ~fd:client_fd ~mode:Responder in
  (* Handshake *)
  (match handle_handshake mux with
   | Error _ -> ()
   | Ok () ->
     (* Main message loop *)
     let running = ref true in
     while !running do
       match recv_msg mux with
       | Error _ -> running := false
       | Ok (hdr, payload) ->
         let pid = hdr.Mux.protocol_id in
         if pid = 6 then begin
           (* Local state query *)
           match Local_state_query.of_bytes payload with
           | Error _ -> ()
           | Ok (MsgAcquire _point) ->
             let resp = Local_state_query.to_bytes MsgAcquired in
             ignore (send_msg mux ~protocol_id:6 resp)
           | Ok (MsgQuery query_cbor) ->
             let result = match handle_state_query_msg ctx query_cbor with
               | Ok r -> r | Error _ -> Cbor.Null in
             let resp = Local_state_query.to_bytes (MsgResult result) in
             ignore (send_msg mux ~protocol_id:6 resp)
           | Ok MsgRelease ->
             ()  (* stay in loop *)
           | Ok MsgDone ->
             running := false
           | Ok _ -> ()
         end else if pid = 7 then begin
           (* Local tx submission *)
           match Local_tx_submission.of_bytes payload with
           | Error _ -> ()
           | Ok (MsgSubmitTx tx_bytes) ->
             (* Try to decode and validate *)
             let resp = match Cbor.decode tx_bytes with
               | Error e ->
                 Local_tx_submission.to_bytes (MsgRejectTx (Cbor.Text e))
               | Ok tx_cbor ->
                 match Tx_decoder.decode_transaction ~era:Babbage tx_cbor with
                 | Error e ->
                   Local_tx_submission.to_bytes (MsgRejectTx (Cbor.Text e))
                 | Ok tx ->
                   let utxo = Ledger_state.utxo ctx.ledger in
                   let (slot, _) = Ledger_state.tip ctx.ledger in
                   let errors = Utxo.validate_tx ~utxo ~current_slot:slot tx in
                   if errors = [] then
                     Local_tx_submission.to_bytes MsgAcceptTx
                   else
                     let err_msg = String.concat "; "
                       (List.map Utxo.error_to_string errors) in
                     Local_tx_submission.to_bytes (MsgRejectTx (Cbor.Text err_msg))
             in
             ignore (send_msg mux ~protocol_id:7 resp)
           | Ok MsgDone -> running := false
           | Ok _ -> ()
         end else if pid = 9 then begin
           (* Local tx monitor — empty mempool *)
           match Local_tx_monitor.of_bytes payload with
           | Error _ -> ()
           | Ok MsgAcquire ->
             let (slot, _) = Ledger_state.tip ctx.ledger in
             ignore (send_msg mux ~protocol_id:9
               (Local_tx_monitor.to_bytes (MsgAcquired slot)))
           | Ok MsgNextTx ->
             ignore (send_msg mux ~protocol_id:9
               (Local_tx_monitor.to_bytes (MsgReplyNextTx None)))
           | Ok (MsgHasTx _) ->
             ignore (send_msg mux ~protocol_id:9
               (Local_tx_monitor.to_bytes (MsgReplyHasTx false)))
           | Ok MsgGetSizes ->
             ignore (send_msg mux ~protocol_id:9
               (Local_tx_monitor.to_bytes
                  (MsgReplyGetSizes { capacity = 0L; size = 0L; num_txs = 0 })))
           | Ok MsgRelease -> ()
           | Ok MsgDone -> running := false
           | Ok _ -> ()
         end else
           ()  (* ignore other protocols *)
     done);
  (try Unix.close client_fd with _ -> ())
