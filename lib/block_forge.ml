(* Block construction from mempool transactions.

   Selects transactions ordered by fee density, validates against
   evolving UTXO, encodes as Babbage-era CBOR block. *)

type forged_block = {
  fb_slot : int64;
  fb_block_number : int64;
  fb_prev_hash : bytes;
  fb_issuer_vkey : bytes;
  fb_body_hash : bytes;
  fb_protocol_version : int64 * int64;
  fb_tx_count : int;
  fb_total_fees : int64;
  fb_encoded : bytes;
}

(** Select transactions from mempool that fit within block body size.
    Re-validates each tx against the evolving UTXO. *)
let select_transactions ~mempool ~utxo ~max_body_size ~current_slot =
  let all_txs = Mempool.get_all mempool in
  let selected = ref [] in
  let total_size = ref 0 in
  let total_fees = ref 0L in
  (* Track which inputs are consumed by already-selected txs *)
  let consumed = Hashtbl.create 64 in
  List.iter (fun (entry : Mempool.mempool_entry) ->
    if !total_size + entry.size_bytes <= max_body_size then begin
      (* Check no input conflicts with already-selected txs *)
      let has_conflict = List.exists (fun (inp : Tx_decoder.tx_input) ->
        Hashtbl.mem consumed (inp.ti_tx_hash, inp.ti_index)
      ) entry.decoded.dt_inputs in
      if not has_conflict then begin
        (* Re-validate against current UTXO *)
        let errors = Utxo.validate_tx ~utxo ~current_slot entry.decoded in
        let real_errors = List.filter (function
          | Utxo.Insufficient_fee _ -> false  (* skip fee check for selection *)
          | _ -> true) errors in
        if real_errors = [] then begin
          selected := entry :: !selected;
          total_size := !total_size + entry.size_bytes;
          total_fees := Int64.add !total_fees entry.fee;
          (* Mark inputs as consumed *)
          List.iter (fun (inp : Tx_decoder.tx_input) ->
            Hashtbl.replace consumed (inp.ti_tx_hash, inp.ti_index) ()
          ) entry.decoded.dt_inputs
        end
      end
    end
  ) all_txs;
  (List.rev !selected, !total_fees)

(** Encode a block body as Babbage-era CBOR:
    [tx_body_list, witness_set_list, is_valid_flags, aux_data_map] *)
let encode_body txs =
  let tx_bodies = List.map (fun (e : Mempool.mempool_entry) ->
    (* For now, use the raw CBOR as the tx body *)
    match Cbor.decode e.raw_cbor with
    | Ok cbor -> cbor
    | Error _ -> Cbor.Null
  ) txs in
  let witnesses = List.map (fun _ -> Cbor.Map []) txs in
  let is_valid = Cbor.Map [] in  (* all valid *)
  let aux_data = Cbor.Map [] in
  Cbor.Array [
    Cbor.Array tx_bodies;
    Cbor.Array witnesses;
    is_valid;
    aux_data;
  ]

(** Forge a complete block. *)
let forge_block ~slot ~block_number ~prev_hash ~issuer_vkey
    ~vrf_vkey ~vrf_output ~vrf_proof
    ~opcert ~kes_signature
    ~protocol_version ~mempool ~utxo ~max_block_body_size =
  let (txs, total_fees) = select_transactions
    ~mempool ~utxo ~max_body_size:max_block_body_size ~current_slot:slot in
  let body_cbor = encode_body txs in
  let body_bytes = Cbor.encode body_cbor in
  let body_hash = Crypto.blake2b_256 body_bytes in
  let (proto_major, proto_minor) = protocol_version in
  (* Babbage header body: 14 elements *)
  let header_body = Cbor.Array [
    Cbor.Uint block_number;                      (* 0: block_number *)
    Cbor.Uint slot;                              (* 1: slot *)
    Cbor.Bytes prev_hash;                        (* 2: prev_hash *)
    Cbor.Bytes issuer_vkey;                      (* 3: issuer_vkey *)
    Cbor.Bytes vrf_vkey;                         (* 4: vrf_vkey *)
    Cbor.Array [Cbor.Bytes vrf_output; Cbor.Bytes vrf_proof]; (* 5: vrf_result *)
    Cbor.Uint (Int64.of_int (Bytes.length body_bytes)); (* 6: body_size *)
    Cbor.Bytes body_hash;                        (* 7: body_hash *)
    Cbor.Bytes opcert.Block_decoder.oc_hot_vkey; (* 8: hot_vkey *)
    Cbor.Uint opcert.oc_sequence_number;         (* 9: seq_num *)
    Cbor.Uint opcert.oc_kes_period;              (* 10: kes_period *)
    Cbor.Bytes opcert.oc_cold_signature;         (* 11: cold_sig *)
    Cbor.Uint proto_major;                       (* 12: proto_major *)
    Cbor.Uint proto_minor;                       (* 13: proto_minor *)
  ] in
  let header = Cbor.Array [header_body; Cbor.Bytes kes_signature] in
  let block = Cbor.Array [
    header;
    Cbor.Array (List.map (fun (e : Mempool.mempool_entry) ->
      match Cbor.decode e.raw_cbor with Ok c -> c | Error _ -> Cbor.Null
    ) txs);
    Cbor.Array (List.map (fun _ -> Cbor.Map []) txs);
    Cbor.Map [];
  ] in
  let era_tagged = Cbor.Array [Cbor.Uint 5L; block] in  (* 5 = Babbage *)
  let encoded = Cbor.encode era_tagged in
  { fb_slot = slot; fb_block_number = block_number;
    fb_prev_hash = prev_hash; fb_issuer_vkey = issuer_vkey;
    fb_body_hash = body_hash; fb_protocol_version = protocol_version;
    fb_tx_count = List.length txs;
    fb_total_fees = total_fees; fb_encoded = encoded }
