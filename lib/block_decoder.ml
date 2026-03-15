(* Era-aware block deserialization from raw CBOR bytes.

   Blocks arrive from chain-sync/block-fetch as CBOR bytes. The outer
   structure for Shelley+ eras is [era_tag, block_cbor] where era_tag
   identifies which era's CDDL to use. Real nodes wrap this in
   tag 24 (CBOR-in-CBOR).

   Era tags: 0=Byron, 1=Shelley, 2=Allegra, 3=Mary,
             4=Alonzo, 5=Babbage, 6=Conway *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Types                                                             *)
(* ================================================================ *)

type era =
  | Byron | Shelley | Allegra | Mary | Alonzo | Babbage | Conway

type block_header = {
  bh_slot : int64;
  bh_block_number : int64;
  bh_prev_hash : bytes option;
  bh_issuer_vkey : bytes;
  bh_body_hash : bytes;
  bh_protocol_version : int64 * int64;
  bh_era : era;
}

type decoded_block = {
  db_era : era;
  db_header : block_header;
  db_tx_count : int;
  db_tx_raw : Cbor.cbor_value list;
  (** Raw CBOR for each transaction body in the block *)
  db_raw_cbor : Cbor.cbor_value;
  (** The full decoded CBOR of the inner block *)
}

let era_of_tag = function
  | 0L -> Ok Byron | 1L -> Ok Shelley | 2L -> Ok Allegra | 3L -> Ok Mary
  | 4L -> Ok Alonzo | 5L -> Ok Babbage | 6L -> Ok Conway
  | n -> Error (Printf.sprintf "unknown era tag %Ld" n)

let era_name = function
  | Byron -> "byron" | Shelley -> "shelley" | Allegra -> "allegra"
  | Mary -> "mary" | Alonzo -> "alonzo" | Babbage -> "babbage"
  | Conway -> "conway"

(* ================================================================ *)
(* Header extraction                                                 *)
(* ================================================================ *)

(** Extract header fields from a Shelley-era block (eras 1-4).
    Block structure: [[header_body, sig], [tx_bodies], [witnesses], {metadata}]
    Header body: [block_no, slot, prev_hash, issuer, vrf_vkey,
                  nonce_vrf, leader_vrf, body_size, body_hash,
                  hot_vkey, seq, kes_period, sigma, proto_major, proto_minor] *)
let decode_shelley_header era block_cbor =
  match block_cbor with
  | Cbor.Array (Cbor.Array [Cbor.Array header_body; _sig] :: _) ->
    (match header_body with
     | Cbor.Uint block_no :: Cbor.Uint slot :: prev_hash_cbor :: Cbor.Bytes issuer :: rest ->
       let prev_hash = match prev_hash_cbor with
         | Cbor.Bytes h -> Some h | _ -> None in
       let body_hash = match rest with
         | _ :: _ :: _ :: _ :: Cbor.Bytes h :: _ -> h
         | _ -> Bytes.empty in
       let proto = match List.rev rest with
         | Cbor.Uint minor :: Cbor.Uint major :: _ -> (major, minor)
         | _ -> (0L, 0L) in
       Ok { bh_slot = slot; bh_block_number = block_no;
            bh_prev_hash = prev_hash; bh_issuer_vkey = issuer;
            bh_body_hash = body_hash; bh_protocol_version = proto;
            bh_era = era }
     | _ -> Error "shelley header: unexpected header body structure")
  | _ -> Error "shelley block: expected [[header, sig], ...]"

(** Extract header fields from a Babbage/Conway-era block.
    Header body has 14 elements (single VRF result). *)
let decode_babbage_header era block_cbor =
  match block_cbor with
  | Cbor.Array (Cbor.Array [Cbor.Array header_body; _sig] :: _) ->
    (match header_body with
     | Cbor.Uint block_no :: Cbor.Uint slot :: prev_hash_cbor :: Cbor.Bytes issuer :: rest ->
       let prev_hash = match prev_hash_cbor with
         | Cbor.Bytes h -> Some h | _ -> None in
       let body_hash = match rest with
         | _ :: _ :: _ :: Cbor.Bytes h :: _ -> h
         | _ -> Bytes.empty in
       let proto = match List.rev rest with
         | Cbor.Uint minor :: Cbor.Uint major :: _ -> (major, minor)
         | _ -> (0L, 0L) in
       Ok { bh_slot = slot; bh_block_number = block_no;
            bh_prev_hash = prev_hash; bh_issuer_vkey = issuer;
            bh_body_hash = body_hash; bh_protocol_version = proto;
            bh_era = era }
     | _ -> Error "babbage header: unexpected structure")
  | _ -> Error "babbage block: expected [[header, sig], ...]"

(** Extract header from Byron block.
    Structure: [header, body, extra]
    Header: [protocol_magic, prev_block, body_proof, consensus_data, extra_data]
    Consensus: [[epoch, slot], pubkey, [difficulty], signature] *)
let decode_byron_header block_cbor =
  match block_cbor with
  | Cbor.Array (header_cbor :: _) ->
    (match header_cbor with
     | Cbor.Array [_magic; Cbor.Bytes prev_hash;
                   _proof; consensus; _extra] ->
       (match consensus with
        | Cbor.Array (Cbor.Array [_epoch; Cbor.Uint slot] ::
                      Cbor.Bytes issuer :: _) ->
          Ok { bh_slot = slot; bh_block_number = 0L;
               bh_prev_hash = Some prev_hash;
               bh_issuer_vkey = issuer;
               bh_body_hash = Bytes.empty;
               bh_protocol_version = (0L, 0L);
               bh_era = Byron }
        | _ ->
          (* Byron EBB or other format — return minimal header *)
          Ok { bh_slot = 0L; bh_block_number = 0L;
               bh_prev_hash = None; bh_issuer_vkey = Bytes.empty;
               bh_body_hash = Bytes.empty;
               bh_protocol_version = (0L, 0L); bh_era = Byron })
     | _ ->
       Ok { bh_slot = 0L; bh_block_number = 0L;
            bh_prev_hash = None; bh_issuer_vkey = Bytes.empty;
            bh_body_hash = Bytes.empty;
            bh_protocol_version = (0L, 0L); bh_era = Byron })
  | _ -> Error "byron block: expected array"

(* ================================================================ *)
(* Transaction extraction                                            *)
(* ================================================================ *)

(** Extract raw tx CBOR values from a Shelley+ block.
    Block: [header, [tx_bodies...], [witnesses...], metadata] *)
let extract_tx_bodies block_cbor =
  match block_cbor with
  | Cbor.Array (_ :: Cbor.Array tx_bodies :: _) -> tx_bodies
  | _ -> []

(** Extract transactions from Byron block body.
    Body: [tx_payload, ssc, dlg, upd]
    tx_payload is complex CBOR-in-CBOR; return empty for now *)
let extract_byron_txs _block_cbor = []

(* ================================================================ *)
(* Main decoder                                                      *)
(* ================================================================ *)

(** Decode a block from raw CBOR bytes.
    Handles both era-tagged wrapping [era, block] and tag 24 CBOR-in-CBOR. *)
let decode_block cbor_bytes =
  let* outer = Cbor.decode cbor_bytes in
  (* Unwrap tag 24 if present *)
  let unwrapped = match outer with
    | Cbor.Tag (24L, Cbor.Bytes inner) ->
      (match Cbor.decode inner with
       | Ok v -> v | Error _ -> outer)
    | _ -> outer
  in
  (* Determine era from outer structure *)
  match unwrapped with
  | Cbor.Array [Cbor.Uint era_tag; block_inner] ->
    (* Era-tagged: [era, block_cbor] or [era, #6.24(block_bytes)] *)
    let* era = era_of_tag era_tag in
    let block_cbor = match block_inner with
      | Cbor.Tag (24L, Cbor.Bytes inner_bytes) ->
        (match Cbor.decode inner_bytes with Ok v -> v | Error _ -> block_inner)
      | _ -> block_inner
    in
    let* header = match era with
      | Byron -> decode_byron_header block_cbor
      | Shelley | Allegra | Mary | Alonzo ->
        decode_shelley_header era block_cbor
      | Babbage | Conway ->
        decode_babbage_header era block_cbor
    in
    let tx_raw = match era with
      | Byron -> extract_byron_txs block_cbor
      | _ -> extract_tx_bodies block_cbor
    in
    Ok { db_era = era; db_header = header;
         db_tx_count = List.length tx_raw;
         db_tx_raw = tx_raw; db_raw_cbor = block_cbor }
  | Cbor.Array (_ :: _) ->
    (* Bare block without era tag — assume Byron *)
    let* header = decode_byron_header unwrapped in
    Ok { db_era = Byron; db_header = header;
         db_tx_count = 0; db_tx_raw = [];
         db_raw_cbor = unwrapped }
  | _ -> Error "block: expected [era, block] array"

(** Decode just the header from raw CBOR bytes. *)
let decode_block_header cbor_bytes =
  let* block = decode_block cbor_bytes in
  Ok block.db_header
