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

(** Operational certificate extracted from block header.
    Per Shelley spec OCERT rule: cold key signs (hot_vkey || seq || kes_period). *)
type opcert = {
  oc_hot_vkey : bytes;       (** 32-byte KES verification key *)
  oc_sequence_number : int64;
  oc_kes_period : int64;
  oc_cold_signature : bytes; (** Ed25519 signature by cold (pool) key *)
}

type block_header = {
  bh_slot : int64;
  bh_block_number : int64;
  bh_prev_hash : bytes option;
  bh_issuer_vkey : bytes;       (** 32-byte Ed25519 cold/issuer key *)
  bh_body_hash : bytes;
  bh_protocol_version : int64 * int64;
  bh_era : era;
  bh_vrf_vkey : bytes;          (** 32-byte VRF verification key *)
  bh_block_signature : bytes;   (** KES signature over header body *)
  bh_opcert : opcert option;    (** Operational certificate (Shelley+) *)
  bh_header_body_cbor : bytes;  (** Raw serialized header body (signed data) *)
}

type decoded_block = {
  db_era : era;
  db_header : block_header;
  db_tx_count : int;
  db_tx_raw : Cbor.cbor_value list;
  db_raw_cbor : Cbor.cbor_value;
}

let era_of_tag = function
  | 0L -> Ok Byron | 1L -> Ok Shelley | 2L -> Ok Allegra | 3L -> Ok Mary
  | 4L -> Ok Alonzo | 5L -> Ok Babbage | 6L -> Ok Conway
  | n -> Error (Printf.sprintf "unknown era tag %Ld" n)

let era_name = function
  | Byron -> "byron" | Shelley -> "shelley" | Allegra -> "allegra"
  | Mary -> "mary" | Alonzo -> "alonzo" | Babbage -> "babbage"
  | Conway -> "conway"

let empty_header era = {
  bh_slot = 0L; bh_block_number = 0L; bh_prev_hash = None;
  bh_issuer_vkey = Bytes.empty; bh_body_hash = Bytes.empty;
  bh_protocol_version = (0L, 0L); bh_era = era;
  bh_vrf_vkey = Bytes.empty; bh_block_signature = Bytes.empty;
  bh_opcert = None; bh_header_body_cbor = Bytes.empty;
}

(* ================================================================ *)
(* Opcert extraction                                                 *)
(* ================================================================ *)

(** Extract opcert from a Shelley header body array.
    Shelley: indices 9=hot_vkey, 10=seq, 11=kes_period, 12=cold_sig
    (after block_no, slot, prev_hash, issuer, vrf_vkey,
     nonce_vrf, leader_vrf, body_size, body_hash) *)
let extract_opcert_shelley header_body =
  let arr = Array.of_list header_body in
  let len = Array.length arr in
  if len >= 13 then
    match arr.(9), arr.(10), arr.(11), arr.(12) with
    | Cbor.Bytes hot_vk, Cbor.Uint seq, Cbor.Uint period, Cbor.Bytes cold_sig ->
      Some { oc_hot_vkey = hot_vk; oc_sequence_number = seq;
             oc_kes_period = period; oc_cold_signature = cold_sig }
    | _ -> None
  else None

(** Extract opcert from Babbage header body (14 elements).
    Babbage: indices 8=hot_vkey, 9=seq, 10=kes_period, 11=cold_sig
    (after block_no, slot, prev_hash, issuer, vrf_vkey,
     vrf_result, body_size, body_hash) *)
let extract_opcert_babbage header_body =
  let arr = Array.of_list header_body in
  let len = Array.length arr in
  if len >= 12 then
    match arr.(8), arr.(9), arr.(10), arr.(11) with
    | Cbor.Bytes hot_vk, Cbor.Uint seq, Cbor.Uint period, Cbor.Bytes cold_sig ->
      Some { oc_hot_vkey = hot_vk; oc_sequence_number = seq;
             oc_kes_period = period; oc_cold_signature = cold_sig }
    | _ -> None
  else None

(* ================================================================ *)
(* Header extraction                                                 *)
(* ================================================================ *)

let decode_shelley_header era block_cbor =
  match block_cbor with
  | Cbor.Array (Cbor.Array [Cbor.Array header_body; sig_cbor] :: _) ->
    let header_body_cbor = Cbor.encode (Cbor.Array header_body) in
    let block_sig = match sig_cbor with
      | Cbor.Bytes s -> s | _ -> Bytes.empty in
    (match header_body with
     | Cbor.Uint block_no :: Cbor.Uint slot :: prev_hash_cbor
       :: Cbor.Bytes issuer :: rest ->
       let prev_hash = match prev_hash_cbor with
         | Cbor.Bytes h -> Some h | _ -> None in
       let vrf_vkey = match rest with
         | Cbor.Bytes vk :: _ -> vk | _ -> Bytes.empty in
       let body_hash = match rest with
         | _ :: _ :: _ :: _ :: Cbor.Bytes h :: _ -> h
         | _ -> Bytes.empty in
       let proto = match List.rev rest with
         | Cbor.Uint minor :: Cbor.Uint major :: _ -> (major, minor)
         | _ -> (0L, 0L) in
       let opcert = extract_opcert_shelley header_body in
       Ok { bh_slot = slot; bh_block_number = block_no;
            bh_prev_hash = prev_hash; bh_issuer_vkey = issuer;
            bh_body_hash = body_hash; bh_protocol_version = proto;
            bh_era = era; bh_vrf_vkey = vrf_vkey;
            bh_block_signature = block_sig; bh_opcert = opcert;
            bh_header_body_cbor = header_body_cbor }
     | _ -> Error "shelley header: unexpected header body structure")
  | _ -> Error "shelley block: expected [[header, sig], ...]"

let decode_babbage_header era block_cbor =
  match block_cbor with
  | Cbor.Array (Cbor.Array [Cbor.Array header_body; sig_cbor] :: _) ->
    let header_body_cbor = Cbor.encode (Cbor.Array header_body) in
    let block_sig = match sig_cbor with
      | Cbor.Bytes s -> s | _ -> Bytes.empty in
    (match header_body with
     | Cbor.Uint block_no :: Cbor.Uint slot :: prev_hash_cbor
       :: Cbor.Bytes issuer :: rest ->
       let prev_hash = match prev_hash_cbor with
         | Cbor.Bytes h -> Some h | _ -> None in
       let vrf_vkey = match rest with
         | Cbor.Bytes vk :: _ -> vk | _ -> Bytes.empty in
       let body_hash = match rest with
         | _ :: _ :: _ :: Cbor.Bytes h :: _ -> h
         | _ -> Bytes.empty in
       let proto = match List.rev rest with
         | Cbor.Uint minor :: Cbor.Uint major :: _ -> (major, minor)
         | _ -> (0L, 0L) in
       let opcert = extract_opcert_babbage header_body in
       Ok { bh_slot = slot; bh_block_number = block_no;
            bh_prev_hash = prev_hash; bh_issuer_vkey = issuer;
            bh_body_hash = body_hash; bh_protocol_version = proto;
            bh_era = era; bh_vrf_vkey = vrf_vkey;
            bh_block_signature = block_sig; bh_opcert = opcert;
            bh_header_body_cbor = header_body_cbor }
     | _ -> Error "babbage header: unexpected structure")
  | _ -> Error "babbage block: expected [[header, sig], ...]"

let decode_byron_header block_cbor =
  match block_cbor with
  | Cbor.Array (header_cbor :: _) ->
    (match header_cbor with
     | Cbor.Array [_magic; Cbor.Bytes prev_hash;
                   _proof; consensus; _extra] ->
       (match consensus with
        | Cbor.Array (Cbor.Array [_epoch; Cbor.Uint slot] ::
                      Cbor.Bytes issuer :: _) ->
          Ok { (empty_header Byron) with
               bh_slot = slot; bh_prev_hash = Some prev_hash;
               bh_issuer_vkey = issuer }
        | _ -> Ok (empty_header Byron))
     | _ -> Ok (empty_header Byron))
  | _ -> Error "byron block: expected array"

(* ================================================================ *)
(* Transaction extraction                                            *)
(* ================================================================ *)

let extract_tx_bodies block_cbor =
  match block_cbor with
  | Cbor.Array (_ :: Cbor.Array tx_bodies :: _) -> tx_bodies
  | _ -> []

let extract_byron_txs _block_cbor = []

(* ================================================================ *)
(* Main decoder                                                      *)
(* ================================================================ *)

let decode_block cbor_bytes =
  let* outer = Cbor.decode cbor_bytes in
  let unwrapped = match outer with
    | Cbor.Tag (24L, Cbor.Bytes inner) ->
      (match Cbor.decode inner with Ok v -> v | Error _ -> outer)
    | _ -> outer
  in
  match unwrapped with
  | Cbor.Array [Cbor.Uint era_tag; block_inner] ->
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
    let* header = decode_byron_header unwrapped in
    Ok { db_era = Byron; db_header = header;
         db_tx_count = 0; db_tx_raw = [];
         db_raw_cbor = unwrapped }
  | _ -> Error "block: expected [era, block] array"

let decode_block_header cbor_bytes =
  let* block = decode_block cbor_bytes in
  Ok block.db_header
