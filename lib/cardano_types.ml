(* Cardano block and transaction types for Byron and Shelley eras.

   References:
   - Byron CDDL:   cardano-ledger/eras/byron/cddl-spec/byron.cddl
   - Shelley CDDL: cardano-ledger/eras/shelley/impl/cddl-files/shelley.cddl
   - Shelley formal ledger spec (SL-D5) *)

let ( let* ) = Result.bind

(* ================================================================ *)
(* Common types                                                      *)
(* ================================================================ *)

(** 28-byte hash — Blake2b-224, used for key hashes and script hashes *)
type hash28 = bytes

(** 32-byte hash — Blake2b-256, used for block hashes, tx hashes *)
type hash32 = bytes

type block_hash = hash32
type tx_hash = hash32
type script_hash = hash28
type vkey_hash = hash28
type pool_keyhash = hash28
type genesis_hash = hash28
type genesis_delegate_hash = hash28
type vrf_keyhash = hash32
type auxiliary_data_hash = hash32

type vkey = bytes           (* 32-byte Ed25519 verification key *)
type signature = bytes      (* 64-byte Ed25519 signature *)
type vrf_vkey = bytes       (* 32-byte VRF verification key *)
type kes_vkey = bytes       (* 32-byte KES verification key *)
type kes_signature = bytes  (* KES signature *)

(** VRF certificate: [output, proof] per CDDL vrf_cert = [bytes, bytes] *)
type vrf_cert = {
  vrf_output : bytes;
  vrf_proof : bytes;
}

type slot_number = int64
type epoch_number = int64
type block_number = int64
type coin = int64

(** Rational number, encoded as #6.30([numerator, denominator]) *)
type rational = {
  numerator : int64;
  denominator : int64;
}

(** Unit interval — rational in [0, 1] *)
type unit_interval = rational

(* ================================================================ *)
(* Byron era types                                                   *)
(* ================================================================ *)

(** Byron address (CDDL: #6.24(bytes .cbor [root, attributes, type]))
    - address_root: Blake2b-224 hash of [type, spending_data, attributes]
    - address_attributes: map (may contain derivation path key 1, network magic key 2)
    - address_type: 0 = pubkey, 1 = script, 2 = redeem *)
type byron_address = {
  address_root : hash28;
  address_attributes : Cbor.cbor_value;
  address_type : int;
}

(** Byron transaction input (CDDL: [0, [tx_id, index]]) *)
type byron_tx_in = {
  byron_txin_id : tx_hash;
  byron_txin_index : int64;
}

(** Byron transaction output (CDDL: [address, amount]) *)
type byron_tx_out = {
  byron_txout_address : byron_address;
  byron_txout_amount : coin;
}

(** Byron transaction body (CDDL: [inputs, outputs, attributes]) *)
type byron_tx = {
  byron_tx_inputs : byron_tx_in list;
  byron_tx_outputs : byron_tx_out list;
  byron_tx_attributes : Cbor.cbor_value;
}

(** Byron consensus data within block header
    CDDL: [slot_id, pubkey, difficulty, block_signature]
    where slot_id = [epoch, slot_within_epoch]
    and difficulty = [chain_difficulty] *)
type byron_consensus_data = {
  byron_epoch : epoch_number;
  byron_slot_in_epoch : slot_number;
  byron_issuer : bytes;             (* extended public key, 64 bytes in Byron *)
  byron_difficulty : int64;
  byron_signature : Cbor.cbor_value; (* delegation/signature structure *)
}

(** Byron block header
    CDDL: [protocol_magic, prev_block, body_proof, consensus_data, extra_data] *)
type byron_block_header = {
  byron_protocol_magic : int64;
  byron_prev_block : block_hash;
  byron_body_proof : Cbor.cbor_value;
  byron_consensus : byron_consensus_data;
  byron_extra_data : Cbor.cbor_value;
}

(** Byron block body — payloads other than transactions kept opaque
    CDDL: [tx_payload, ssc_payload, dlg_payload, upd_payload] *)
type byron_block_body = {
  byron_tx_payload : Cbor.cbor_value;
  byron_ssc_payload : Cbor.cbor_value;
  byron_dlg_payload : Cbor.cbor_value;
  byron_upd_payload : Cbor.cbor_value;
}

(** Byron main block (CDDL: [header, body, extra]) *)
type byron_block = {
  byron_header : byron_block_header;
  byron_body : byron_block_body;
  byron_extra : Cbor.cbor_value list;
}

(* ================================================================ *)
(* Shelley era types                                                 *)
(* ================================================================ *)

(** Stake credential (CDDL: [0, addr_keyhash] / [1, script_hash]) *)
type credential =
  | Key_hash of vkey_hash
  | Script_hash of script_hash

(** Pointer for pointer addresses — slot, tx_index, cert_index
    encoded as variable-length naturals in address bytes *)
type stake_pointer = {
  slot : slot_number;
  tx_index : int64;
  cert_index : int64;
}

(** Shelley-era address variants, parsed from raw address bytes.
    Header byte layout: bits 7-4 = type, bits 3-0 = network id.
    Type 0-3: base address (payment + stake credentials)
    Type 4-5: pointer address (payment + chain pointer)
    Type 6-7: enterprise address (payment only)
    Type 8:   Byron compatibility
    Type 14-15: reward account (stake only) *)
type shelley_address =
  | Base_address of { network : int; payment : credential; stake : credential }
  | Pointer_address of { network : int; payment : credential; pointer : stake_pointer }
  | Enterprise_address of { network : int; payment : credential }
  | Reward_address of { network : int; stake : credential }
  | Byron_compat_address of bytes

(** Shelley transaction input (CDDL: [transaction_id, index]) *)
type tx_in = {
  tx_id : tx_hash;
  tx_index : int64;
}

(** Shelley transaction output (CDDL: [address, amount]) *)
type shelley_tx_out = {
  address : bytes;
  amount : coin;
}

(** Withdrawals: reward_account bytes -> coin *)
type withdrawals = (bytes * coin) list

(** Pool relay (CDDL: [0, port/null, ipv4/null, ipv6/null]
                     / [1, port/null, dns_name]
                     / [2, dns_name]) *)
type relay =
  | Single_host_addr of { port : int option; ipv4 : bytes option; ipv6 : bytes option }
  | Single_host_name of { port : int option; dns_name : string }
  | Multi_host_name of { dns_name : string }

(** Pool metadata (CDDL: [url, metadata_hash]) *)
type pool_metadata = {
  pool_url : string;
  pool_metadata_hash : hash32;
}

(** Pool registration parameters — flattened into certificate array
    CDDL: (operator, vrf_keyhash, pledge, cost, margin,
            reward_account, pool_owners, relays, pool_metadata/null) *)
type pool_params = {
  pool_operator : pool_keyhash;
  pool_vrf_keyhash : vrf_keyhash;
  pool_pledge : coin;
  pool_cost : coin;
  pool_margin : unit_interval;
  pool_reward_account : bytes;
  pool_owners : vkey_hash list;
  pool_relays : relay list;
  pool_metadata : pool_metadata option;
}

(** Shelley certificate (CDDL: [type_tag, ...params]) *)
type certificate =
  | Stake_registration of credential
  | Stake_deregistration of credential
  | Stake_delegation of { delegator : credential; pool : pool_keyhash }
  | Pool_registration of pool_params
  | Pool_retirement of { pool : pool_keyhash; epoch : epoch_number }
  | Genesis_key_delegation of {
      genesis_hash : genesis_hash;
      delegate_hash : genesis_delegate_hash;
      vrf_keyhash : vrf_keyhash;
    }
  | Move_instantaneous_rewards of Cbor.cbor_value

(** Protocol parameter update — CBOR map with optional fields keyed 0-15 *)
type protocol_param_update = {
  min_fee_a : int64 option;
  min_fee_b : int64 option;
  max_block_body_size : int64 option;
  max_tx_size : int64 option;
  max_block_header_size : int64 option;
  key_deposit : coin option;
  pool_deposit : coin option;
  max_epoch : epoch_number option;
  n_opt : int64 option;
  pool_pledge_influence : rational option;
  expansion_rate : unit_interval option;
  treasury_growth_rate : unit_interval option;
  decentralization_param : unit_interval option;
  extra_entropy : Cbor.cbor_value option;
  protocol_version : (int64 * int64) option;
  min_utxo_value : coin option;
}

(** Proposed protocol updates: genesis_hash -> param_update *)
type proposed_protocol_updates = (genesis_hash * protocol_param_update) list

(** Update payload (CDDL: [proposed_updates, epoch]) *)
type update = {
  proposed_updates : proposed_protocol_updates;
  update_epoch : epoch_number;
}

(** Shelley transaction body — CBOR map with keys 0-7
    { 0: set<tx_in>, 1: [*tx_out], 2: fee, 3: ttl,
      ?4: [*certificate], ?5: withdrawals, ?6: update, ?7: aux_data_hash } *)
type shelley_tx_body = {
  inputs : tx_in list;
  outputs : shelley_tx_out list;
  fee : coin;
  ttl : slot_number;
  certs : certificate list option;
  withdrawals : withdrawals option;
  update : update option;
  auxiliary_data_hash : auxiliary_data_hash option;
}

(** VKey witness (CDDL: [vkey, signature]) *)
type vkey_witness = {
  witness_vkey : vkey;
  witness_sig : signature;
}

(** Multisig native script (Shelley era)
    CDDL: [0, keyhash] / [1, [*script]] / [2, [*script]] / [3, n, [*script]] *)
type multisig_script =
  | Sig of vkey_hash
  | All_of of multisig_script list
  | Any_of of multisig_script list
  | N_of of int * multisig_script list

(** Shelley transaction witness set — CBOR map with optional fields
    { ?0: [*vkey_witness], ?1: [*multisig_script], ?2: [*bootstrap_witness] } *)
type shelley_tx_witness_set = {
  vkey_witnesses : vkey_witness list option;
  multisig_scripts : multisig_script list option;
  bootstrap_witnesses : Cbor.cbor_value option;
}

(** Shelley transaction: [body, witness_set, metadata/null] *)
type shelley_tx = {
  shelley_tx_body : shelley_tx_body;
  shelley_tx_witness_set : shelley_tx_witness_set;
  shelley_tx_metadata : Cbor.cbor_value option;
}

(** Operational certificate — fields inlined into header body array
    CDDL: (hot_vkey, sequence_number, kes_period, sigma) *)
type operational_cert = {
  hot_vkey : kes_vkey;
  sequence_number : int64;
  kes_period : int64;
  sigma : signature;
}

(** Protocol version: (major, minor) inlined into header body array *)
type protocol_version = {
  proto_major : int64;
  proto_minor : int64;
}

(** Shelley header body — CBOR array with 15 elements
    CDDL: [block_number, slot, prev_hash/null, issuer_vkey, vrf_vkey,
            nonce_vrf, leader_vrf, body_size, body_hash,
            hot_vkey, seq_num, kes_period, sigma,
            proto_major, proto_minor] *)
type shelley_header_body = {
  shb_block_number : block_number;
  shb_slot : slot_number;
  shb_prev_hash : block_hash option;
  shb_issuer_vkey : vkey;
  shb_vrf_vkey : vrf_vkey;
  shb_nonce_vrf : vrf_cert;
  shb_leader_vrf : vrf_cert;
  shb_body_size : int64;
  shb_body_hash : hash32;
  shb_operational_cert : operational_cert;
  shb_protocol_version : protocol_version;
}

(** Shelley header (CDDL: [header_body, body_signature]) *)
type shelley_header = {
  sh_header_body : shelley_header_body;
  sh_body_signature : kes_signature;
}

(** Shelley block
    CDDL: [header, [*tx_body], [*tx_witness_set], {*uint => metadata}] *)
type shelley_block = {
  sb_header : shelley_header;
  sb_tx_bodies : shelley_tx_body list;
  sb_tx_witness_sets : shelley_tx_witness_set list;
  sb_tx_metadata : (int * Cbor.cbor_value) list;
}

(** Era-tagged block *)
type block =
  | Byron_block of byron_block
  | Shelley_block of shelley_block

(* ================================================================ *)
(* CBOR encoding/decoding helpers                                    *)
(* ================================================================ *)

let list_map_result f items =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs ->
      match f x with
      | Ok v -> go (v :: acc) xs
      | Error _ as e -> e
  in
  go [] items

(** Look up an integer key in a CBOR map *)
let find_map_entry key pairs =
  List.find_map (fun (k, v) ->
    match k with
    | Cbor.Uint n when Int64.equal n (Int64.of_int key) -> Some v
    | _ -> None
  ) pairs

let decode_uint = function
  | Cbor.Uint n -> Ok n
  | _ -> Error "expected uint"

let decode_bytes = function
  | Cbor.Bytes b -> Ok b
  | _ -> Error "expected bytes"

let decode_hash28 = function
  | Cbor.Bytes b when Bytes.length b = 28 -> Ok b
  | Cbor.Bytes b -> Error (Printf.sprintf "expected 28 bytes, got %d" (Bytes.length b))
  | _ -> Error "expected bytes (hash28)"

let _decode_hash32 = function
  | Cbor.Bytes b when Bytes.length b = 32 -> Ok b
  | Cbor.Bytes b -> Error (Printf.sprintf "expected 32 bytes, got %d" (Bytes.length b))
  | _ -> Error "expected bytes (hash32)"

(* ================================================================ *)
(* Rational number encoding                                          *)
(* ================================================================ *)

let encode_rational r =
  Cbor.Tag (30L, Cbor.Array [Cbor.Uint r.numerator; Cbor.Uint r.denominator])

let decode_rational = function
  | Cbor.Tag (30L, Cbor.Array [Cbor.Uint n; Cbor.Uint d]) ->
    Ok { numerator = n; denominator = d }
  | _ -> Error "rational: expected #6.30([uint, uint])"

(* ================================================================ *)
(* Byron CBOR encoding/decoding                                      *)
(* ================================================================ *)

let encode_byron_address addr =
  let inner = Cbor.Array [
    Cbor.Bytes addr.address_root;
    addr.address_attributes;
    Cbor.Uint (Int64.of_int addr.address_type);
  ] in
  let inner_bytes = Cbor.encode inner in
  Cbor.Tag (24L, Cbor.Bytes inner_bytes)

let decode_byron_address = function
  | Cbor.Tag (24L, Cbor.Bytes inner_bytes) ->
    let* inner = Cbor.decode inner_bytes in
    (match inner with
     | Cbor.Array [Cbor.Bytes root; attrs; Cbor.Uint addr_type]
       when Bytes.length root = 28 ->
       Ok { address_root = root;
            address_attributes = attrs;
            address_type = Int64.to_int addr_type }
     | _ -> Error "byron_address: invalid inner structure")
  | _ -> Error "byron_address: expected #6.24(bytes)"

let encode_byron_tx_in inp =
  Cbor.Array [
    Cbor.Uint 0L;
    Cbor.Tag (24L, Cbor.Bytes (Cbor.encode
      (Cbor.Array [Cbor.Bytes inp.byron_txin_id; Cbor.Uint inp.byron_txin_index])))
  ]

let decode_byron_tx_in = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Tag (24L, Cbor.Bytes inner_bytes)] ->
    let* inner = Cbor.decode inner_bytes in
    (match inner with
     | Cbor.Array [Cbor.Bytes tx_id; Cbor.Uint idx] when Bytes.length tx_id = 32 ->
       Ok { byron_txin_id = tx_id; byron_txin_index = idx }
     | _ -> Error "byron_tx_in: invalid inner structure")
  | _ -> Error "byron_tx_in: expected [0, #6.24(bytes)]"

let encode_byron_tx_out out =
  Cbor.Array [
    encode_byron_address out.byron_txout_address;
    Cbor.Uint out.byron_txout_amount;
  ]

let decode_byron_tx_out = function
  | Cbor.Array [addr_cbor; Cbor.Uint amount] ->
    let* addr = decode_byron_address addr_cbor in
    Ok { byron_txout_address = addr; byron_txout_amount = amount }
  | _ -> Error "byron_tx_out: expected [address, uint]"

let encode_byron_tx tx =
  Cbor.Array [
    Cbor.Array (List.map encode_byron_tx_in tx.byron_tx_inputs);
    Cbor.Array (List.map encode_byron_tx_out tx.byron_tx_outputs);
    tx.byron_tx_attributes;
  ]

let decode_byron_tx = function
  | Cbor.Array [Cbor.Array inputs; Cbor.Array outputs; attrs] ->
    let* inputs = list_map_result decode_byron_tx_in inputs in
    let* outputs = list_map_result decode_byron_tx_out outputs in
    Ok { byron_tx_inputs = inputs;
         byron_tx_outputs = outputs;
         byron_tx_attributes = attrs }
  | _ -> Error "byron_tx: expected [inputs, outputs, attributes]"

let encode_byron_consensus_data cd =
  Cbor.Array [
    Cbor.Array [Cbor.Uint cd.byron_epoch; Cbor.Uint cd.byron_slot_in_epoch];
    Cbor.Bytes cd.byron_issuer;
    Cbor.Array [Cbor.Uint cd.byron_difficulty];
    cd.byron_signature;
  ]

let decode_byron_consensus_data = function
  | Cbor.Array [Cbor.Array [Cbor.Uint epoch; Cbor.Uint slot];
                Cbor.Bytes issuer;
                Cbor.Array [Cbor.Uint difficulty];
                signature] ->
    Ok { byron_epoch = epoch;
         byron_slot_in_epoch = slot;
         byron_issuer = issuer;
         byron_difficulty = difficulty;
         byron_signature = signature }
  | _ -> Error "byron_consensus_data: invalid structure"

let encode_byron_block_header hdr =
  Cbor.Array [
    Cbor.Uint hdr.byron_protocol_magic;
    Cbor.Bytes hdr.byron_prev_block;
    hdr.byron_body_proof;
    encode_byron_consensus_data hdr.byron_consensus;
    hdr.byron_extra_data;
  ]

let decode_byron_block_header = function
  | Cbor.Array [Cbor.Uint magic; Cbor.Bytes prev; proof; consensus; extra] ->
    let* consensus = decode_byron_consensus_data consensus in
    Ok { byron_protocol_magic = magic;
         byron_prev_block = prev;
         byron_body_proof = proof;
         byron_consensus = consensus;
         byron_extra_data = extra }
  | _ -> Error "byron_block_header: expected 5-element array"

let encode_byron_block_body body =
  Cbor.Array [
    body.byron_tx_payload;
    body.byron_ssc_payload;
    body.byron_dlg_payload;
    body.byron_upd_payload;
  ]

let decode_byron_block_body = function
  | Cbor.Array [tx; ssc; dlg; upd] ->
    Ok { byron_tx_payload = tx;
         byron_ssc_payload = ssc;
         byron_dlg_payload = dlg;
         byron_upd_payload = upd }
  | _ -> Error "byron_block_body: expected 4-element array"

let encode_byron_block block =
  Cbor.Array [
    encode_byron_block_header block.byron_header;
    encode_byron_block_body block.byron_body;
    Cbor.Array block.byron_extra;
  ]

let decode_byron_block = function
  | Cbor.Array [header; body; Cbor.Array extra] ->
    let* header = decode_byron_block_header header in
    let* body = decode_byron_block_body body in
    Ok { byron_header = header; byron_body = body; byron_extra = extra }
  | _ -> Error "byron_block: expected [header, body, extra]"

(* ================================================================ *)
(* Shelley address encoding (raw bytes, not CBOR)                    *)
(* ================================================================ *)

(** Encode a variable-length natural number (used in pointer addresses).
    Big-endian, 7 bits per byte, high bit = continuation. *)
let encode_var_nat buf n =
  let rec byte_count n =
    if Int64.compare n 128L < 0 then 1
    else 1 + byte_count (Int64.shift_right_logical n 7)
  in
  let count = byte_count n in
  let tmp = Bytes.create count in
  let rec fill i n =
    if i < 0 then ()
    else begin
      let b = Int64.to_int (Int64.logand n 0x7FL) in
      let b = if i < count - 1 then b lor 0x80 else b in
      Bytes.set_uint8 tmp i b;
      fill (i - 1) (Int64.shift_right_logical n 7)
    end
  in
  fill (count - 1) n;
  Buffer.add_bytes buf tmp

(** Decode a variable-length natural number from raw bytes at given position. *)
let decode_var_nat data pos =
  let len = Bytes.length data in
  let rec go pos acc =
    if pos >= len then Error "unexpected end of variable-length natural"
    else
      let b = Bytes.get_uint8 data pos in
      let acc = Int64.logor (Int64.shift_left acc 7) (Int64.of_int (b land 0x7F)) in
      if b land 0x80 = 0 then Ok (acc, pos + 1)
      else go (pos + 1) acc
  in
  go pos 0L

let credential_of_header_and_bytes header_type hash =
  if header_type = 0 then Key_hash hash
  else Script_hash hash

let encode_shelley_address addr =
  match addr with
  | Base_address { network; payment; stake } ->
    let pay_type = (match payment with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let stk_type = (match stake with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let addr_type = (pay_type * 2) lor stk_type in
    let header = (addr_type lsl 4) lor (network land 0x0F) in
    let buf = Buffer.create 57 in
    Buffer.add_uint8 buf header;
    (match payment with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    (match stake with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    Buffer.to_bytes buf
  | Pointer_address { network; payment; pointer } ->
    let pay_type = (match payment with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let addr_type = 4 + pay_type in
    let header = (addr_type lsl 4) lor (network land 0x0F) in
    let buf = Buffer.create 40 in
    Buffer.add_uint8 buf header;
    (match payment with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    encode_var_nat buf pointer.slot;
    encode_var_nat buf pointer.tx_index;
    encode_var_nat buf pointer.cert_index;
    Buffer.to_bytes buf
  | Enterprise_address { network; payment } ->
    let pay_type = (match payment with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let addr_type = 6 + pay_type in
    let header = (addr_type lsl 4) lor (network land 0x0F) in
    let buf = Buffer.create 29 in
    Buffer.add_uint8 buf header;
    (match payment with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    Buffer.to_bytes buf
  | Reward_address { network; stake } ->
    let stk_type = (match stake with Key_hash _ -> 0 | Script_hash _ -> 1) in
    let addr_type = 14 + stk_type in
    let header = (addr_type lsl 4) lor (network land 0x0F) in
    let buf = Buffer.create 29 in
    Buffer.add_uint8 buf header;
    (match stake with Key_hash h | Script_hash h -> Buffer.add_bytes buf h);
    Buffer.to_bytes buf
  | Byron_compat_address raw -> raw

let decode_shelley_address data =
  let len = Bytes.length data in
  if len < 1 then Error "address: empty"
  else
    let header = Bytes.get_uint8 data 0 in
    let addr_type = header lsr 4 in
    let network = header land 0x0F in
    match addr_type with
    | 0 | 1 | 2 | 3 when len = 57 ->
      let pay_hash = Bytes.sub data 1 28 in
      let stk_hash = Bytes.sub data 29 28 in
      let payment = credential_of_header_and_bytes (addr_type lsr 1) pay_hash in
      let stake = credential_of_header_and_bytes (addr_type land 1) stk_hash in
      Ok (Base_address { network; payment; stake })
    | 4 | 5 when len >= 30 ->
      let pay_hash = Bytes.sub data 1 28 in
      let payment = credential_of_header_and_bytes (addr_type - 4) pay_hash in
      let* slot, pos = decode_var_nat data 29 in
      let* tx_index, pos = decode_var_nat data pos in
      let* cert_index, _pos = decode_var_nat data pos in
      Ok (Pointer_address { network; payment;
                            pointer = { slot; tx_index; cert_index } })
    | 6 | 7 when len = 29 ->
      let pay_hash = Bytes.sub data 1 28 in
      let payment = credential_of_header_and_bytes (addr_type - 6) pay_hash in
      Ok (Enterprise_address { network; payment })
    | 8 ->
      Ok (Byron_compat_address data)
    | 14 | 15 when len = 29 ->
      let stk_hash = Bytes.sub data 1 28 in
      let stake = credential_of_header_and_bytes (addr_type - 14) stk_hash in
      Ok (Reward_address { network; stake })
    | _ -> Error (Printf.sprintf "address: unsupported type %d" addr_type)

(* ================================================================ *)
(* Shelley CBOR encoding/decoding                                    *)
(* ================================================================ *)

(* -- Credential -- *)

let encode_credential = function
  | Key_hash h -> Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h]
  | Script_hash h -> Cbor.Array [Cbor.Uint 1L; Cbor.Bytes h]

let decode_credential = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Key_hash h)
  | Cbor.Array [Cbor.Uint 1L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Script_hash h)
  | _ -> Error "credential: expected [0, hash28] or [1, hash28]"

(* -- Transaction input -- *)

let encode_tx_in inp =
  Cbor.Array [Cbor.Bytes inp.tx_id; Cbor.Uint inp.tx_index]

let decode_tx_in = function
  | Cbor.Array [Cbor.Bytes tx_id; Cbor.Uint idx] when Bytes.length tx_id = 32 ->
    Ok { tx_id; tx_index = idx }
  | _ -> Error "tx_in: expected [hash32, uint]"

(* -- Transaction output -- *)

let encode_shelley_tx_out out =
  Cbor.Array [Cbor.Bytes out.address; Cbor.Uint out.amount]

let decode_shelley_tx_out = function
  | Cbor.Array [Cbor.Bytes addr; Cbor.Uint amount] ->
    Ok { address = addr; amount }
  | _ -> Error "shelley_tx_out: expected [bytes, uint]"

(* -- Pool metadata -- *)

let encode_pool_metadata md =
  Cbor.Array [Cbor.Text md.pool_url; Cbor.Bytes md.pool_metadata_hash]

let decode_pool_metadata = function
  | Cbor.Array [Cbor.Text url; Cbor.Bytes hash] when Bytes.length hash = 32 ->
    Ok { pool_url = url; pool_metadata_hash = hash }
  | _ -> Error "pool_metadata: expected [text, hash32]"

(* -- Relay -- *)

let encode_relay = function
  | Single_host_addr { port; ipv4; ipv6 } ->
    Cbor.Array [
      Cbor.Uint 0L;
      (match port with Some p -> Cbor.Uint (Int64.of_int p) | None -> Cbor.Null);
      (match ipv4 with Some b -> Cbor.Bytes b | None -> Cbor.Null);
      (match ipv6 with Some b -> Cbor.Bytes b | None -> Cbor.Null);
    ]
  | Single_host_name { port; dns_name } ->
    Cbor.Array [
      Cbor.Uint 1L;
      (match port with Some p -> Cbor.Uint (Int64.of_int p) | None -> Cbor.Null);
      Cbor.Text dns_name;
    ]
  | Multi_host_name { dns_name } ->
    Cbor.Array [Cbor.Uint 2L; Cbor.Text dns_name]

let decode_relay = function
  | Cbor.Array [Cbor.Uint 0L; port; ipv4; ipv6] ->
    let port = match port with Cbor.Uint p -> Some (Int64.to_int p) | _ -> None in
    let ipv4 = match ipv4 with Cbor.Bytes b -> Some b | _ -> None in
    let ipv6 = match ipv6 with Cbor.Bytes b -> Some b | _ -> None in
    Ok (Single_host_addr { port; ipv4; ipv6 })
  | Cbor.Array [Cbor.Uint 1L; port; Cbor.Text dns] ->
    let port = match port with Cbor.Uint p -> Some (Int64.to_int p) | _ -> None in
    Ok (Single_host_name { port; dns_name = dns })
  | Cbor.Array [Cbor.Uint 2L; Cbor.Text dns] ->
    Ok (Multi_host_name { dns_name = dns })
  | _ -> Error "relay: unrecognized structure"

(* -- Pool params (flattened for certificate encoding) -- *)

let encode_pool_params_flat params =
  [ Cbor.Bytes params.pool_operator;
    Cbor.Bytes params.pool_vrf_keyhash;
    Cbor.Uint params.pool_pledge;
    Cbor.Uint params.pool_cost;
    encode_rational params.pool_margin;
    Cbor.Bytes params.pool_reward_account;
    Cbor.Array (List.map (fun h -> Cbor.Bytes h) params.pool_owners);
    Cbor.Array (List.map encode_relay params.pool_relays);
    (match params.pool_metadata with
     | None -> Cbor.Null
     | Some md -> encode_pool_metadata md) ]

let decode_pool_params_flat = function
  | [Cbor.Bytes operator; Cbor.Bytes vrf_kh; Cbor.Uint pledge; Cbor.Uint cost;
     margin_cbor; Cbor.Bytes reward_acct; Cbor.Array owners_cbor;
     Cbor.Array relays_cbor; metadata_cbor]
    when Bytes.length operator = 28 && Bytes.length vrf_kh = 32 ->
    let* margin = decode_rational margin_cbor in
    let* pool_owners = list_map_result (function
      | Cbor.Bytes h when Bytes.length h = 28 -> Ok h
      | _ -> Error "pool_params: invalid owner hash"
    ) owners_cbor in
    let* pool_relays = list_map_result decode_relay relays_cbor in
    let* pool_metadata = match metadata_cbor with
      | Cbor.Null -> Ok None
      | v -> let* md = decode_pool_metadata v in Ok (Some md)
    in
    Ok { pool_operator = operator;
         pool_vrf_keyhash = vrf_kh;
         pool_pledge = pledge;
         pool_cost = cost;
         pool_margin = margin;
         pool_reward_account = reward_acct;
         pool_owners;
         pool_relays;
         pool_metadata }
  | _ -> Error "pool_params: invalid flat structure"

(* -- Certificate -- *)

let encode_certificate = function
  | Stake_registration cred ->
    Cbor.Array [Cbor.Uint 0L; encode_credential cred]
  | Stake_deregistration cred ->
    Cbor.Array [Cbor.Uint 1L; encode_credential cred]
  | Stake_delegation { delegator; pool } ->
    Cbor.Array [Cbor.Uint 2L; encode_credential delegator; Cbor.Bytes pool]
  | Pool_registration params ->
    Cbor.Array (Cbor.Uint 3L :: encode_pool_params_flat params)
  | Pool_retirement { pool; epoch } ->
    Cbor.Array [Cbor.Uint 4L; Cbor.Bytes pool; Cbor.Uint epoch]
  | Genesis_key_delegation { genesis_hash; delegate_hash; vrf_keyhash } ->
    Cbor.Array [Cbor.Uint 5L; Cbor.Bytes genesis_hash;
                Cbor.Bytes delegate_hash; Cbor.Bytes vrf_keyhash]
  | Move_instantaneous_rewards v ->
    Cbor.Array [Cbor.Uint 6L; v]

let decode_certificate = function
  | Cbor.Array [Cbor.Uint 0L; cred] ->
    let* c = decode_credential cred in Ok (Stake_registration c)
  | Cbor.Array [Cbor.Uint 1L; cred] ->
    let* c = decode_credential cred in Ok (Stake_deregistration c)
  | Cbor.Array [Cbor.Uint 2L; cred; Cbor.Bytes pool]
    when Bytes.length pool = 28 ->
    let* c = decode_credential cred in
    Ok (Stake_delegation { delegator = c; pool })
  | Cbor.Array (Cbor.Uint 3L :: rest) when List.length rest = 9 ->
    let* params = decode_pool_params_flat rest in
    Ok (Pool_registration params)
  | Cbor.Array [Cbor.Uint 4L; Cbor.Bytes pool; Cbor.Uint epoch]
    when Bytes.length pool = 28 ->
    Ok (Pool_retirement { pool; epoch })
  | Cbor.Array [Cbor.Uint 5L; Cbor.Bytes gh; Cbor.Bytes dh; Cbor.Bytes vrf]
    when Bytes.length gh = 28 && Bytes.length dh = 28 && Bytes.length vrf = 32 ->
    Ok (Genesis_key_delegation { genesis_hash = gh; delegate_hash = dh; vrf_keyhash = vrf })
  | Cbor.Array [Cbor.Uint 6L; v] ->
    Ok (Move_instantaneous_rewards v)
  | _ -> Error "certificate: unrecognized structure"

(* -- Withdrawals -- *)

let encode_withdrawals wds =
  Cbor.Map (List.map (fun (acct, amount) ->
    (Cbor.Bytes acct, Cbor.Uint amount)) wds)

let decode_withdrawals = function
  | Cbor.Map pairs ->
    list_map_result (fun (k, v) ->
      let* acct = decode_bytes k in
      let* amount = decode_uint v in
      Ok (acct, amount)
    ) pairs
  | _ -> Error "withdrawals: expected map"

(* -- Protocol parameter update -- *)

let encode_protocol_param_update p =
  let opt_uint key v =
    match v with None -> [] | Some n -> [(Cbor.Uint (Int64.of_int key), Cbor.Uint n)]
  in
  let opt_rational key v =
    match v with None -> [] | Some r -> [(Cbor.Uint (Int64.of_int key), encode_rational r)]
  in
  let entries =
    opt_uint 0 p.min_fee_a
    @ opt_uint 1 p.min_fee_b
    @ opt_uint 2 p.max_block_body_size
    @ opt_uint 3 p.max_tx_size
    @ opt_uint 4 p.max_block_header_size
    @ opt_uint 5 p.key_deposit
    @ opt_uint 6 p.pool_deposit
    @ opt_uint 7 p.max_epoch
    @ opt_uint 8 p.n_opt
    @ opt_rational 9 p.pool_pledge_influence
    @ opt_rational 10 p.expansion_rate
    @ opt_rational 11 p.treasury_growth_rate
    @ opt_rational 12 p.decentralization_param
    @ (match p.extra_entropy with
       | None -> []
       | Some v -> [(Cbor.Uint 13L, v)])
    @ (match p.protocol_version with
       | None -> []
       | Some (maj, min) ->
         [(Cbor.Uint 14L, Cbor.Array [Cbor.Uint maj; Cbor.Uint min])])
    @ opt_uint 15 p.min_utxo_value
  in
  Cbor.Map entries

let decode_protocol_param_update = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let opt_uint key =
      match find key with
      | Some (Cbor.Uint n) -> Ok (Some n)
      | None -> Ok None
      | _ -> Error (Printf.sprintf "protocol_param_update: key %d: expected uint" key)
    in
    let opt_rational key =
      match find key with
      | Some v -> let* r = decode_rational v in Ok (Some r)
      | None -> Ok None
    in
    let* min_fee_a = opt_uint 0 in
    let* min_fee_b = opt_uint 1 in
    let* max_block_body_size = opt_uint 2 in
    let* max_tx_size = opt_uint 3 in
    let* max_block_header_size = opt_uint 4 in
    let* key_deposit = opt_uint 5 in
    let* pool_deposit = opt_uint 6 in
    let* max_epoch = opt_uint 7 in
    let* n_opt = opt_uint 8 in
    let* pool_pledge_influence = opt_rational 9 in
    let* expansion_rate = opt_rational 10 in
    let* treasury_growth_rate = opt_rational 11 in
    let* decentralization_param = opt_rational 12 in
    let extra_entropy = find 13 in
    let* protocol_version = match find 14 with
      | Some (Cbor.Array [Cbor.Uint maj; Cbor.Uint min]) -> Ok (Some (maj, min))
      | None -> Ok None
      | _ -> Error "protocol_param_update: key 14: expected [uint, uint]"
    in
    let* min_utxo_value = opt_uint 15 in
    Ok { min_fee_a; min_fee_b; max_block_body_size; max_tx_size;
         max_block_header_size; key_deposit; pool_deposit; max_epoch;
         n_opt; pool_pledge_influence; expansion_rate; treasury_growth_rate;
         decentralization_param; extra_entropy; protocol_version; min_utxo_value }
  | _ -> Error "protocol_param_update: expected map"

let empty_protocol_param_update = {
  min_fee_a = None; min_fee_b = None; max_block_body_size = None;
  max_tx_size = None; max_block_header_size = None; key_deposit = None;
  pool_deposit = None; max_epoch = None; n_opt = None;
  pool_pledge_influence = None; expansion_rate = None;
  treasury_growth_rate = None; decentralization_param = None;
  extra_entropy = None; protocol_version = None; min_utxo_value = None;
}

(* -- Update -- *)

let encode_update upd =
  Cbor.Array [
    Cbor.Map (List.map (fun (hash, params) ->
      (Cbor.Bytes hash, encode_protocol_param_update params)
    ) upd.proposed_updates);
    Cbor.Uint upd.update_epoch;
  ]

let decode_update = function
  | Cbor.Array [Cbor.Map proposals; Cbor.Uint epoch] ->
    let* proposed_updates = list_map_result (fun (k, v) ->
      let* hash = decode_hash28 k in
      let* params = decode_protocol_param_update v in
      Ok (hash, params)
    ) proposals in
    Ok { proposed_updates; update_epoch = epoch }
  | _ -> Error "update: expected [map, uint]"

(* -- Shelley transaction body -- *)

let encode_shelley_tx_body body =
  let entries = [
    (Cbor.Uint 0L, Cbor.Array (List.map encode_tx_in body.inputs));
    (Cbor.Uint 1L, Cbor.Array (List.map encode_shelley_tx_out body.outputs));
    (Cbor.Uint 2L, Cbor.Uint body.fee);
    (Cbor.Uint 3L, Cbor.Uint body.ttl);
  ] in
  let entries = match body.certs with
    | None | Some [] -> entries
    | Some certs ->
      entries @ [(Cbor.Uint 4L, Cbor.Array (List.map encode_certificate certs))]
  in
  let entries = match body.withdrawals with
    | None | Some [] -> entries
    | Some wds -> entries @ [(Cbor.Uint 5L, encode_withdrawals wds)]
  in
  let entries = match body.update with
    | None -> entries
    | Some upd -> entries @ [(Cbor.Uint 6L, encode_update upd)]
  in
  let entries = match body.auxiliary_data_hash with
    | None -> entries
    | Some hash -> entries @ [(Cbor.Uint 7L, Cbor.Bytes hash)]
  in
  Cbor.Map entries

let decode_shelley_tx_body = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* inputs = match find 0 with
      | Some (Cbor.Array items) -> list_map_result decode_tx_in items
      | _ -> Error "tx_body: missing or invalid inputs (key 0)"
    in
    let* outputs = match find 1 with
      | Some (Cbor.Array items) -> list_map_result decode_shelley_tx_out items
      | _ -> Error "tx_body: missing or invalid outputs (key 1)"
    in
    let* fee = match find 2 with
      | Some (Cbor.Uint n) -> Ok n
      | _ -> Error "tx_body: missing or invalid fee (key 2)"
    in
    let* ttl = match find 3 with
      | Some (Cbor.Uint n) -> Ok n
      | _ -> Error "tx_body: missing or invalid ttl (key 3)"
    in
    let* certs = match find 4 with
      | Some (Cbor.Array items) ->
        let* cs = list_map_result decode_certificate items in Ok (Some cs)
      | None -> Ok None
      | _ -> Error "tx_body: invalid certificates (key 4)"
    in
    let* withdrawals = match find 5 with
      | Some v -> let* wds = decode_withdrawals v in Ok (Some wds)
      | None -> Ok None
    in
    let* update = match find 6 with
      | Some v -> let* u = decode_update v in Ok (Some u)
      | None -> Ok None
    in
    let* auxiliary_data_hash = match find 7 with
      | Some (Cbor.Bytes h) when Bytes.length h = 32 -> Ok (Some h)
      | None -> Ok None
      | _ -> Error "tx_body: invalid auxiliary_data_hash (key 7)"
    in
    Ok { inputs; outputs; fee; ttl; certs; withdrawals; update; auxiliary_data_hash }
  | _ -> Error "tx_body: expected map"

(* -- VKey witness -- *)

let encode_vkey_witness w =
  Cbor.Array [Cbor.Bytes w.witness_vkey; Cbor.Bytes w.witness_sig]

let decode_vkey_witness = function
  | Cbor.Array [Cbor.Bytes vk; Cbor.Bytes sig_] ->
    Ok { witness_vkey = vk; witness_sig = sig_ }
  | _ -> Error "vkey_witness: expected [bytes, bytes]"

(* -- Multisig script -- *)

let rec encode_multisig_script = function
  | Sig hash ->
    Cbor.Array [Cbor.Uint 0L; Cbor.Bytes hash]
  | All_of scripts ->
    Cbor.Array [Cbor.Uint 1L; Cbor.Array (List.map encode_multisig_script scripts)]
  | Any_of scripts ->
    Cbor.Array [Cbor.Uint 2L; Cbor.Array (List.map encode_multisig_script scripts)]
  | N_of (n, scripts) ->
    Cbor.Array [Cbor.Uint 3L; Cbor.Uint (Int64.of_int n);
                Cbor.Array (List.map encode_multisig_script scripts)]

let rec decode_multisig_script = function
  | Cbor.Array [Cbor.Uint 0L; Cbor.Bytes h] when Bytes.length h = 28 ->
    Ok (Sig h)
  | Cbor.Array [Cbor.Uint 1L; Cbor.Array scripts] ->
    let* ss = list_map_result decode_multisig_script scripts in
    Ok (All_of ss)
  | Cbor.Array [Cbor.Uint 2L; Cbor.Array scripts] ->
    let* ss = list_map_result decode_multisig_script scripts in
    Ok (Any_of ss)
  | Cbor.Array [Cbor.Uint 3L; Cbor.Uint n; Cbor.Array scripts] ->
    let* ss = list_map_result decode_multisig_script scripts in
    Ok (N_of (Int64.to_int n, ss))
  | _ -> Error "multisig_script: unrecognized structure"

(* -- Shelley transaction witness set -- *)

let encode_shelley_tx_witness_set ws =
  let entries = [] in
  let entries = match ws.vkey_witnesses with
    | None | Some [] -> entries
    | Some vkws ->
      entries @ [(Cbor.Uint 0L, Cbor.Array (List.map encode_vkey_witness vkws))]
  in
  let entries = match ws.multisig_scripts with
    | None | Some [] -> entries
    | Some scripts ->
      entries @ [(Cbor.Uint 1L, Cbor.Array (List.map encode_multisig_script scripts))]
  in
  let entries = match ws.bootstrap_witnesses with
    | None -> entries
    | Some bw -> entries @ [(Cbor.Uint 2L, bw)]
  in
  Cbor.Map entries

let decode_shelley_tx_witness_set = function
  | Cbor.Map pairs ->
    let find key = find_map_entry key pairs in
    let* vkey_witnesses = match find 0 with
      | Some (Cbor.Array items) ->
        let* ws = list_map_result decode_vkey_witness items in Ok (Some ws)
      | None -> Ok None
      | _ -> Error "tx_witness_set: invalid vkey_witnesses (key 0)"
    in
    let* multisig_scripts = match find 1 with
      | Some (Cbor.Array items) ->
        let* ss = list_map_result decode_multisig_script items in Ok (Some ss)
      | None -> Ok None
      | _ -> Error "tx_witness_set: invalid multisig_scripts (key 1)"
    in
    let bootstrap_witnesses = find 2 in
    Ok { vkey_witnesses; multisig_scripts; bootstrap_witnesses }
  | _ -> Error "tx_witness_set: expected map"

(* -- VRF cert -- *)

let encode_vrf_cert cert =
  Cbor.Array [Cbor.Bytes cert.vrf_output; Cbor.Bytes cert.vrf_proof]

let decode_vrf_cert = function
  | Cbor.Array [Cbor.Bytes output; Cbor.Bytes proof] ->
    Ok { vrf_output = output; vrf_proof = proof }
  | _ -> Error "vrf_cert: expected [bytes, bytes]"

(* -- Operational cert -- *)

let encode_operational_cert oc =
  [ Cbor.Bytes oc.hot_vkey;
    Cbor.Uint oc.sequence_number;
    Cbor.Uint oc.kes_period;
    Cbor.Bytes oc.sigma ]

let decode_operational_cert_flat = function
  | [Cbor.Bytes hot_vkey; Cbor.Uint seq; Cbor.Uint period; Cbor.Bytes sigma] ->
    Ok { hot_vkey; sequence_number = seq; kes_period = period; sigma }
  | _ -> Error "operational_cert: invalid flat structure"

(* -- Shelley header body -- *)

let encode_shelley_header_body hb =
  Cbor.Array ([
    Cbor.Uint hb.shb_block_number;
    Cbor.Uint hb.shb_slot;
    (match hb.shb_prev_hash with None -> Cbor.Null | Some h -> Cbor.Bytes h);
    Cbor.Bytes hb.shb_issuer_vkey;
    Cbor.Bytes hb.shb_vrf_vkey;
    encode_vrf_cert hb.shb_nonce_vrf;
    encode_vrf_cert hb.shb_leader_vrf;
    Cbor.Uint hb.shb_body_size;
    Cbor.Bytes hb.shb_body_hash;
  ] @ encode_operational_cert hb.shb_operational_cert @ [
    Cbor.Uint hb.shb_protocol_version.proto_major;
    Cbor.Uint hb.shb_protocol_version.proto_minor;
  ])

let decode_shelley_header_body = function
  | Cbor.Array [Cbor.Uint block_num; Cbor.Uint slot; prev_hash_cbor;
                Cbor.Bytes issuer; Cbor.Bytes vrf_vk;
                nonce_vrf_cbor; leader_vrf_cbor;
                Cbor.Uint body_size; Cbor.Bytes body_hash;
                Cbor.Bytes hot_vk; Cbor.Uint seq; Cbor.Uint period;
                Cbor.Bytes sigma;
                Cbor.Uint proto_maj; Cbor.Uint proto_min] ->
    let* prev_hash = match prev_hash_cbor with
      | Cbor.Null -> Ok None
      | Cbor.Bytes h when Bytes.length h = 32 -> Ok (Some h)
      | _ -> Error "header_body: invalid prev_hash"
    in
    let* nonce_vrf = decode_vrf_cert nonce_vrf_cbor in
    let* leader_vrf = decode_vrf_cert leader_vrf_cbor in
    Ok { shb_block_number = block_num;
         shb_slot = slot;
         shb_prev_hash = prev_hash;
         shb_issuer_vkey = issuer;
         shb_vrf_vkey = vrf_vk;
         shb_nonce_vrf = nonce_vrf;
         shb_leader_vrf = leader_vrf;
         shb_body_size = body_size;
         shb_body_hash = body_hash;
         shb_operational_cert = { hot_vkey = hot_vk; sequence_number = seq;
                                  kes_period = period; sigma };
         shb_protocol_version = { proto_major = proto_maj; proto_minor = proto_min } }
  | _ -> Error "header_body: expected 15-element array"

(* -- Shelley header -- *)

let encode_shelley_header hdr =
  Cbor.Array [
    encode_shelley_header_body hdr.sh_header_body;
    Cbor.Bytes hdr.sh_body_signature;
  ]

let decode_shelley_header = function
  | Cbor.Array [hb_cbor; Cbor.Bytes sig_] ->
    let* hb = decode_shelley_header_body hb_cbor in
    Ok { sh_header_body = hb; sh_body_signature = sig_ }
  | _ -> Error "header: expected [header_body, signature]"

(* -- Shelley transaction -- *)

let encode_shelley_tx tx =
  Cbor.Array [
    encode_shelley_tx_body tx.shelley_tx_body;
    encode_shelley_tx_witness_set tx.shelley_tx_witness_set;
    (match tx.shelley_tx_metadata with None -> Cbor.Null | Some m -> m);
  ]

let decode_shelley_tx = function
  | Cbor.Array [body_cbor; ws_cbor; meta_cbor] ->
    let* body = decode_shelley_tx_body body_cbor in
    let* ws = decode_shelley_tx_witness_set ws_cbor in
    let meta = match meta_cbor with Cbor.Null -> None | v -> Some v in
    Ok { shelley_tx_body = body;
         shelley_tx_witness_set = ws;
         shelley_tx_metadata = meta }
  | _ -> Error "shelley_tx: expected [body, witnesses, metadata/null]"

(* -- Shelley block -- *)

let encode_shelley_block block =
  Cbor.Array [
    encode_shelley_header block.sb_header;
    Cbor.Array (List.map encode_shelley_tx_body block.sb_tx_bodies);
    Cbor.Array (List.map encode_shelley_tx_witness_set block.sb_tx_witness_sets);
    Cbor.Map (List.map (fun (idx, meta) ->
      (Cbor.Uint (Int64.of_int idx), meta)
    ) block.sb_tx_metadata);
  ]

let decode_shelley_block = function
  | Cbor.Array [hdr_cbor; Cbor.Array bodies_cbor;
                Cbor.Array witnesses_cbor; Cbor.Map meta_cbor] ->
    let* header = decode_shelley_header hdr_cbor in
    let* bodies = list_map_result decode_shelley_tx_body bodies_cbor in
    let* witnesses = list_map_result decode_shelley_tx_witness_set witnesses_cbor in
    let* metadata = list_map_result (fun (k, v) ->
      let* idx = decode_uint k in
      Ok (Int64.to_int idx, v)
    ) meta_cbor in
    Ok { sb_header = header;
         sb_tx_bodies = bodies;
         sb_tx_witness_sets = witnesses;
         sb_tx_metadata = metadata }
  | _ -> Error "shelley_block: expected [header, [bodies], [witnesses], {metadata}]"
