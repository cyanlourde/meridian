open Meridian

(* ================================================================ *)
(* Test helpers                                                      *)
(* ================================================================ *)

let hex_of_bytes b =
  let buf = Buffer.create (Bytes.length b * 2) in
  Bytes.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    b;
  Buffer.contents buf

let bytes_testable =
  Alcotest.testable
    (fun fmt b -> Format.fprintf fmt "%s" (hex_of_bytes b))
    Bytes.equal

(** Round-trip test: encode to CBOR -> serialize -> deserialize -> decode -> re-encode -> compare *)
let check_cbor_roundtrip name encode decode value =
  let cbor1 = encode value in
  let raw1 = Cbor.encode cbor1 in
  match Cbor.decode raw1 with
  | Error e -> Alcotest.fail (Printf.sprintf "CBOR decode failed: %s" e)
  | Ok cbor2 ->
    match decode cbor2 with
    | Error e -> Alcotest.fail (Printf.sprintf "type decode failed: %s" e)
    | Ok value' ->
      let cbor3 = encode value' in
      let raw2 = Cbor.encode cbor3 in
      Alcotest.check bytes_testable name raw1 raw2

(** Round-trip test for raw-bytes encoding (addresses) *)
let check_bytes_roundtrip name encode decode value =
  let raw1 = encode value in
  match decode raw1 with
  | Error e -> Alcotest.fail (Printf.sprintf "decode failed: %s" e)
  | Ok value' ->
    let raw2 = encode value' in
    Alcotest.check bytes_testable name raw1 raw2

(* Dummy hashes for testing *)
let hash28 () = Bytes.make 28 '\xab'
let hash32 () = Bytes.make 32 '\xcd'
let hash28_alt () = Bytes.make 28 '\x01'
let hash32_alt () = Bytes.make 32 '\x02'

(* ================================================================ *)
(* Byron tests                                                       *)
(* ================================================================ *)

let test_byron_address () =
  let addr = Cardano_types.{
    address_root = hash28 ();
    address_attributes = Cbor.Map [];
    address_type = 0;
  } in
  check_cbor_roundtrip "byron address"
    Cardano_types.encode_byron_address
    Cardano_types.decode_byron_address
    addr

let test_byron_tx_in () =
  let inp = Cardano_types.{
    byron_txin_id = hash32 ();
    byron_txin_index = 0L;
  } in
  check_cbor_roundtrip "byron tx_in"
    Cardano_types.encode_byron_tx_in
    Cardano_types.decode_byron_tx_in
    inp

let test_byron_tx () =
  let addr = Cardano_types.{
    address_root = hash28 ();
    address_attributes = Cbor.Map [];
    address_type = 0;
  } in
  let tx = Cardano_types.{
    byron_tx_inputs = [
      { byron_txin_id = hash32 (); byron_txin_index = 0L };
      { byron_txin_id = hash32_alt (); byron_txin_index = 1L };
    ];
    byron_tx_outputs = [
      { byron_txout_address = addr; byron_txout_amount = 1000000L };
    ];
    byron_tx_attributes = Cbor.Map [];
  } in
  check_cbor_roundtrip "byron tx"
    Cardano_types.encode_byron_tx
    Cardano_types.decode_byron_tx
    tx

let test_byron_block_header () =
  let hdr = Cardano_types.{
    byron_protocol_magic = 764824073L;
    byron_prev_block = hash32 ();
    byron_body_proof = Cbor.Bytes (hash32 ());
    byron_consensus = {
      byron_epoch = 0L;
      byron_slot_in_epoch = 42L;
      byron_issuer = Bytes.make 64 '\xee';
      byron_difficulty = 1L;
      byron_signature = Cbor.Array [Cbor.Uint 0L; Cbor.Bytes (Bytes.make 64 '\xff')];
    };
    byron_extra_data = Cbor.Array [
      Cbor.Array [Cbor.Uint 1L; Cbor.Uint 0L; Cbor.Uint 0L];
      Cbor.Array [Cbor.Text "meridian"; Cbor.Uint 0L];
      Cbor.Map [];
      Cbor.Bytes (hash32 ());
    ];
  } in
  check_cbor_roundtrip "byron block header"
    Cardano_types.encode_byron_block_header
    Cardano_types.decode_byron_block_header
    hdr

(* ================================================================ *)
(* Shelley address tests                                             *)
(* ================================================================ *)

let test_base_address () =
  let addr = Cardano_types.Base_address {
    network = 1;
    payment = Key_hash (hash28 ());
    stake = Key_hash (hash28_alt ());
  } in
  check_bytes_roundtrip "base address (key/key)"
    Cardano_types.encode_shelley_address
    Cardano_types.decode_shelley_address
    addr

let test_base_address_script () =
  let addr = Cardano_types.Base_address {
    network = 0;
    payment = Script_hash (hash28 ());
    stake = Key_hash (hash28_alt ());
  } in
  check_bytes_roundtrip "base address (script/key)"
    Cardano_types.encode_shelley_address
    Cardano_types.decode_shelley_address
    addr

let test_enterprise_address () =
  let addr = Cardano_types.Enterprise_address {
    network = 1;
    payment = Key_hash (hash28 ());
  } in
  check_bytes_roundtrip "enterprise address"
    Cardano_types.encode_shelley_address
    Cardano_types.decode_shelley_address
    addr

let test_reward_address () =
  let addr = Cardano_types.Reward_address {
    network = 1;
    stake = Key_hash (hash28 ());
  } in
  check_bytes_roundtrip "reward address"
    Cardano_types.encode_shelley_address
    Cardano_types.decode_shelley_address
    addr

let test_pointer_address () =
  let addr = Cardano_types.Pointer_address {
    network = 1;
    payment = Key_hash (hash28 ());
    pointer = { slot = 300L; tx_index = 2L; cert_index = 0L };
  } in
  check_bytes_roundtrip "pointer address"
    Cardano_types.encode_shelley_address
    Cardano_types.decode_shelley_address
    addr

(* ================================================================ *)
(* Shelley type tests                                                *)
(* ================================================================ *)

let test_credential () =
  check_cbor_roundtrip "key_hash credential"
    Cardano_types.encode_credential
    Cardano_types.decode_credential
    (Key_hash (hash28 ()));
  check_cbor_roundtrip "script_hash credential"
    Cardano_types.encode_credential
    Cardano_types.decode_credential
    (Script_hash (hash28 ()))

let test_tx_in () =
  let inp = Cardano_types.{ tx_id = hash32 (); tx_index = 3L } in
  check_cbor_roundtrip "tx_in"
    Cardano_types.encode_tx_in
    Cardano_types.decode_tx_in
    inp

let test_shelley_tx_out () =
  let out = Cardano_types.{
    address = Cardano_types.encode_shelley_address
      (Enterprise_address { network = 1; payment = Key_hash (hash28 ()) });
    amount = 2000000L;
  } in
  check_cbor_roundtrip "shelley tx_out"
    Cardano_types.encode_shelley_tx_out
    Cardano_types.decode_shelley_tx_out
    out

let test_rational () =
  let r = Cardano_types.{ numerator = 1L; denominator = 2L } in
  check_cbor_roundtrip "rational"
    Cardano_types.encode_rational
    Cardano_types.decode_rational
    r

let test_vrf_cert () =
  let cert = Cardano_types.{
    vrf_output = Bytes.make 32 '\xaa';
    vrf_proof = Bytes.make 80 '\xbb';
  } in
  check_cbor_roundtrip "vrf_cert"
    Cardano_types.encode_vrf_cert
    Cardano_types.decode_vrf_cert
    cert

let test_vkey_witness () =
  let w = Cardano_types.{
    witness_vkey = Bytes.make 32 '\x11';
    witness_sig = Bytes.make 64 '\x22';
  } in
  check_cbor_roundtrip "vkey_witness"
    Cardano_types.encode_vkey_witness
    Cardano_types.decode_vkey_witness
    w

let test_multisig_script () =
  let script = Cardano_types.All_of [
    Sig (hash28 ());
    Any_of [Sig (hash28_alt ())];
    N_of (1, [Sig (hash28 ()); Sig (hash28_alt ())]);
  ] in
  check_cbor_roundtrip "multisig_script"
    Cardano_types.encode_multisig_script
    Cardano_types.decode_multisig_script
    script

(* ================================================================ *)
(* Certificate tests                                                 *)
(* ================================================================ *)

let test_cert_stake_registration () =
  let cert = Cardano_types.Stake_registration (Key_hash (hash28 ())) in
  check_cbor_roundtrip "stake registration"
    Cardano_types.encode_certificate
    Cardano_types.decode_certificate
    cert

let test_cert_stake_delegation () =
  let cert = Cardano_types.Stake_delegation {
    delegator = Key_hash (hash28 ());
    pool = hash28_alt ();
  } in
  check_cbor_roundtrip "stake delegation"
    Cardano_types.encode_certificate
    Cardano_types.decode_certificate
    cert

let test_cert_pool_registration () =
  let params = Cardano_types.{
    pool_operator = hash28 ();
    pool_vrf_keyhash = hash32 ();
    pool_pledge = 50000000000L;
    pool_cost = 340000000L;
    pool_margin = { numerator = 1L; denominator = 100L };
    pool_reward_account = Cardano_types.encode_shelley_address
      (Reward_address { network = 1; stake = Key_hash (hash28 ()) });
    pool_owners = [hash28 ()];
    pool_relays = [
      Single_host_name { port = Some 3001; dns_name = "relay.example.com" };
    ];
    pool_metadata = Some {
      pool_url = "https://example.com/pool.json";
      pool_metadata_hash = hash32 ();
    };
  } in
  let cert = Cardano_types.Pool_registration params in
  check_cbor_roundtrip "pool registration"
    Cardano_types.encode_certificate
    Cardano_types.decode_certificate
    cert

let test_cert_pool_retirement () =
  let cert = Cardano_types.Pool_retirement {
    pool = hash28 ();
    epoch = 300L;
  } in
  check_cbor_roundtrip "pool retirement"
    Cardano_types.encode_certificate
    Cardano_types.decode_certificate
    cert

let test_cert_genesis_delegation () =
  let cert = Cardano_types.Genesis_key_delegation {
    genesis_hash = hash28 ();
    delegate_hash = hash28_alt ();
    vrf_keyhash = hash32 ();
  } in
  check_cbor_roundtrip "genesis delegation"
    Cardano_types.encode_certificate
    Cardano_types.decode_certificate
    cert

(* ================================================================ *)
(* Transaction body tests                                            *)
(* ================================================================ *)

let test_shelley_tx_body_minimal () =
  let body = Cardano_types.{
    inputs = [{ tx_id = hash32 (); tx_index = 0L }];
    outputs = [{
      address = encode_shelley_address
        (Enterprise_address { network = 1; payment = Key_hash (hash28 ()) });
      amount = 1500000L;
    }];
    fee = 200000L;
    ttl = 50000000L;
    certs = None;
    withdrawals = None;
    update = None;
    auxiliary_data_hash = None;
  } in
  check_cbor_roundtrip "shelley tx body (minimal)"
    Cardano_types.encode_shelley_tx_body
    Cardano_types.decode_shelley_tx_body
    body

let test_shelley_tx_body_full () =
  let body = Cardano_types.{
    inputs = [
      { tx_id = hash32 (); tx_index = 0L };
      { tx_id = hash32_alt (); tx_index = 1L };
    ];
    outputs = [
      { address = encode_shelley_address
          (Enterprise_address { network = 1; payment = Key_hash (hash28 ()) });
        amount = 1500000L };
      { address = encode_shelley_address
          (Base_address { network = 1; payment = Key_hash (hash28 ());
                          stake = Key_hash (hash28_alt ()) });
        amount = 3500000L };
    ];
    fee = 200000L;
    ttl = 50000000L;
    certs = Some [Stake_delegation { delegator = Key_hash (hash28 ());
                                     pool = hash28_alt () }];
    withdrawals = Some [
      (encode_shelley_address
         (Reward_address { network = 1; stake = Key_hash (hash28 ()) }),
       5000000L);
    ];
    update = None;
    auxiliary_data_hash = Some (hash32 ());
  } in
  check_cbor_roundtrip "shelley tx body (full)"
    Cardano_types.encode_shelley_tx_body
    Cardano_types.decode_shelley_tx_body
    body

let test_shelley_tx_witness_set () =
  let ws = Cardano_types.{
    vkey_witnesses = Some [
      { witness_vkey = Bytes.make 32 '\x11';
        witness_sig = Bytes.make 64 '\x22' };
    ];
    multisig_scripts = Some [Sig (hash28 ())];
    bootstrap_witnesses = None;
  } in
  check_cbor_roundtrip "shelley witness set"
    Cardano_types.encode_shelley_tx_witness_set
    Cardano_types.decode_shelley_tx_witness_set
    ws

(* ================================================================ *)
(* Protocol parameters test                                          *)
(* ================================================================ *)

let test_protocol_param_update () =
  let p = Cardano_types.{
    empty_protocol_param_update with
    min_fee_a = Some 44L;
    min_fee_b = Some 155381L;
    max_block_body_size = Some 65536L;
    max_tx_size = Some 16384L;
    key_deposit = Some 2000000L;
    pool_deposit = Some 500000000L;
    pool_pledge_influence = Some { numerator = 3L; denominator = 10L };
    protocol_version = Some (2L, 0L);
  } in
  check_cbor_roundtrip "protocol param update"
    Cardano_types.encode_protocol_param_update
    Cardano_types.decode_protocol_param_update
    p

let test_protocol_param_update_empty () =
  check_cbor_roundtrip "empty protocol param update"
    Cardano_types.encode_protocol_param_update
    Cardano_types.decode_protocol_param_update
    Cardano_types.empty_protocol_param_update

(* ================================================================ *)
(* Shelley header body test                                          *)
(* ================================================================ *)

let test_shelley_header_body () =
  let hb = Cardano_types.{
    shb_block_number = 100L;
    shb_slot = 1000L;
    shb_prev_hash = Some (hash32 ());
    shb_issuer_vkey = Bytes.make 32 '\x33';
    shb_vrf_vkey = Bytes.make 32 '\x44';
    shb_nonce_vrf = {
      vrf_output = Bytes.make 32 '\xaa';
      vrf_proof = Bytes.make 80 '\xbb';
    };
    shb_leader_vrf = {
      vrf_output = Bytes.make 32 '\xcc';
      vrf_proof = Bytes.make 80 '\xdd';
    };
    shb_body_size = 1024L;
    shb_body_hash = hash32 ();
    shb_operational_cert = {
      hot_vkey = Bytes.make 32 '\x55';
      sequence_number = 1L;
      kes_period = 200L;
      sigma = Bytes.make 64 '\x66';
    };
    shb_protocol_version = { proto_major = 2L; proto_minor = 0L };
  } in
  check_cbor_roundtrip "shelley header body"
    Cardano_types.encode_shelley_header_body
    Cardano_types.decode_shelley_header_body
    hb

let test_shelley_header () =
  let hdr = Cardano_types.{
    sh_header_body = {
      shb_block_number = 100L;
      shb_slot = 1000L;
      shb_prev_hash = None;
      shb_issuer_vkey = Bytes.make 32 '\x33';
      shb_vrf_vkey = Bytes.make 32 '\x44';
      shb_nonce_vrf = {
        vrf_output = Bytes.make 32 '\xaa';
        vrf_proof = Bytes.make 80 '\xbb';
      };
      shb_leader_vrf = {
        vrf_output = Bytes.make 32 '\xcc';
        vrf_proof = Bytes.make 80 '\xdd';
      };
      shb_body_size = 0L;
      shb_body_hash = hash32 ();
      shb_operational_cert = {
        hot_vkey = Bytes.make 32 '\x55';
        sequence_number = 0L;
        kes_period = 0L;
        sigma = Bytes.make 64 '\x66';
      };
      shb_protocol_version = { proto_major = 2L; proto_minor = 0L };
    };
    sh_body_signature = Bytes.make 448 '\x77';
  } in
  check_cbor_roundtrip "shelley header"
    Cardano_types.encode_shelley_header
    Cardano_types.decode_shelley_header
    hdr

(* ================================================================ *)
(* Relay tests                                                       *)
(* ================================================================ *)

let test_relay_single_host_addr () =
  let relay = Cardano_types.Single_host_addr {
    port = Some 3001;
    ipv4 = Some (Bytes.of_string "\x7f\x00\x00\x01");
    ipv6 = None;
  } in
  check_cbor_roundtrip "relay single host addr"
    Cardano_types.encode_relay
    Cardano_types.decode_relay
    relay

let test_relay_single_host_name () =
  let relay = Cardano_types.Single_host_name {
    port = Some 3001;
    dns_name = "relay1.example.com";
  } in
  check_cbor_roundtrip "relay single host name"
    Cardano_types.encode_relay
    Cardano_types.decode_relay
    relay

let test_relay_multi_host_name () =
  let relay = Cardano_types.Multi_host_name {
    dns_name = "_tcp.example.com";
  } in
  check_cbor_roundtrip "relay multi host name"
    Cardano_types.encode_relay
    Cardano_types.decode_relay
    relay

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Cardano Types"
    [ ( "Byron types",
        [ Alcotest.test_case "address" `Quick test_byron_address;
          Alcotest.test_case "tx_in" `Quick test_byron_tx_in;
          Alcotest.test_case "transaction" `Quick test_byron_tx;
          Alcotest.test_case "block header" `Quick test_byron_block_header ] );
      ( "Shelley addresses",
        [ Alcotest.test_case "base (key/key)" `Quick test_base_address;
          Alcotest.test_case "base (script/key)" `Quick test_base_address_script;
          Alcotest.test_case "enterprise" `Quick test_enterprise_address;
          Alcotest.test_case "reward" `Quick test_reward_address;
          Alcotest.test_case "pointer" `Quick test_pointer_address ] );
      ( "Shelley primitives",
        [ Alcotest.test_case "credential" `Quick test_credential;
          Alcotest.test_case "tx_in" `Quick test_tx_in;
          Alcotest.test_case "tx_out" `Quick test_shelley_tx_out;
          Alcotest.test_case "rational" `Quick test_rational;
          Alcotest.test_case "vrf_cert" `Quick test_vrf_cert;
          Alcotest.test_case "vkey_witness" `Quick test_vkey_witness;
          Alcotest.test_case "multisig_script" `Quick test_multisig_script ] );
      ( "Certificates",
        [ Alcotest.test_case "stake registration" `Quick test_cert_stake_registration;
          Alcotest.test_case "stake delegation" `Quick test_cert_stake_delegation;
          Alcotest.test_case "pool registration" `Quick test_cert_pool_registration;
          Alcotest.test_case "pool retirement" `Quick test_cert_pool_retirement;
          Alcotest.test_case "genesis delegation" `Quick test_cert_genesis_delegation ] );
      ( "Transaction bodies",
        [ Alcotest.test_case "minimal" `Quick test_shelley_tx_body_minimal;
          Alcotest.test_case "with optionals" `Quick test_shelley_tx_body_full;
          Alcotest.test_case "witness set" `Quick test_shelley_tx_witness_set ] );
      ( "Protocol parameters",
        [ Alcotest.test_case "partial update" `Quick test_protocol_param_update;
          Alcotest.test_case "empty update" `Quick test_protocol_param_update_empty ] );
      ( "Block headers",
        [ Alcotest.test_case "header body" `Quick test_shelley_header_body;
          Alcotest.test_case "full header" `Quick test_shelley_header ] );
      ( "Relays",
        [ Alcotest.test_case "single host addr" `Quick test_relay_single_host_addr;
          Alcotest.test_case "single host name" `Quick test_relay_single_host_name;
          Alcotest.test_case "multi host name" `Quick test_relay_multi_host_name ] );
    ]
