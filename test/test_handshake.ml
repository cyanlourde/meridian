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

(** Round-trip: encode to CBOR -> serialize -> deserialize -> decode -> re-encode -> compare bytes *)
let check_msg_roundtrip name msg =
  let raw1 = Handshake.to_bytes msg in
  match Handshake.of_bytes raw1 with
  | Error e -> Alcotest.fail (Printf.sprintf "decode failed: %s" e)
  | Ok msg' ->
    let raw2 = Handshake.to_bytes msg' in
    Alcotest.check bytes_testable name raw1 raw2

let mainnet_params = Handshake.default_params ~network_magic:Handshake.mainnet_magic
let _preprod_params = Handshake.default_params ~network_magic:Handshake.preprod_magic

(* ================================================================ *)
(* ProposeVersions encode/decode                                     *)
(* ================================================================ *)

let test_propose_single_version () =
  let msg = Handshake.ProposeVersions [(13L, mainnet_params)] in
  check_msg_roundtrip "propose single version" msg

let test_propose_multiple_versions () =
  let msg = Handshake.ProposeVersions [
    (10L, mainnet_params);
    (11L, mainnet_params);
    (13L, mainnet_params);
  ] in
  check_msg_roundtrip "propose multiple versions" msg

let test_propose_empty () =
  let msg = Handshake.ProposeVersions [] in
  check_msg_roundtrip "propose empty" msg

let test_propose_with_params () =
  let params = Handshake.{
    network_magic = Handshake.mainnet_magic;
    initiator_only_diffusion_mode = true;
    peer_sharing = 2;
    query = true;
  } in
  let msg = Handshake.ProposeVersions [(13L, params)] in
  check_msg_roundtrip "propose with full params" msg

(* ================================================================ *)
(* AcceptVersion encode/decode                                       *)
(* ================================================================ *)

let test_accept_version () =
  let msg = Handshake.AcceptVersion (13L, mainnet_params) in
  check_msg_roundtrip "accept version 13" msg

let test_accept_version_legacy () =
  (* Version 6 uses simple uint encoding for params *)
  let msg = Handshake.AcceptVersion (6L, mainnet_params) in
  check_msg_roundtrip "accept version 6 (legacy)" msg

let test_accept_version_v11 () =
  let params = Handshake.{
    network_magic = Handshake.preprod_magic;
    initiator_only_diffusion_mode = false;
    peer_sharing = 1;
    query = false;
  } in
  let msg = Handshake.AcceptVersion (11L, params) in
  check_msg_roundtrip "accept version 11" msg

(* ================================================================ *)
(* Refuse encode/decode                                              *)
(* ================================================================ *)

let test_refuse_version_mismatch () =
  let msg = Handshake.Refuse (VersionMismatch [10L; 11L; 12L]) in
  check_msg_roundtrip "refuse version mismatch" msg

let test_refuse_decode_error () =
  let msg = Handshake.Refuse (HandshakeDecodeError (13L, "bad params")) in
  check_msg_roundtrip "refuse decode error" msg

let test_refuse_refused () =
  let msg = Handshake.Refuse (Refused (13L, "wrong network")) in
  check_msg_roundtrip "refuse refused" msg

let test_refuse_version_mismatch_empty () =
  let msg = Handshake.Refuse (VersionMismatch []) in
  check_msg_roundtrip "refuse empty version list" msg

(* ================================================================ *)
(* CBOR structure verification                                       *)
(* ================================================================ *)

let test_propose_cbor_structure () =
  (* Verify the CBOR matches [0, {version => params}] *)
  let msg = Handshake.ProposeVersions [(13L, mainnet_params)] in
  let cbor = Handshake.encode_handshake_message msg in
  match cbor with
  | Cbor.Array [Cbor.Uint 0L; Cbor.Map _] -> ()
  | _ -> Alcotest.fail "ProposeVersions should be [0, map]"

let test_accept_cbor_structure () =
  let msg = Handshake.AcceptVersion (13L, mainnet_params) in
  let cbor = Handshake.encode_handshake_message msg in
  match cbor with
  | Cbor.Array [Cbor.Uint 1L; Cbor.Uint 13L; _] -> ()
  | _ -> Alcotest.fail "AcceptVersion should be [1, version, params]"

let test_refuse_cbor_structure () =
  let msg = Handshake.Refuse (VersionMismatch [10L]) in
  let cbor = Handshake.encode_handshake_message msg in
  match cbor with
  | Cbor.Array [Cbor.Uint 2L; Cbor.Array [Cbor.Uint 0L; Cbor.Array _]] -> ()
  | _ -> Alcotest.fail "Refuse should be [2, [0, [versions]]]"

(* ================================================================ *)
(* Network magic encoding                                            *)
(* ================================================================ *)

let test_network_magic_mainnet () =
  Alcotest.(check int64) "mainnet magic" 764824073L Handshake.mainnet_magic

let test_network_magic_preprod () =
  Alcotest.(check int64) "preprod magic" 1L Handshake.preprod_magic

let test_network_magic_preview () =
  Alcotest.(check int64) "preview magic" 2L Handshake.preview_magic

let test_network_magic_in_params () =
  (* Version 6 encodes params as just the magic number *)
  let params = Handshake.default_params ~network_magic:Handshake.mainnet_magic in
  let cbor = Handshake.encode_version_params 6L params in
  match cbor with
  | Cbor.Uint 764824073L -> ()
  | _ -> Alcotest.fail "v6 params should be just the network magic uint"

let test_network_magic_in_array_params () =
  (* Version 13 encodes params as [magic, ...] *)
  let params = Handshake.default_params ~network_magic:Handshake.mainnet_magic in
  let cbor = Handshake.encode_version_params 13L params in
  match cbor with
  | Cbor.Array (Cbor.Uint 764824073L :: _) -> ()
  | _ -> Alcotest.fail "v13 params should start with network magic in array"

(* ================================================================ *)
(* Version params encoding per version range                         *)
(* ================================================================ *)

let test_params_v6_encoding () =
  let params = Handshake.default_params ~network_magic:42L in
  let cbor = Handshake.encode_version_params 6L params in
  (match cbor with Cbor.Uint 42L -> () | _ -> Alcotest.fail "v6: expected uint");
  match Handshake.decode_version_params cbor with
  | Error e -> Alcotest.fail e
  | Ok p -> Alcotest.(check int64) "v6 magic" 42L p.network_magic

let test_params_v10_encoding () =
  let params = Handshake.{ (default_params ~network_magic:42L) with
    initiator_only_diffusion_mode = true } in
  let cbor = Handshake.encode_version_params 10L params in
  (match cbor with
   | Cbor.Array [Cbor.Uint 42L; Cbor.Bool true] -> ()
   | _ -> Alcotest.fail "v10: expected [magic, true]");
  match Handshake.decode_version_params cbor with
  | Error e -> Alcotest.fail e
  | Ok p ->
    Alcotest.(check int64) "v10 magic" 42L p.network_magic;
    Alcotest.(check bool) "v10 initiator_only" true p.initiator_only_diffusion_mode

let test_params_v12_encoding () =
  let params = Handshake.{ (default_params ~network_magic:42L) with peer_sharing = 2 } in
  let cbor = Handshake.encode_version_params 12L params in
  (match cbor with
   | Cbor.Array [Cbor.Uint 42L; Cbor.Bool false; Cbor.Uint 2L] -> ()
   | _ -> Alcotest.fail "v12: expected [magic, false, 2]");
  match Handshake.decode_version_params cbor with
  | Error e -> Alcotest.fail e
  | Ok p ->
    Alcotest.(check int) "v12 peer_sharing" 2 p.peer_sharing

let test_params_v13_encoding () =
  let params = Handshake.{
    network_magic = 42L;
    initiator_only_diffusion_mode = true;
    peer_sharing = 1;
    query = true;
  } in
  let cbor = Handshake.encode_version_params 13L params in
  (match cbor with
   | Cbor.Array [Cbor.Uint 42L; Cbor.Bool true; Cbor.Uint 1L; Cbor.Bool true] -> ()
   | _ -> Alcotest.fail "v13: expected [magic, true, 1, true]");
  match Handshake.decode_version_params cbor with
  | Error e -> Alcotest.fail e
  | Ok p ->
    Alcotest.(check int64) "v13 magic" 42L p.network_magic;
    Alcotest.(check bool) "v13 initiator_only" true p.initiator_only_diffusion_mode;
    Alcotest.(check int) "v13 peer_sharing" 1 p.peer_sharing;
    Alcotest.(check bool) "v13 query" true p.query

(* ================================================================ *)
(* Version negotiation                                               *)
(* ================================================================ *)

let test_negotiate_mutual_version () =
  let server_supported = [
    (10L, mainnet_params);
    (11L, mainnet_params);
    (13L, mainnet_params);
  ] in
  let client_msg = Handshake.ProposeVersions [
    (10L, mainnet_params);
    (12L, mainnet_params);
    (13L, mainnet_params);
  ] in
  match Handshake.negotiate ~supported:server_supported client_msg with
  | AcceptVersion (v, _) ->
    (* Should pick highest mutual: 13 *)
    Alcotest.(check int64) "negotiated version" 13L v
  | Refuse _ -> Alcotest.fail "expected AcceptVersion"
  | ProposeVersions _ -> Alcotest.fail "unexpected ProposeVersions"

let test_negotiate_picks_highest () =
  let server_supported = [
    (7L, mainnet_params);
    (8L, mainnet_params);
    (9L, mainnet_params);
  ] in
  let client_msg = Handshake.ProposeVersions [
    (7L, mainnet_params);
    (8L, mainnet_params);
  ] in
  match Handshake.negotiate ~supported:server_supported client_msg with
  | AcceptVersion (v, _) ->
    Alcotest.(check int64) "highest mutual" 8L v
  | _ -> Alcotest.fail "expected AcceptVersion"

let test_negotiate_no_overlap () =
  let server_supported = [(13L, mainnet_params)] in
  let client_msg = Handshake.ProposeVersions [
    (10L, mainnet_params);
    (11L, mainnet_params);
  ] in
  match Handshake.negotiate ~supported:server_supported client_msg with
  | Refuse (VersionMismatch vs) ->
    Alcotest.(check int) "mismatch count" 2 (List.length vs)
  | _ -> Alcotest.fail "expected Refuse VersionMismatch"

let test_negotiate_empty_proposals () =
  let server_supported = [(13L, mainnet_params)] in
  let client_msg = Handshake.ProposeVersions [] in
  match Handshake.negotiate ~supported:server_supported client_msg with
  | Refuse (VersionMismatch vs) ->
    Alcotest.(check int) "empty mismatch" 0 (List.length vs)
  | _ -> Alcotest.fail "expected Refuse VersionMismatch"

let test_negotiate_uses_server_params () =
  let server_params = Handshake.{
    network_magic = Handshake.mainnet_magic;
    initiator_only_diffusion_mode = false;
    peer_sharing = 2;
    query = false;
  } in
  let client_params = Handshake.{
    network_magic = Handshake.mainnet_magic;
    initiator_only_diffusion_mode = true;
    peer_sharing = 0;
    query = true;
  } in
  let server_supported = [(13L, server_params)] in
  let client_msg = Handshake.ProposeVersions [(13L, client_params)] in
  match Handshake.negotiate ~supported:server_supported client_msg with
  | AcceptVersion (_, p) ->
    (* Server's params should be used, not client's *)
    Alcotest.(check bool) "server's initiator_only" false p.initiator_only_diffusion_mode;
    Alcotest.(check int) "server's peer_sharing" 2 p.peer_sharing
  | _ -> Alcotest.fail "expected AcceptVersion"

let test_negotiate_wrong_message_type () =
  let server_supported = [(13L, mainnet_params)] in
  let bad_msg = Handshake.AcceptVersion (13L, mainnet_params) in
  match Handshake.negotiate ~supported:server_supported bad_msg with
  | Refuse (Refused _) -> ()
  | _ -> Alcotest.fail "expected Refuse for non-ProposeVersions"

(* ================================================================ *)
(* Client handle_response                                            *)
(* ================================================================ *)

let test_client_accept_valid () =
  let supported = [(13L, mainnet_params)] in
  let response = Handshake.AcceptVersion (13L, mainnet_params) in
  match Handshake.handle_response ~supported response with
  | Ok (v, _) -> Alcotest.(check int64) "accepted" 13L v
  | Error e -> Alcotest.fail e

let test_client_accept_unsupported () =
  let supported = [(13L, mainnet_params)] in
  let response = Handshake.AcceptVersion (10L, mainnet_params) in
  match Handshake.handle_response ~supported response with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for unsupported version"

let test_client_refuse () =
  let supported = [(13L, mainnet_params)] in
  let response = Handshake.Refuse (VersionMismatch [13L]) in
  match Handshake.handle_response ~supported response with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for Refuse"

(* ================================================================ *)
(* Full client-server roundtrip                                      *)
(* ================================================================ *)

let test_full_handshake_roundtrip () =
  let client_versions = [
    (10L, mainnet_params);
    (13L, mainnet_params);
  ] in
  let server_versions = [
    (11L, mainnet_params);
    (13L, mainnet_params);
  ] in
  (* Client proposes *)
  let proposal = Handshake.propose_versions client_versions in
  (* Serialize and deserialize (simulates network) *)
  let raw = Handshake.to_bytes proposal in
  let proposal' = match Handshake.of_bytes raw with
    | Ok m -> m | Error e -> Alcotest.fail e
  in
  (* Server negotiates *)
  let response = Handshake.negotiate ~supported:server_versions proposal' in
  (* Serialize and deserialize *)
  let raw = Handshake.to_bytes response in
  let response' = match Handshake.of_bytes raw with
    | Ok m -> m | Error e -> Alcotest.fail e
  in
  (* Client validates *)
  match Handshake.handle_response ~supported:client_versions response' with
  | Ok (v, _) -> Alcotest.(check int64) "negotiated 13" 13L v
  | Error e -> Alcotest.fail e

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Handshake"
    [ ( "ProposeVersions",
        [ Alcotest.test_case "single version" `Quick test_propose_single_version;
          Alcotest.test_case "multiple versions" `Quick test_propose_multiple_versions;
          Alcotest.test_case "empty" `Quick test_propose_empty;
          Alcotest.test_case "with full params" `Quick test_propose_with_params ] );
      ( "AcceptVersion",
        [ Alcotest.test_case "version 13" `Quick test_accept_version;
          Alcotest.test_case "version 6 legacy" `Quick test_accept_version_legacy;
          Alcotest.test_case "version 11" `Quick test_accept_version_v11 ] );
      ( "Refuse",
        [ Alcotest.test_case "version mismatch" `Quick test_refuse_version_mismatch;
          Alcotest.test_case "decode error" `Quick test_refuse_decode_error;
          Alcotest.test_case "refused" `Quick test_refuse_refused;
          Alcotest.test_case "empty version list" `Quick test_refuse_version_mismatch_empty ] );
      ( "CBOR structure",
        [ Alcotest.test_case "propose structure" `Quick test_propose_cbor_structure;
          Alcotest.test_case "accept structure" `Quick test_accept_cbor_structure;
          Alcotest.test_case "refuse structure" `Quick test_refuse_cbor_structure ] );
      ( "Network magic",
        [ Alcotest.test_case "mainnet" `Quick test_network_magic_mainnet;
          Alcotest.test_case "preprod" `Quick test_network_magic_preprod;
          Alcotest.test_case "preview" `Quick test_network_magic_preview;
          Alcotest.test_case "in uint params" `Quick test_network_magic_in_params;
          Alcotest.test_case "in array params" `Quick test_network_magic_in_array_params ] );
      ( "Version params encoding",
        [ Alcotest.test_case "v6 (uint)" `Quick test_params_v6_encoding;
          Alcotest.test_case "v10 (2-elem)" `Quick test_params_v10_encoding;
          Alcotest.test_case "v12 (3-elem)" `Quick test_params_v12_encoding;
          Alcotest.test_case "v13 (4-elem)" `Quick test_params_v13_encoding ] );
      ( "Negotiation",
        [ Alcotest.test_case "mutual version" `Quick test_negotiate_mutual_version;
          Alcotest.test_case "picks highest" `Quick test_negotiate_picks_highest;
          Alcotest.test_case "no overlap" `Quick test_negotiate_no_overlap;
          Alcotest.test_case "empty proposals" `Quick test_negotiate_empty_proposals;
          Alcotest.test_case "uses server params" `Quick test_negotiate_uses_server_params;
          Alcotest.test_case "wrong message type" `Quick test_negotiate_wrong_message_type ] );
      ( "Client logic",
        [ Alcotest.test_case "accept valid" `Quick test_client_accept_valid;
          Alcotest.test_case "accept unsupported" `Quick test_client_accept_unsupported;
          Alcotest.test_case "refuse" `Quick test_client_refuse ] );
      ( "Full roundtrip",
        [ Alcotest.test_case "client-server" `Quick test_full_handshake_roundtrip ] );
    ]
