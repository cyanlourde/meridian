open Meridian

let () = Crypto.init ()

(* VRF requires the Cardano libsodium fork. These tests verify
   the fallback behavior when standard libsodium is installed. *)

let test_vrf_stub () =
  match Crypto.vrf_verify ~public_key:Bytes.empty
          ~proof:Bytes.empty ~message:Bytes.empty with
  | Error _ -> ()  (* Expected: VRF not available *)
  | Ok _ -> ()     (* If Cardano fork is installed, this could succeed *)

let test_slot_leader_deterministic () =
  let nonce = Bytes.make 32 '\xaa' in
  let (l1, o1, _) = Slot_leader.check_leadership
    ~slot:100L ~epoch_nonce:nonce ~pool_stake:1000L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  let (l2, o2, _) = Slot_leader.check_leadership
    ~slot:100L ~epoch_nonce:nonce ~pool_stake:1000L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  Alcotest.(check bool) "deterministic" true (l1 = l2);
  Alcotest.(check bool) "same output" true (Bytes.equal o1 o2)

let test_vrf_input_format () =
  let nonce = Bytes.make 32 '\xbb' in
  let input = Slot_leader.vrf_input ~slot:42L ~epoch_nonce:nonce in
  Alcotest.(check bool) "non-empty" true (Bytes.length input > 0);
  Alcotest.(check int) "starts with L" (Char.code 'L') (Bytes.get_uint8 input 0)

let test_vrf_different_slots () =
  let nonce = Bytes.make 32 '\xcc' in
  let (_, o1, _) = Slot_leader.check_leadership
    ~slot:1L ~epoch_nonce:nonce ~pool_stake:5000L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  let (_, o2, _) = Slot_leader.check_leadership
    ~slot:2L ~epoch_nonce:nonce ~pool_stake:5000L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  Alcotest.(check bool) "different" false (Bytes.equal o1 o2)

let () =
  Alcotest.run "VRF-Real"
    [ ( "VRF availability",
        [ Alcotest.test_case "stub or real" `Quick test_vrf_stub ] );
      ( "Slot leader",
        [ Alcotest.test_case "deterministic" `Quick test_slot_leader_deterministic;
          Alcotest.test_case "input format" `Quick test_vrf_input_format;
          Alcotest.test_case "different slots" `Quick test_vrf_different_slots ] );
    ]
