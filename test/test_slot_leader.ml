open Meridian

let () = Crypto.init ()

let epoch_nonce = Bytes.make 32 '\xaa'

let test_deterministic () =
  let (leader1, out1, _) = Slot_leader.check_leadership
    ~slot:100L ~epoch_nonce ~pool_stake:1000L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  let (leader2, out2, _) = Slot_leader.check_leadership
    ~slot:100L ~epoch_nonce ~pool_stake:1000L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  Alcotest.(check bool) "deterministic" true (leader1 = leader2);
  Alcotest.(check bool) "same output" true (Bytes.equal out1 out2)

let test_zero_stake () =
  let (leader, _, _) = Slot_leader.check_leadership
    ~slot:100L ~epoch_nonce ~pool_stake:0L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  Alcotest.(check bool) "zero stake = no leader" false leader

let test_full_stake () =
  let (leader, _, _) = Slot_leader.check_leadership
    ~slot:100L ~epoch_nonce ~pool_stake:10000L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  Alcotest.(check bool) "full stake = leader" true leader

let test_vrf_input () =
  let input = Slot_leader.vrf_input ~slot:42L ~epoch_nonce in
  Alcotest.(check bool) "non-empty" true (Bytes.length input > 0);
  (* Should start with 'L' *)
  Alcotest.(check int) "starts with L" (Char.code 'L') (Bytes.get_uint8 input 0)

let test_kes_period () =
  Alcotest.(check int64) "slot 0" 0L
    (Slot_leader.kes_period_of_slot ~slots_per_kes_period:129600L 0L);
  Alcotest.(check int64) "slot 129599" 0L
    (Slot_leader.kes_period_of_slot ~slots_per_kes_period:129600L 129599L);
  Alcotest.(check int64) "slot 129600" 1L
    (Slot_leader.kes_period_of_slot ~slots_per_kes_period:129600L 129600L);
  Alcotest.(check int64) "slot 259200" 2L
    (Slot_leader.kes_period_of_slot ~slots_per_kes_period:129600L 259200L)

let test_different_slots () =
  (* Different slots should give different VRF outputs *)
  let (_, out1, _) = Slot_leader.check_leadership
    ~slot:100L ~epoch_nonce ~pool_stake:5000L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  let (_, out2, _) = Slot_leader.check_leadership
    ~slot:101L ~epoch_nonce ~pool_stake:5000L ~total_stake:10000L
    ~active_slots_coeff_num:1 ~active_slots_coeff_den:20 in
  Alcotest.(check bool) "different outputs" false (Bytes.equal out1 out2)

let test_kes_key () =
  let key = Kes.create ~secret_key:(Bytes.make 64 '\x01')
    ~start_period:0L ~max_period:62 in
  Alcotest.(check int) "remaining" 62 (Kes.remaining_periods key);
  (match Kes.evolve key with Ok () -> () | Error e -> Alcotest.fail e);
  Alcotest.(check int) "remaining after evolve" 61 (Kes.remaining_periods key)

let () =
  Alcotest.run "Slot-Leader"
    [ ( "Leadership",
        [ Alcotest.test_case "deterministic" `Quick test_deterministic;
          Alcotest.test_case "zero stake" `Quick test_zero_stake;
          Alcotest.test_case "full stake" `Quick test_full_stake;
          Alcotest.test_case "different slots" `Quick test_different_slots ] );
      ( "VRF",
        [ Alcotest.test_case "input format" `Quick test_vrf_input ] );
      ( "KES",
        [ Alcotest.test_case "period calc" `Quick test_kes_period;
          Alcotest.test_case "key lifecycle" `Quick test_kes_key ] );
    ]
