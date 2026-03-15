open Meridian

let params = Consensus.mainnet_params
let hash_of c = Bytes.make 32 c

let make_chain tip_char block_number slot =
  Consensus.{
    chain_tip = Chain_sync.Point (slot, hash_of tip_char);
    chain_block_number = block_number;
    chain_slot = slot;
  }

(* ================================================================ *)
(* Protocol parameter defaults                                       *)
(* ================================================================ *)

let test_mainnet_params () =
  Alcotest.(check int) "k" 2160 params.security_param;
  Alcotest.(check int) "f_num" 1 params.active_slot_coeff_num;
  Alcotest.(check int) "f_den" 20 params.active_slot_coeff_den;
  Alcotest.(check int64) "epoch_length" 432000L params.epoch_length;
  Alcotest.(check int) "slot_length_ms" 1000 params.slot_length_ms;
  Alcotest.(check int64) "slots_per_kes" 129600L params.slots_per_kes_period;
  Alcotest.(check int) "max_kes_evo" 62 params.max_kes_evolutions

(* ================================================================ *)
(* Epoch and slot calculations                                       *)
(* ================================================================ *)

let test_epoch_of_slot () =
  Alcotest.(check int64) "slot 0" 0L (Consensus.epoch_of_slot params 0L);
  Alcotest.(check int64) "slot 431999" 0L (Consensus.epoch_of_slot params 431999L);
  Alcotest.(check int64) "slot 432000" 1L (Consensus.epoch_of_slot params 432000L);
  Alcotest.(check int64) "slot 864000" 2L (Consensus.epoch_of_slot params 864000L);
  Alcotest.(check int64) "slot 1000000" 2L (Consensus.epoch_of_slot params 1000000L)

let test_slot_in_epoch () =
  Alcotest.(check int64) "slot 0" 0L (Consensus.slot_in_epoch params 0L);
  Alcotest.(check int64) "slot 100" 100L (Consensus.slot_in_epoch params 100L);
  Alcotest.(check int64) "slot 432000" 0L (Consensus.slot_in_epoch params 432000L);
  Alcotest.(check int64) "slot 432042" 42L (Consensus.slot_in_epoch params 432042L)

let test_kes_period () =
  Alcotest.(check int64) "slot 0" 0L (Consensus.kes_period_of_slot params 0L);
  Alcotest.(check int64) "slot 129599" 0L (Consensus.kes_period_of_slot params 129599L);
  Alcotest.(check int64) "slot 129600" 1L (Consensus.kes_period_of_slot params 129600L);
  Alcotest.(check int64) "slot 259200" 2L (Consensus.kes_period_of_slot params 259200L)

let test_kes_key_valid () =
  (* Key issued at period 0, valid for 63 periods (0..62) *)
  Alcotest.(check bool) "period 0, slot 0" true
    (Consensus.kes_key_valid params ~kes_start_period:0L 0L);
  Alcotest.(check bool) "period 0, slot 129599" true
    (Consensus.kes_key_valid params ~kes_start_period:0L 129599L);
  (* period 62 = last valid *)
  let slot_period_62 = Int64.mul 62L 129600L in
  Alcotest.(check bool) "period 0, at period 62" true
    (Consensus.kes_key_valid params ~kes_start_period:0L slot_period_62);
  (* period 63 = expired *)
  let slot_period_63 = Int64.mul 63L 129600L in
  Alcotest.(check bool) "period 0, at period 63" false
    (Consensus.kes_key_valid params ~kes_start_period:0L slot_period_63);
  (* Key issued at period 10 *)
  Alcotest.(check bool) "period 10, at period 10" true
    (Consensus.kes_key_valid params ~kes_start_period:10L (Int64.mul 10L 129600L));
  Alcotest.(check bool) "period 10, at period 72" true
    (Consensus.kes_key_valid params ~kes_start_period:10L (Int64.mul 72L 129600L));
  Alcotest.(check bool) "period 10, at period 73" false
    (Consensus.kes_key_valid params ~kes_start_period:10L (Int64.mul 73L 129600L))

let test_max_kes_slot () =
  (* 129600 * 63 = 8164800 *)
  Alcotest.(check int64) "max kes slot" 8164800L (Consensus.max_kes_slot params)

(* ================================================================ *)
(* Chain selection                                                   *)
(* ================================================================ *)

let test_longer_chain_wins () =
  let our = make_chain '\x01' 100L 1000L in
  let candidate = make_chain '\x02' 101L 1001L in
  Alcotest.(check bool) "longer wins" true
    (Consensus.compare_chains params ~our ~candidate ~fork_block_number:99L = Prefer)

let test_shorter_chain_rejected () =
  let our = make_chain '\x01' 100L 1000L in
  let candidate = make_chain '\x02' 99L 999L in
  Alcotest.(check bool) "shorter rejected" true
    (Consensus.compare_chains params ~our ~candidate ~fork_block_number:98L = Reject)

let test_equal_length_tiebreak () =
  (* Same block number, candidate hash < our hash → Prefer *)
  let our = make_chain '\xbb' 100L 1000L in
  let candidate = make_chain '\xaa' 100L 1000L in
  Alcotest.(check bool) "smaller hash wins" true
    (Consensus.compare_chains params ~our ~candidate ~fork_block_number:99L = Prefer)

let test_equal_length_tiebreak_reject () =
  let our = make_chain '\xaa' 100L 1000L in
  let candidate = make_chain '\xbb' 100L 1000L in
  Alcotest.(check bool) "larger hash rejected" true
    (Consensus.compare_chains params ~our ~candidate ~fork_block_number:99L = Reject)

let test_equal_chains () =
  let our = make_chain '\xaa' 100L 1000L in
  let candidate = make_chain '\xaa' 100L 1000L in
  Alcotest.(check bool) "equal" true
    (Consensus.compare_chains params ~our ~candidate ~fork_block_number:99L = Equal)

let test_k_deep_fork_rejected () =
  (* Fork deeper than k=2160 blocks is always rejected even if longer *)
  let our = make_chain '\x01' 5000L 50000L in
  let candidate = make_chain '\x02' 5001L 50001L in
  (* fork_block_number = 5000 - 2161 = 2839, rollback = 5000 - 2839 = 2161 > k *)
  Alcotest.(check bool) "k-deep rejected" true
    (Consensus.compare_chains params ~our ~candidate ~fork_block_number:2839L = Reject)

let test_exactly_k_deep_allowed () =
  (* Fork exactly k blocks deep should be allowed *)
  let our = make_chain '\x01' 5000L 50000L in
  let candidate = make_chain '\x02' 5001L 50001L in
  (* fork_block_number = 5000 - 2160 = 2840, rollback = 5000 - 2840 = 2160 = k *)
  Alcotest.(check bool) "exactly k allowed" true
    (Consensus.compare_chains params ~our ~candidate ~fork_block_number:2840L = Prefer)

let test_origin_tiebreak () =
  let our = { Consensus.chain_tip = Origin; chain_block_number = 0L; chain_slot = 0L } in
  let candidate = make_chain '\x01' 0L 0L in
  Alcotest.(check bool) "point preferred over origin" true
    (Consensus.compare_chains params ~our ~candidate ~fork_block_number:0L = Prefer)

(* ================================================================ *)
(* Slot leader eligibility                                           *)
(* ================================================================ *)

let test_zero_stake_not_leader () =
  let vrf = Bytes.make 32 '\x00' in
  Alcotest.(check bool) "zero stake" false
    (Consensus.is_slot_leader params ~vrf_output:vrf ~stake_num:0L ~stake_den:1000L)

let test_full_stake_always_leader () =
  (* 100% stake should always be elected regardless of VRF output *)
  let vrf = Bytes.make 32 '\xff' in
  Alcotest.(check bool) "full stake" true
    (Consensus.is_slot_leader params ~vrf_output:vrf ~stake_num:1000L ~stake_den:1000L)

let test_low_vrf_elected () =
  (* VRF output all zeros = 0 / 2^64, which is < any positive threshold *)
  let vrf = Bytes.make 32 '\x00' in
  Alcotest.(check bool) "low VRF with reasonable stake" true
    (Consensus.is_slot_leader params ~vrf_output:vrf ~stake_num:100L ~stake_den:1000L)

let test_high_vrf_small_stake () =
  (* VRF output all 0xFF = ~1.0, tiny stake → threshold ≈ f*σ ≈ tiny *)
  let vrf = Bytes.make 32 '\xff' in
  Alcotest.(check bool) "high VRF tiny stake" false
    (Consensus.is_slot_leader params ~vrf_output:vrf ~stake_num:1L ~stake_den:1000000L)

let test_medium_stake_threshold () =
  (* 10% stake with f=1/20: threshold ≈ 1-(1-0.05)^0.1 ≈ 0.00513
     VRF output 0x01... = ~1/256 ≈ 0.0039 < 0.00513 → elected *)
  let vrf = Bytes.make 32 '\x00' in
  Bytes.set_uint8 vrf 0 0x01;  (* ~0.0039 *)
  Alcotest.(check bool) "10% stake, low VRF" true
    (Consensus.is_slot_leader params ~vrf_output:vrf ~stake_num:100L ~stake_den:1000L)

let test_half_stake () =
  (* 50% stake: threshold ≈ 1-(0.95)^0.5 ≈ 0.02532
     VRF 0x06... = 6/256 ≈ 0.0234 < 0.02532 → elected *)
  let vrf = Bytes.make 32 '\x00' in
  Bytes.set_uint8 vrf 0 0x06;
  Alcotest.(check bool) "50% stake, VRF 0x06" true
    (Consensus.is_slot_leader params ~vrf_output:vrf ~stake_num:500L ~stake_den:1000L);
  (* VRF 0x07... = 7/256 ≈ 0.0273 > 0.02532 → not elected *)
  Bytes.set_uint8 vrf 0 0x07;
  Alcotest.(check bool) "50% stake, VRF 0x07" false
    (Consensus.is_slot_leader params ~vrf_output:vrf ~stake_num:500L ~stake_den:1000L)

(* ================================================================ *)
(* Chain state                                                       *)
(* ================================================================ *)

let test_genesis_state () =
  let gs = Consensus.genesis_state in
  Alcotest.(check int64) "block" 0L gs.cs_block_number;
  Alcotest.(check int64) "slot" 0L gs.cs_slot;
  Alcotest.(check int64) "epoch" 0L gs.cs_epoch;
  (match gs.cs_tip with Origin -> () | _ -> Alcotest.fail "expected Origin")

let test_chain_state_of_candidate () =
  let c = make_chain '\xab' 500L 500000L in
  let cs = Consensus.chain_state_of_candidate params c in
  Alcotest.(check int64) "block" 500L cs.cs_block_number;
  Alcotest.(check int64) "slot" 500000L cs.cs_slot;
  Alcotest.(check int64) "epoch" 1L cs.cs_epoch  (* 500000 / 432000 = 1 *)

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "Consensus"
    [ ( "Protocol parameters",
        [ Alcotest.test_case "mainnet defaults" `Quick test_mainnet_params ] );
      ( "Epoch/slot arithmetic",
        [ Alcotest.test_case "epoch_of_slot" `Quick test_epoch_of_slot;
          Alcotest.test_case "slot_in_epoch" `Quick test_slot_in_epoch;
          Alcotest.test_case "kes_period" `Quick test_kes_period;
          Alcotest.test_case "kes_key_valid" `Quick test_kes_key_valid;
          Alcotest.test_case "max_kes_slot" `Quick test_max_kes_slot ] );
      ( "Chain selection",
        [ Alcotest.test_case "longer wins" `Quick test_longer_chain_wins;
          Alcotest.test_case "shorter rejected" `Quick test_shorter_chain_rejected;
          Alcotest.test_case "tiebreak smaller hash" `Quick test_equal_length_tiebreak;
          Alcotest.test_case "tiebreak larger rejected" `Quick test_equal_length_tiebreak_reject;
          Alcotest.test_case "equal chains" `Quick test_equal_chains;
          Alcotest.test_case "k-deep fork rejected" `Quick test_k_deep_fork_rejected;
          Alcotest.test_case "exactly k allowed" `Quick test_exactly_k_deep_allowed;
          Alcotest.test_case "origin tiebreak" `Quick test_origin_tiebreak ] );
      ( "Slot leader eligibility",
        [ Alcotest.test_case "zero stake" `Quick test_zero_stake_not_leader;
          Alcotest.test_case "full stake" `Quick test_full_stake_always_leader;
          Alcotest.test_case "low VRF elected" `Quick test_low_vrf_elected;
          Alcotest.test_case "high VRF tiny stake" `Quick test_high_vrf_small_stake;
          Alcotest.test_case "medium stake" `Quick test_medium_stake_threshold;
          Alcotest.test_case "half stake" `Quick test_half_stake ] );
      ( "Chain state",
        [ Alcotest.test_case "genesis" `Quick test_genesis_state;
          Alcotest.test_case "from candidate" `Quick test_chain_state_of_candidate ] );
    ]
