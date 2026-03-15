# Meridian Roadmap

## Phase 1 — Foundation ✅
- [x] CBOR (RFC 8949), Cardano types (all 7 eras), address parsing, test vectors

## Phase 2 — Networking ✅
- [x] Ouroboros multiplexer, all 10 mini-protocols, TCP client/server
- [x] Segment reassembly, keep-alive, unified sync pipeline

## Phase 3 — Consensus ✅
- [x] Ouroboros Praos chain selection, VRF leader election, KES sum composition
- [x] Epoch arithmetic (Byron→Shelley), opcert Ed25519 signing

## Phase 4 — Ledger ✅
- [x] Full multi-asset UTXO with exact conservation equation
- [x] Deposits: stake/pool/DRep reg/dereg, governance proposals
- [x] Failed Plutus handling (is_valid=false, collateral)
- [x] Block header/crypto validation (Ed25519 opcert via libsodium)
- [x] Genesis parsing, multi-network support
- [x] Conway governance: DReps, proposals, voting, treasury (CIP-1694)
- [ ] Plutus script cost model validation (requires UPLC interpreter)
- [ ] Stake distribution tracking (for governance ratification)
- [ ] Protocol parameter updates via governance enactment

## Phase 5 — Storage & Recovery ✅
- [x] Content-addressed block store, crash-safe writes, snapshots
- [x] Resumable sync via chain intersection points
- [x] Mithril snapshot import (aggregator API, certificate chain, chunk import)

## Phase 6 — Block Production ✅
- [x] Block forging from mempool (fee-density, conflict detection, size limits)
- [x] Transaction mempool (revalidation, expiry, ordering)
- [x] KES signing (real sum composition, Ed25519 at leaves)
- [x] VRF (Cardano libsodium fork via dlopen, graceful fallback)
- [x] Opcert Ed25519 signing
- [ ] Full production loop with slot timing and network broadcast

## Phase 7 — P2P & Serving ✅
- [x] TCP server, peer manager, chain-sync server, block-fetch server
- [x] Block propagation, topology parsing, peer scoring, dynamic discovery
- [x] Unix socket local server: state queries, tx submission, tx monitor
- [ ] Tx-submission server (receive txs from peers via mini-protocol)
- [ ] Peer sharing mini-protocol (gossip)

## Phase 8 — Integration
- [x] Preview testnet sync (5400+ blocks, zero errors)
- [x] Real network handshake + protocol negotiation
- [x] Crash-safe resume across restarts
- [ ] Full mainnet Mithril import at scale
- [ ] Genesis-to-tip sync (millions of blocks)
- [ ] Private testnet with Haskell nodes
- [ ] 10-day stability run

## Summary
- **592 tests** across 42 suites
- **23,629 lines** of OCaml + C
- **50 library modules**
- Phases 1-7 substantially complete
