# Meridian Roadmap

## Phase 1 — Foundation
- [x] CBOR encoding/decoding library (RFC 8949)
- [x] Cardano address parsing
- [x] Block/transaction type definitions (all 7 eras)
- [x] Unit tests against known test vectors

## Phase 2 — Networking
- [x] Ouroboros multiplexer (mini-protocol framing)
- [x] All 10 mini-protocols (5 n2n + 5 n2c) with state machines
- [x] TCP connections with DNS, timeouts, reconnection
- [x] Per-protocol segment reassembly
- [x] Keep-alive auto-responder
- [x] Unified sync pipeline (chain-sync + block-fetch + storage)

## Phase 3 — Consensus
- [x] Ouroboros Praos chain selection (k-deep fork rule)
- [x] Slot leader election (VRF threshold, deterministic test mode)
- [x] KES key management with period tracking
- [x] Epoch arithmetic with Byron→Shelley transition
- [x] Opcert Ed25519 signing and verification

## Phase 4 — Ledger
- [x] Transaction validation (UTXO rules per Shelley formal spec)
- [x] Block header validation (slot ordering, prev_hash, body hash, sizes)
- [x] Full multi-asset UTXO tracking (policy_id → asset_name → quantity)
- [x] Value conservation (inputs + withdrawals + refunds + mint = outputs + fee + deposits)
- [x] Certificate deposit/refund tracking (stake reg/dereg, pool reg/retirement)
- [x] Failed Plutus transaction handling (is_valid=false, collateral)
- [x] Block crypto validation (opcert Ed25519 via libsodium)
- [x] Genesis configuration parsing and UTXO bootstrapping
- [x] Multi-network support (preview, preprod, mainnet)
- [ ] Full Plutus script cost model validation
- [ ] Complete Conway-era governance ledger rules

## Phase 5 — Storage & Recovery
- [x] On-disk chain storage (content-addressed, append-only index)
- [x] UTXO state snapshots (crash-safe binary format)
- [x] Crash recovery (atomic writes, snapshot restore)
- [x] Resumable sync (chain intersection points)
- [x] Mithril snapshot import (aggregator API, certificate chain, chunk import)

## Phase 6 — Block Production
- [x] Block forging from mempool (fee-density selection, conflict detection)
- [x] Transaction mempool (revalidation, expiry, ordering)
- [x] Operational certificate Ed25519 signing
- [x] VRF slot leader election (deterministic test mode)
- [ ] Real VRF proof generation (requires Cardano libsodium fork)
- [ ] Real KES signing (requires Cardano libsodium fork)
- [ ] Full block production loop with slot timing

## Phase 7 — P2P & Serving
- [x] TCP server for inbound peer connections
- [x] Peer manager with lifecycle management
- [x] Chain-sync server (serve headers to peers)
- [x] Block-fetch server (serve block bodies in ranges)
- [x] Block propagation to connected peers
- [x] Unix domain socket for local clients
- [x] State query server (era, epoch, UTXO by address/txin, protocol params)
- [x] Local tx submission with UTXO validation
- [x] Local tx monitor (mempool snapshots)
- [ ] Tx-submission server (receive txs from peers)
- [ ] Full peer discovery and gossip

## Phase 8 — Integration
- [x] Preview testnet sync (5400+ blocks, zero validation errors)
- [x] Real network handshake and protocol negotiation
- [x] Crash-safe resume across multiple restarts
- [ ] Full genesis-to-tip sync (millions of blocks)
- [ ] Mainnet Mithril snapshot import at scale
- [ ] Private testnet with Haskell nodes
- [ ] 10-day stability run
