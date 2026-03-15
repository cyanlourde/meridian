# Meridian Roadmap

## Phase 1 — Foundation (Weeks 1-6)
- [x] CBOR encoding/decoding library (from CDDL spec)
- [x] Basic binary serialization primitives
- [x] Cardano address parsing
- [x] Block/transaction type definitions (all eras)
- [x] Unit tests against known test vectors

## Phase 2 — Networking (Weeks 4-10)
- [x] Ouroboros multiplexer (mini-protocol framing)
- [x] Handshake mini-protocol
- [x] Chain-sync mini-protocol (node-to-node)
- [x] Block-fetch mini-protocol
- [x] Tx-submission mini-protocol (node-to-node)
- [x] Keep-alive mini-protocol
- [x] Chain-sync mini-protocol (node-to-client)
- [x] State-query mini-protocol (node-to-client)
- [x] Tx-submission mini-protocol (node-to-client)
- [x] Tx-monitor mini-protocol (node-to-client)
- [x] TCP connection layer with DNS, timeouts, reconnection
- [x] Per-protocol segment reassembly (Proto_buffer)
- [x] Concurrent keep-alive auto-responder
- [x] Unified sync pipeline (chain-sync + block-fetch + storage)

## Phase 3 — Consensus (Weeks 8-16)
- [x] Ouroboros Praos chain selection
- [x] VRF verification — stub, awaiting Cardano-specific libsodium fork
- [x] KES signature verification — stub, awaiting KES library
- [x] Slot leader check
- [x] Epoch transition logic
- [x] Epoch arithmetic with Byron→Shelley transition

## Phase 4 — Ledger (Weeks 6-24)
- [x] Transaction validation (UTXO rules per Shelley formal spec)
- [x] Block header validation (slot ordering, prev_hash, body hash, sizes)
- [x] UTXO set management (functional map, add/remove/find)
- [x] Value conservation (inputs + withdrawals + refunds = outputs + fee + deposits)
- [x] Certificate deposit/refund tracking (stake reg/dereg, pool reg/retirement)
- [x] Failed Plutus transaction handling (is_valid=false, collateral)
- [x] Block crypto validation (opcert Ed25519 via libsodium)
- [x] Genesis configuration parsing and UTXO bootstrapping
- [ ] Full multi-asset UTXO tracking (currently lovelace-only with multi-asset warnings)
- [ ] Plutus script cost model validation
- [ ] Complete Conway-era governance ledger rules
- [ ] Byron era ledger rules

## Phase 5 — Storage & Recovery (Weeks 16-28)
- [x] On-disk chain storage (content-addressed, append-only index)
- [x] UTXO state snapshots (crash-safe binary format)
- [x] Crash recovery (atomic writes, snapshot restore)
- [x] Resumable sync (chain intersection points from stored data)
- [ ] Mithril snapshot import

## Phase 6 — Block Production (Weeks 24-32)
- [ ] Operational certificate handling
- [ ] VRF proof generation
- [ ] KES signing
- [ ] Block forging
- [ ] Mempool management

## Phase 7 — Integration (Weeks 28-36)
- [x] Sync from genesis (preview testnet, 2950+ blocks validated)
- [x] Real network handshake and protocol negotiation
- [ ] Full genesis-to-tip sync (millions of blocks)
- [ ] Private testnet with Haskell nodes
- [ ] Preview/preprod block production
- [ ] 10-day stability run
- [ ] Memory profiling and optimization
