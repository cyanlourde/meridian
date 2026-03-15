# Meridian Roadmap

## Phase 1 — Foundation (Weeks 1-6)
- [ ] CBOR encoding/decoding library (from CDDL spec)
- [ ] Basic binary serialization primitives
- [ ] Cardano address parsing
- [ ] Block/transaction type definitions (all eras)
- [ ] Unit tests against known test vectors

## Phase 2 — Networking (Weeks 4-10)
- [ ] Ouroboros multiplexer (mini-protocol framing)
- [ ] Handshake mini-protocol
- [ ] Chain-sync mini-protocol (node-to-node)
- [ ] Block-fetch mini-protocol
- [ ] Tx-submission mini-protocol (node-to-node)
- [ ] Keep-alive mini-protocol
- [ ] Chain-sync mini-protocol (node-to-client)
- [ ] State-query mini-protocol (node-to-client)
- [ ] Tx-submission mini-protocol (node-to-client)
- [ ] Tx-monitor mini-protocol (node-to-client)

## Phase 3 — Consensus (Weeks 8-16)
- [ ] Ouroboros Praos chain selection
- [ ] VRF verification (libsodium bindings)
- [ ] KES signature verification
- [ ] Slot leader check
- [ ] Epoch transition logic

## Phase 4 — Ledger (Weeks 6-24)
- [ ] Byron era ledger rules
- [ ] Shelley era ledger rules
- [ ] Allegra era ledger rules
- [ ] Mary era ledger rules
- [ ] Alonzo era ledger rules (Plutus)
- [ ] Babbage era ledger rules
- [ ] Conway era ledger rules (governance)
- [ ] Transaction validation
- [ ] Block validation
- [ ] UTXO set management

## Phase 5 — Storage & Recovery (Weeks 16-28)
- [ ] On-disk chain storage (immutable + volatile DB)
- [ ] UTXO state snapshots
- [ ] Crash recovery (fsync discipline)
- [ ] Mithril snapshot import

## Phase 6 — Block Production (Weeks 24-32)
- [ ] Operational certificate handling
- [ ] VRF proof generation
- [ ] KES signing
- [ ] Block forging
- [ ] Mempool management

## Phase 7 — Integration (Weeks 28-36)
- [ ] Sync from genesis to tip
- [ ] Private testnet with Haskell nodes
- [ ] Preview/preprod block production
- [ ] 10-day stability run
- [ ] Memory profiling and optimization
