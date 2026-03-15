# Meridian

An independent Cardano node implementation in OCaml, built entirely from formal specifications.

## What is this?

Meridian is a from-scratch Cardano node written in OCaml. Every module is implemented directly from the Cardano formal ledger specifications, Ouroboros research papers, CDDL schemas, and RFCs — with zero code derived from any existing node implementation.

The goal is a correct, readable, and independently verifiable implementation of the Cardano protocol.

## Current status

**v0.2.0-dev** — Syncing real blocks from the Cardano network with full validation.

| Layer | Status | Description |
|-------|--------|-------------|
| Serialization | Complete | CBOR encoder/decoder (RFC 8949), canonical mode, indefinite-length items |
| Cardano types | Complete | All 7 eras: Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway |
| Networking | Complete | TCP connections, Ouroboros multiplexer, segment reassembly, all 10 mini-protocols |
| Consensus | Complete | Ouroboros Praos chain selection, VRF slot leader check, epoch arithmetic |
| Cryptography | Partial | Blake2b-256/224 (pure OCaml + libsodium), Ed25519 verify/sign (libsodium), VRF/KES stubs |
| Block decoding | Complete | Era-aware deserialization, header/tx/address decoding, opcert extraction |
| Ledger rules | Partial | UTXO validation, value conservation, deposits/withdrawals, Plutus collateral |
| Storage | Complete | Content-addressed block store, chain index, crash-safe writes, LRU cache, snapshots |
| Sync pipeline | Complete | Chain-sync + block-fetch + storage + validation, resumable, keep-alive |
| Genesis | Complete | Shelley genesis parser, UTXO bootstrapping, protocol params |

**472 tests** across 30 test suites. All passing.

### Verified against real Cardano network

- Handshake v15 negotiated with preview testnet relay
- 2950+ blocks synced with **zero validation errors**
- Full validation pipeline active: header checks, Ed25519 opcert verification, UTXO conservation
- Crash-safe resume verified across 8 reconnection cycles

## Architecture

```
lib/
  ── Serialization ──
  cbor.ml                 — CBOR encoder/decoder (RFC 8949)
  cardano_types.ml        — Block, transaction, certificate types for all 7 eras (3475 lines)

  ── Cryptography ──
  crypto.ml               — Blake2b (RFC 7693), Ed25519 via libsodium FFI
  libsodium_stubs.c       — C stubs: dlopen loading, Ed25519, Blake2b

  ── Consensus ──
  consensus.ml            — Ouroboros Praos chain selection, slot leader check
  epoch.ml                — Epoch/slot arithmetic, Byron→Shelley transition

  ── Mini-protocols (node-to-node) ──
  miniprotocol.ml         — Protocol IDs, agency, handler signature
  mux.ml                  — Ouroboros network multiplexer (segment framing)
  handshake.ml            — Version negotiation (ID 0)
  chain_sync.ml           — Chain synchronization (ID 2)
  block_fetch.ml          — Block range download (ID 3)
  tx_submission.ml        — Transaction submission (ID 4)
  keep_alive.ml           — Connection liveness (ID 8)

  ── Mini-protocols (node-to-client) ──
  local_chain_sync.ml     — Full-block chain sync (ID 5)
  local_state_query.ml    — Ledger state queries (ID 6)
  local_tx_submission.ml  — Local transaction submission (ID 7)
  local_tx_monitor.ml     — Mempool monitoring (ID 9)

  ── Networking ──
  tcp_connection.ml       — TCP client with DNS, timeouts, error handling
  network.ml              — Mux-to-TCP wiring, segment reassembly, keep-alive responder

  ── Block processing ──
  block_decoder.ml        — Era-aware block deserialization (7 eras)
  tx_decoder.ml           — Transaction body decoding with all value-affecting fields
  address.ml              — Address type parsing and credential extraction

  ── Validation ──
  header_validation.ml    — Slot ordering, prev_hash, block_number, body hash, sizes
  chain_validation.ml     — Chain integrity, genesis checks, break detection
  block_validator.ml      — Opcert Ed25519 verification, key/sig size checks
  utxo.ml                 — UTXO set, transaction validation, value conservation
  ledger_state.ml         — Persistent UTXO state, snapshots, era-aware params

  ── Storage & sync ──
  store.ml                — Content-addressed block store, chain index, LRU cache
  sync_pipeline.ml        — Unified chain-sync + block-fetch + storage pipeline
  genesis.ml              — Shelley genesis parser, UTXO bootstrapping
  json.ml                 — Minimal JSON parser (zero dependencies)

bin/
  connect.ml              — Handshake test
  sync.ml                 — Full sync pipeline with validation
  inspect.ml              — Block/tx inspection
  validate.ml             — Chain validation
```

**10,778 lines** of library code, **7,606 lines** of tests, **31 modules**.

## Build

Requires OCaml >= 4.14 and opam. Libsodium runtime library recommended for Ed25519 verification.

```
opam install . --deps-only --with-test
dune build
dune runtest
```

### Sync from Cardano preview testnet

```bash
# Basic sync (headers + block bodies stored to disk)
dune exec bin/sync.exe -- --data-dir ./data preview-node.play.dev.cardano.org 3001

# Full validation (header + crypto + UTXO checks on every block)
dune exec bin/sync.exe -- --full-validation --data-dir ./data preview-node.play.dev.cardano.org 3001

# Inspect a stored block
dune exec bin/inspect.exe -- --data-dir ./data block 100
```

## Roadmap

Phases 1-5 substantially complete. Next:

- **Phase 4** (remaining) — Full multi-asset UTXO tracking, Plutus script cost model validation, complete era-specific ledger rules
- **Phase 6** — Block production: VRF proofs, KES signing, mempool, block forging
- **Phase 7** — Integration: full genesis-to-tip sync, Haskell node interop testing, stability runs

See [docs/ROADMAP.md](docs/ROADMAP.md) for the full breakdown.

## Design principles

- **Spec-driven**: Every type, encoding, and state machine is derived from the Cardano CDDL specs, formal ledger specifications, Ouroboros papers, and relevant RFCs. No code is copied or adapted from existing implementations.
- **Original work**: All code is original and verifiable by plagiarism detection tools (MOSS/JPlag). Every commit includes model attribution in the commit message.
- **Test-first**: Each module ships with comprehensive round-trip tests, state machine validation, known test vectors, and pipe-based protocol simulations.
- **Type-safe**: OCaml's type system enforces protocol invariants — era-specific transaction bodies, agency rules, credential types.
- **Verified against real network**: Synced thousands of blocks from Cardano preview testnet with all validation layers active and zero errors.

## License

Apache-2.0
