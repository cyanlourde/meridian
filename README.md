# Meridian

An independent Cardano node implementation in OCaml, built entirely from formal specifications.

## What is this?

Meridian is a from-scratch Cardano node written in OCaml. Every module is implemented directly from the Cardano formal ledger specifications, Ouroboros research papers, CDDL schemas, and RFCs — with zero code derived from any existing node implementation.

The goal is a correct, readable, and independently verifiable implementation of the Cardano protocol.

## Current status

**v0.2.0-dev** — Syncing real blocks from the Cardano network with full validation, serving peers, and accepting wallet queries.

| Layer | Status | Description |
|-------|--------|-------------|
| Serialization | Complete | CBOR encoder/decoder (RFC 8949), canonical mode, indefinite-length |
| Cardano types | Complete | All 7 eras: Byron through Conway, 3475 lines |
| Networking | Complete | TCP client/server, Ouroboros mux, segment reassembly, all 10 mini-protocols |
| P2P serving | Complete | Inbound peers, chain-sync server, block-fetch server, block propagation |
| Consensus | Complete | Ouroboros Praos chain selection, VRF slot leader check, epoch arithmetic |
| Cryptography | Partial | Blake2b (pure OCaml + libsodium), Ed25519 (libsodium), VRF/KES stubs |
| Block decoding | Complete | Era-aware deserialization, header/tx/address decoding, opcert extraction |
| Ledger rules | Complete | Full multi-asset UTXO, value conservation, deposits, Plutus collateral |
| Storage | Complete | Content-addressed block store, crash-safe writes, LRU cache, snapshots |
| Sync pipeline | Complete | Chain-sync + block-fetch + storage + validation, resumable |
| Genesis | Complete | Shelley parser, preview/preprod/mainnet configs, UTXO bootstrapping |
| Mempool | Complete | Fee-density ordering, revalidation, expiry, configurable limits |
| Block production | Partial | Forging, opcert signing, slot leader election; VRF/KES pending |
| Local server | Complete | Unix socket, state queries, tx submission, tx monitor |
| Mithril import | Complete | Aggregator API, certificate chain, snapshot import |
| Multi-network | Complete | Preview, preprod, mainnet genesis and configuration |

**554 tests** across 38 test suites. All passing.

### Verified against real Cardano network

- Handshake v15 negotiated with preview testnet relay
- 5400+ blocks synced with **zero validation errors**
- Full validation active: header + Ed25519 crypto + multi-asset UTXO conservation
- Crash-safe resume verified across multiple reconnection cycles

## Architecture

```
lib/
  ── Serialization ──
  cbor.ml                 — CBOR encoder/decoder (RFC 8949)
  cardano_types.ml        — Block, transaction, certificate types for all 7 eras

  ── Cryptography ──
  crypto.ml               — Blake2b (RFC 7693), Ed25519 via libsodium FFI
  libsodium_stubs.c       — C stubs: dlopen loading, Ed25519, Blake2b

  ── Consensus ──
  consensus.ml            — Ouroboros Praos chain selection, slot leader check
  epoch.ml                — Epoch/slot arithmetic, Byron→Shelley transition
  slot_leader.ml          — VRF-based slot leader election
  kes.ml                  — KES key management, opcert signing

  ── Mini-protocols ──
  miniprotocol.ml         — Protocol IDs, agency, handler signature
  mux.ml                  — Ouroboros network multiplexer
  handshake.ml            — Version negotiation (ID 0)
  chain_sync.ml           — Chain synchronization (ID 2)
  block_fetch.ml          — Block range download (ID 3)
  tx_submission.ml        — Transaction submission (ID 4)
  keep_alive.ml           — Connection liveness (ID 8)
  local_chain_sync.ml     — Full-block chain sync (ID 5)
  local_state_query.ml    — Ledger state queries (ID 6)
  local_tx_submission.ml  — Local transaction submission (ID 7)
  local_tx_monitor.ml     — Mempool monitoring (ID 9)

  ── Networking ──
  tcp_connection.ml       — TCP client with DNS, timeouts, error handling
  tcp_server.ml           — TCP listener for inbound peers
  network.ml              — Mux-to-TCP wiring, segment reassembly, keep-alive
  peer_manager.ml         — Peer lifecycle, inbound/outbound tracking

  ── P2P Serving ──
  chain_sync_server.ml    — Serve block headers to peers
  block_fetch_server.ml   — Serve full block bodies to peers
  block_propagation.ml    — Announce new blocks to all peers

  ── Block Processing ──
  block_decoder.ml        — Era-aware block deserialization
  tx_decoder.ml           — Transaction body decoding with all fields
  multi_asset.ml          — Multi-asset value arithmetic
  address.ml              — Address type parsing

  ── Validation ──
  header_validation.ml    — Slot ordering, prev_hash, body hash, sizes
  chain_validation.ml     — Chain integrity, genesis checks
  block_validator.ml      — Opcert Ed25519 verification
  utxo.ml                 — UTXO set, full multi-asset conservation
  ledger_state.ml         — Persistent UTXO state, snapshots

  ── Block Production ──
  mempool.ml              — Transaction pool with fee-density ordering
  block_forge.ml          — Block construction from mempool

  ── Storage & Sync ──
  store.ml                — Content-addressed block store, chain index
  sync_pipeline.ml        — Unified chain-sync + block-fetch + storage
  genesis.ml              — Genesis parser, multi-network configs
  json.ml                 — Minimal JSON parser

  ── Local Server ──
  unix_listener.ml        — Unix domain socket listener
  local_server.ml         — State queries, tx submission, tx monitor

  ── Mithril ──
  mithril_client.ml       — Aggregator API for preview/preprod/mainnet
  mithril_verify.ml       — Certificate chain, digest verification
  mithril_import.ml       — Snapshot unpacking, block import

bin/
  connect.ml              — Handshake test
  sync.ml                 — Full sync pipeline with validation
  inspect.ml              — Block/tx inspection
  validate.ml             — Chain validation
  import.ml               — Mithril snapshot import
  node.ml                 — Full node with sync + local server
```

**12,455 lines** of library code, **9,107 lines** of tests, **46 modules**.

## Build

Requires OCaml >= 4.14 and opam. Libsodium runtime recommended for Ed25519.

```
opam install . --deps-only --with-test
dune build
dune runtest
```

### Quick start

```bash
# Sync from preview testnet with full validation
dune exec bin/sync.exe -- --full-validation --data-dir ./data \
  preview-node.play.dev.cardano.org 3001

# Import from Mithril snapshot (fast bootstrap)
dune exec bin/import.exe -- snapshot --network preview --data-dir ./data

# Inspect a stored block
dune exec bin/inspect.exe -- --data-dir ./data block 100

# Run full node (sync + local server)
dune exec bin/node.exe -- --data-dir ./data
```

## Roadmap

See [docs/ROADMAP.md](docs/ROADMAP.md) for the full breakdown.

## Design principles

- **Spec-driven**: Derived from Cardano CDDL specs, formal ledger specs, Ouroboros papers, and RFCs. Zero code from existing implementations.
- **Original work**: Verifiable by MOSS/JPlag. Every commit includes model attribution.
- **Test-first**: 554 tests with round-trip encoding, state machine validation, known test vectors, pipe simulations, and real-network integration tests.
- **Verified against real network**: 5400+ blocks from Cardano preview testnet with zero validation errors.

## License

Apache-2.0
