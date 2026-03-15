# Meridian

An independent Cardano node implementation in OCaml, built entirely from formal specifications.

## What is this?

Meridian is a from-scratch Cardano node written in OCaml. Every module is implemented directly from the Cardano formal ledger specifications, Ouroboros research papers, CDDL schemas, and RFCs — with zero code derived from any existing node implementation.

The goal is a correct, readable, and independently verifiable implementation of the Cardano protocol.

## Current status

**v0.1.0-dev** — Foundation, networking, and consensus layers are implemented.

| Layer | Status | Modules |
|-------|--------|---------|
| Serialization | Complete | CBOR encoder/decoder (RFC 8949), canonical mode, indefinite-length items |
| Cardano types | Complete | All eras: Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway |
| Networking | Complete | Ouroboros multiplexer + all 10 mini-protocols (5 node-to-node, 5 node-to-client) |
| Consensus | Complete | Ouroboros Praos chain selection, VRF slot leader check, epoch arithmetic |
| Cryptography | Partial | Blake2b-256/224 (pure OCaml), Ed25519/VRF/KES stubs awaiting libsodium |
| Ledger rules | Not started | Transaction/block validation, UTXO management |
| Storage | Not started | On-disk chain database, snapshots |

**292 tests** across 14 test suites. All passing.

## Architecture

```
lib/
  cbor.ml                 — CBOR encoder/decoder (RFC 8949)
  cardano_types.ml        — Block, transaction, certificate types for all 7 eras
  crypto.ml               — Blake2b (RFC 7693), Ed25519/VRF/KES verification
  consensus.ml            — Ouroboros Praos chain selection, slot leader check
  miniprotocol.ml         — Mini-protocol IDs, agency, handler signature
  mux.ml                  — Ouroboros network multiplexer (segment framing)
  handshake.ml            — Version negotiation (mini-protocol ID 0)
  chain_sync.ml           — Chain synchronization (ID 2, node-to-node)
  block_fetch.ml          — Block range download (ID 3)
  tx_submission.ml        — Transaction submission (ID 4, node-to-node)
  keep_alive.ml           — Connection liveness (ID 8)
  local_chain_sync.ml     — Full-block chain sync (ID 5, node-to-client)
  local_state_query.ml    — Ledger state queries (ID 6)
  local_tx_submission.ml  — Local transaction submission (ID 7)
  local_tx_monitor.ml     — Mempool monitoring (ID 9)
```

Each mini-protocol module includes the complete state machine, all message types with CBOR encoding/decoding, agency validation, and byte serialization.

## Build

Requires OCaml >= 4.14 and opam.

```
opam install . --deps-only --with-test
dune build
dune runtest
```

Or using the Makefile:

```
make build
make test
make clean
```

## Roadmap

Phases 1-3 are complete (foundation, networking, consensus). Next up:

- **Phase 4** — Ledger rules: transaction validation, block validation, UTXO set for all eras
- **Phase 5** — Storage: immutable/volatile chain DB, crash recovery, Mithril import
- **Phase 6** — Block production: VRF proofs, KES signing, mempool, block forging
- **Phase 7** — Integration: genesis sync, testnet interop, stability testing

See [docs/ROADMAP.md](docs/ROADMAP.md) for the full breakdown.

## Design principles

- **Spec-driven**: Every type, encoding, and state machine is derived from the Cardano CDDL specs, formal ledger specifications, Ouroboros papers, and relevant RFCs. No code is copied or adapted from existing implementations.
- **Original work**: All code is original and verifiable by plagiarism detection tools (MOSS/JPlag). Every commit includes model attribution in the commit message.
- **Test-first**: Each module ships with comprehensive round-trip tests, state machine validation, and known test vectors.
- **Type-safe**: OCaml's type system enforces protocol invariants — era-specific transaction bodies, agency rules, credential types.

## License

Apache-2.0
