# Meridian

An independent Cardano node implementation in OCaml, built entirely from formal specifications.

## What is this?

Meridian is a from-scratch Cardano node written in OCaml. Every module is implemented directly from the Cardano formal ledger specifications, Ouroboros research papers, CDDL schemas, and RFCs — with zero code derived from any existing node implementation.

## Current status

**v0.2.0-dev** — Syncing, validating, serving, and producing blocks.

| Layer | Status |
|-------|--------|
| Serialization | Complete — CBOR encoder/decoder (RFC 8949) |
| Cardano types | Complete — All 7 eras (Byron through Conway) |
| Networking | Complete — TCP client/server, mux, all 10 mini-protocols |
| P2P | Complete — Inbound/outbound peers, chain-sync/block-fetch serving, topology, scoring |
| Consensus | Complete — Ouroboros Praos chain selection, VRF leader check, epoch arithmetic |
| Cryptography | Complete — Blake2b, Ed25519, KES sum composition, VRF (Cardano fork) |
| Block decoding | Complete — All eras, header/tx/address, opcert extraction |
| Ledger | Complete — Multi-asset UTXO, conservation, deposits, Plutus collateral |
| Governance | Complete — DReps, proposals, voting, treasury (CIP-1694) |
| Storage | Complete — Content-addressed store, crash-safe writes, snapshots |
| Sync pipeline | Complete — Chain-sync + block-fetch + validation, resumable |
| Mempool | Complete — Fee-density ordering, revalidation, expiry |
| Block production | Complete — Forging, KES signing, opcert, VRF leader election |
| Local server | Complete — Unix socket, state queries, tx submission |
| Mithril | Complete — Aggregator API, certificate chain, snapshot import |
| Multi-network | Complete — Preview, preprod, mainnet genesis and config |

**635 tests** across 43 suites. **14,029 lines** of code. **50 library modules**.

### Verified against real Cardano network

- Private testnet: 3 Haskell nodes + 1 Meridian node running in consensus, zero slot difference over 1000+ blocks
- Preview sync: 170K+ blocks synced and counting, full chain-sync pipeline
- Mainnet: Mithril snapshot import in progress (full immutable DB)
- Handshake v13-v15 with preview, preprod, and private testnet relays
- Full validation: header + Ed25519 crypto + multi-asset UTXO conservation
- Crash-safe resume across multiple reconnection cycles

## Architecture

```
lib/  (50 modules, 14,029 lines)
  cbor.ml                 — CBOR (RFC 8949)
  cardano_types.ml        — All 7 eras (3,475 lines)
  crypto.ml + stubs.c     — Blake2b, Ed25519, VRF, KES via libsodium
  consensus.ml            — Ouroboros Praos chain selection
  epoch.ml                — Byron→Shelley epoch arithmetic
  slot_leader.ml          — VRF-based leader election
  kes.ml                  — KES sum composition (real Ed25519)
  multi_asset.ml          — Multi-asset value arithmetic
  governance.ml           — Conway governance (CIP-1694)
  miniprotocol.ml         — Protocol IDs, agency
  mux.ml                  — Network multiplexer
  handshake.ml .. keep_alive.ml — 5 n2n mini-protocols
  local_*.ml              — 4 n2c mini-protocols
  tcp_connection.ml       — TCP client
  tcp_server.ml           — TCP listener
  network.ml              — Segment reassembly, keep-alive
  peer_manager.ml         — Peer lifecycle
  peer_discovery.ml       — Dynamic connections
  peer_scoring.ml         — Reliability scoring
  topology.ml             — Topology config
  chain_sync_server.ml    — Serve headers
  block_fetch_server.ml   — Serve blocks
  block_propagation.ml    — Announce blocks
  block_decoder.ml        — Era-aware decoding
  tx_decoder.ml           — Transaction parsing
  address.ml              — Address types
  header_validation.ml    — Header checks
  chain_validation.ml     — Chain integrity
  block_validator.ml      — Crypto verification
  utxo.ml                 — UTXO set + conservation
  ledger_state.ml         — Persistent ledger + snapshots
  mempool.ml              — Transaction pool
  block_forge.ml          — Block construction
  store.ml                — Block storage
  sync_pipeline.ml        — Unified sync
  genesis.ml              — Multi-network genesis
  json.ml                 — JSON parser
  mithril_*.ml            — Snapshot import
  unix_listener.ml        — Unix socket
  local_server.ml         — State queries

bin/  (698 lines)
  sync.ml    — Full pipeline with validation
  node.ml    — Full node
  import.ml  — Mithril import
  inspect.ml — Block inspection
  validate.ml — Chain validation
  connect.ml — Handshake test

test/ (42 suites, 9,740 lines)
```

## Build

```
opam install . --deps-only --with-test
dune build
dune runtest   # 635 tests
```

### Quick start

```bash
# Sync preview testnet with full validation
dune exec bin/sync.exe -- --full-validation --data-dir ./data

# Import Mithril snapshot
dune exec bin/import.exe -- snapshot --network preview --data-dir ./data

# Run full node
dune exec bin/node.exe -- --data-dir ./data
```

## Design principles

- **Spec-driven**: From Cardano CDDL, formal ledger specs, Ouroboros papers, RFCs
- **Original**: Verifiable by MOSS/JPlag. Every commit attributed.
- **Test-first**: 635 tests. Real-network integration verified.
- **Verified**: Private testnet consensus, 170K+ preview blocks, mainnet import.

## License

Apache-2.0
