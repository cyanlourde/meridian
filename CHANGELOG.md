# Meridian Changelog

## [0.2.0-dev] - 2026-03-15

### Added

#### TCP Networking
- TCP client with DNS resolution, non-blocking connect with timeout, send/recv with partial I/O handling, clean shutdown (lib/tcp_connection.ml)
- Network layer wiring mux to TCP with per-protocol segment reassembly via Proto_buffer (lib/network.ml)
- Live chain-sync, live block-fetch, keep-alive auto-responder — all transparent to callers

#### Sync Pipeline
- Unified chain-sync + block-fetch + storage in one loop with batch processing, at-tip live mode, rollback handling, error recovery on disconnect, resumable via chain intersection points (lib/sync_pipeline.ml)

#### On-Disk Storage
- Content-addressed block store with crash-safe atomic writes, append-only chain index, LRU tip cache (lib/store.ml)

#### Block & Transaction Decoding
- Era-aware block decoder (Byron through Conway), header extraction with all crypto fields, per-era transaction decoder with full multi-asset values (lib/block_decoder.ml, lib/tx_decoder.ml, lib/address.ml)

#### Validation
- Header validation, chain integrity, epoch arithmetic, UTXO set with full multi-asset conservation (withdrawals, deposits, Plutus collateral), ledger state with snapshots, block crypto validation via libsodium Ed25519

#### Genesis & Multi-Network
- Shelley genesis JSON parser, embedded configs for preview/preprod/mainnet, full Byron-to-Shelley epoch transition arithmetic, per-network aggregator URLs and node addresses

#### Libsodium FFI
- Runtime dlopen: Ed25519 sign/verify/keypair, Blake2b cross-check — no compile-time dependency

#### Multi-Asset UTXO
- Full value type with policy_id → asset_name → quantity, add/subtract/equal, CBOR parsing for outputs and mint fields, exact conservation equation: consumed + mint = produced

#### Mempool
- Transaction pool with fee-density ordering, configurable size/count limits, duplicate rejection, revalidation on new blocks (evict spent inputs), expiry of stale transactions, point-in-time snapshots for tx-monitor (lib/mempool.ml)

#### Block Production
- Block forging from mempool with conflict detection, size limits, body hash computation (lib/block_forge.ml)
- VRF slot leader election with deterministic test mode (lib/slot_leader.ml)
- KES key lifecycle with period tracking, opcert Ed25519 signing (lib/kes.ml)
- Full block CBOR round-trip: forge → encode → decode → verify

#### P2P Networking
- TCP server for inbound peer connections with non-blocking accept (lib/tcp_server.ml)
- Peer manager: register/remove/evict lifecycle, inbound/outbound tracking, max peer limits (lib/peer_manager.ml)
- Chain-sync server: serve block headers to peers from store (lib/chain_sync_server.ml)
- Block-fetch server: serve full block bodies in range batches (lib/block_fetch_server.ml)
- Block propagation: announce new blocks to all connected peers (lib/block_propagation.ml)

#### Local Node-to-Client Server
- Unix domain socket listener with stale socket cleanup (lib/unix_listener.ml)
- State query server: GetCurrentEra, GetEpochNo, GetUTxOByAddress, GetUTxOByTxIn, GetProtocolParameters (lib/local_server.ml)
- Local tx submission with UTXO validation
- Local tx monitor with mempool snapshots (empty mempool for now)
- UTXO address lookup: find_by_address, find_by_txins (lib/utxo.ml)

#### Mithril Snapshot Import
- Aggregator API client for preview/preprod/mainnet (lib/mithril_client.ml)
- Certificate chain hash verification, snapshot digest verification (lib/mithril_verify.ml)
- Immutable DB chunk parsing and import into store (lib/mithril_import.ml)

#### CLI Tools
- `connect` — handshake test
- `sync` — full pipeline with --full-validation, --network, --data-dir, --max-blocks
- `validate` — chain/tip/block validation
- `inspect` — block/header/tx pretty-printing
- `import` — Mithril snapshot import
- `node` — full node with sync + local server + Unix socket

### Verified against real Cardano network
- Handshake v15 negotiated with preview testnet relay
- 5400+ blocks synced with zero validation errors (full multi-asset conservation)
- 9+ automatic reconnections, all resumed correctly from snapshot
- Full validation pipeline: header + crypto + multi-asset UTXO on every block
- 554 tests across 38 suites

## [0.1.0-dev] - 2026-03-15

### Added
- CBOR encoder/decoder implementing RFC 8949 (all major types, canonical mode, indefinite length)
- Cardano block and transaction types for all eras (Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway)
- Ouroboros network multiplexer (segment framing, protocol ID handling, direction bit)
- All 9 node-to-node + node-to-client mini-protocols with state machines and CBOR encoding
- Ouroboros Praos consensus: chain selection, VRF slot leader check, epoch/slot arithmetic
- Cryptographic primitives: Blake2b-256/224 (pure OCaml, RFC 7693), Ed25519/VRF/KES stubs
- 292 tests across 14 test suites
