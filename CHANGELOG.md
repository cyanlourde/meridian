# Meridian Changelog

## [0.2.0-dev] - 2026-03-15

### Added

#### TCP Networking
- TCP client with DNS resolution, non-blocking connect with timeout, send/recv with partial I/O handling, clean shutdown (lib/tcp_connection.ml)
- Network layer wiring mux to TCP with per-protocol segment reassembly via Proto_buffer — handles multi-message segments and cross-segment messages correctly (lib/network.ml)
- Live chain-sync: FindIntersect, RequestNext, RollForward/RollBackward/AwaitReply with automatic non-chain-sync segment skipping (lib/network.ml)
- Live block-fetch: RequestRange, StartBatch/Block/BatchDone with tag-24 CBOR-in-CBOR unwrapping for real Cardano blocks (lib/network.ml)
- Keep-alive auto-responder: transparent to callers, answers MsgKeepAlive pings during chain-sync and block-fetch, tracks ping stats (lib/network.ml)

#### Sync Pipeline
- Unified chain-sync + block-fetch + storage in one loop with configurable batch size, at-tip live mode, rollback handling, error recovery on disconnect (lib/sync_pipeline.ml)
- Resumable: stop at any point, restart, continues from last stored slot via chain intersection points

#### On-Disk Storage
- Content-addressed block store: blocks/<xx>/<hash>.block layout (lib/store.ml)
- Append-only chain index: 8-byte slot + 32-byte hash per entry, binary format
- Crash-safe atomic writes: temp file + Unix.rename on all file operations
- LRU tip cache: most recent k=2160 blocks in memory
- Chain intersection points: tip, tip-10, tip-100, tip-1000, tip-2160, origin

#### Block & Transaction Decoding
- Era-aware block decoder: Byron through Conway (era tags 0-6), tag-24 CBOR-in-CBOR unwrapping (lib/block_decoder.ml)
- Header extraction: slot, block_number, prev_hash, issuer_vkey, body_hash, protocol_version, VRF vkey, KES signature, operational certificate
- Transaction decoder: per-era CBOR map parsing with inputs, outputs, fees, TTL, certificates (with deposit types), withdrawals, mint, collateral inputs/return/total, is_valid flag (lib/tx_decoder.ml)
- Address decoding: Base, Pointer, Enterprise, Byron, Reward types with credential extraction (lib/address.ml)

#### Validation
- Header validation: slot ordering, prev_hash linkage, block_number sequence, body hash, protocol version ranges per era, header/block size limits (lib/header_validation.ml)
- Chain validation: genesis block checks, chain integrity over stored sequences, chain break detection (lib/chain_validation.ml)
- Epoch arithmetic: slot-to-epoch conversion with Byron-to-Shelley transition, epoch boundaries, preview and mainnet parameters (lib/epoch.ml)
- UTXO set: functional map-based with O(log n) lookups, add/remove/find/iter/total_lovelace (lib/utxo.ml)
- Transaction validation per Shelley formal ledger spec: input existence, duplicate inputs, fee minimum, value conservation (with withdrawals, deposits, refunds), output minimum, TTL expiry (lib/utxo.ml)
- Value conservation: withdrawals, stake reg/dereg deposits (key_deposit=2 ADA), pool reg/retirement deposits (pool_deposit=500 ADA), Conway-era cert deposits, is_valid=false Plutus collateral handling, multi-asset tolerance as warnings (lib/utxo.ml)
- Ledger state: persistent UTXO with era-aware protocol params, binary snapshots with atomic write, restore, genesis bootstrap skip (lib/ledger_state.ml)
- Block crypto validation: opcert Ed25519 signature verification via libsodium, key/signature size checks, VRF vkey validation (lib/block_validator.ml)

#### Genesis
- Shelley genesis JSON parser: networkMagic, epochLength, protocolParams, initialFunds (lib/genesis.ml)
- Genesis UTXO derivation: tx_hash = Blake2b-256(address_bytes), tx_index = 0
- Embedded preview testnet defaults: magic=2, epochLength=86400
- Minimal JSON parser: zero-dependency recursive descent (lib/json.ml)

#### Libsodium FFI
- Runtime dlopen loading: no compile-time libsodium-dev dependency (lib/libsodium_stubs.c)
- Ed25519: sign, verify, keypair generation via crypto_sign_ed25519_*
- Blake2b: libsodium fast path with cross-check against pure OCaml implementation
- Graceful fallback: all crypto functions return clear errors if libsodium unavailable

#### CLI Tools
- `connect` — handshake test against any Cardano node
- `sync` — full pipeline with --full-validation, --data-dir, --max-blocks, --batch-size
- `validate` — chain/tip/block validation from stored data
- `inspect` — block/header/tx pretty-printing from stored data
- `integration_test.sh` — automated multi-phase sync + resume verification

### Verified against real Cardano network
- Handshake v15 negotiated with preview-node.play.dev.cardano.org:3001
- 2950+ blocks synced with zero validation errors
- 8 automatic reconnections, all resumed correctly from snapshot
- Full validation pipeline: header + crypto + UTXO on every block
- 472 tests across 30 suites

## [0.1.0-dev] - 2026-03-15

### Added
- CBOR encoder/decoder implementing RFC 8949 (all major types, canonical mode, indefinite length)
- Cardano block and transaction types for all eras (Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway)
- Ouroboros network multiplexer (segment framing, protocol ID handling, direction bit)
- Node-to-node mini-protocols: handshake, chain-sync, block-fetch, tx-submission, keep-alive
- Node-to-client mini-protocols: local chain-sync, local state-query, local tx-submission, local tx-monitor
- Ouroboros Praos consensus: chain selection (k-deep fork rule), slot leader check (VRF threshold), epoch/slot arithmetic
- Cryptographic primitives: Blake2b-256/224 (pure OCaml, RFC 7693), Ed25519/VRF/KES type stubs
- 292 tests across 14 test suites
