# Meridian Changelog

## [0.2.1-dev] - 2026-03-16

### Security
- Mithril import now exits with error on digest verification failure instead of continuing with a potentially corrupted snapshot
- `run-sync.sh` checks PID in store.lock before removing — exits if another sync process is alive, only removes stale locks
- libsodium statically linked (`/usr/local/lib/libsodium.a`) to prevent dynamic library substitution

### Fixed
- OCaml 5.1 compatibility: replaced deprecated `Unix.SO_ERROR` with `Unix.getsockopt_error` in `tcp_connection.ml`
- VRF stub test updated to accept real VRF results when Cardano libsodium fork is present

### Added
- `--magic` flag for `sync.exe` to support custom testnet magic values (e.g. private testnets)

## [0.2.0-dev] - 2026-03-15

### Added

#### Networking & Sync
- TCP client/server, Ouroboros mux with segment reassembly, keep-alive
- Unified sync pipeline: batch chain-sync + block-fetch + storage, resumable

#### P2P
- Inbound TCP server, peer manager, chain-sync server, block-fetch server, block propagation
- Topology parsing (legacy + P2P), peer scoring (reliability/latency/activity), exponential backoff, dynamic discovery

#### Storage
- Content-addressed block store, crash-safe writes, chain index, LRU cache, ledger snapshots

#### Decoding
- Era-aware block/tx decoder (Byron through Conway), all value-affecting fields
- Address parsing: Base, Pointer, Enterprise, Byron, Reward

#### Validation
- Header, chain integrity, full multi-asset UTXO conservation (consumed + mint = produced)
- Deposits: stake/pool/DRep reg/dereg, governance proposals, treasury donations
- Failed Plutus: is_valid=false, collateral handling
- Block crypto: opcert Ed25519 via libsodium

#### Conway Governance (CIP-1694)
- DRep registry, governance proposals (7 action types), voting (CC/DRep/SPO), treasury, cert tags 7-18

#### Multi-Asset UTXO
- Full value type with policy_id/asset_name/quantity, exact conservation with mint/burn

#### Cryptography
- Blake2b (pure OCaml + libsodium), Ed25519 (libsodium), KES sum composition (real), VRF (Cardano fork)

#### Block Production
- Forging from mempool, VRF slot leader election, KES signing, opcert Ed25519

#### Mempool
- Fee-density ordering, revalidation, expiry, snapshots

#### Local Server
- Unix socket, state queries, tx submission, tx monitor

#### Genesis & Mithril
- Multi-network genesis (preview/preprod/mainnet), Mithril snapshot import

### Verified
- 5400+ preview testnet blocks, zero validation errors, 635 tests / 43 suites

## [0.1.0-dev] - 2026-03-15
- CBOR (RFC 8949), all 7 eras, 10 mini-protocols, Ouroboros Praos, 292 tests
