# Meridian Changelog

## [0.1.0-dev] - 2026-03-15

### Added

- CBOR encoder/decoder implementing RFC 8949 (all major types, canonical mode, indefinite length)

- Cardano block and transaction types for all eras (Byron, Shelley, Allegra, Mary, Alonzo, Babbage, Conway)

- Ouroboros network multiplexer (segment framing, protocol ID handling, direction bit)

- Node-to-node mini-protocols: handshake, chain-sync, block-fetch, tx-submission, keep-alive

- Node-to-client mini-protocols: local chain-sync, local state-query, local tx-submission, local tx-monitor

- Ouroboros Praos consensus: chain selection (k-deep fork rule), slot leader check (VRF threshold), epoch/slot arithmetic

- Cryptographic primitives: Blake2b-256/224 (pure OCaml, RFC 7693), Ed25519/VRF/KES verification stubs

- 292 tests across 14 test suites, all passing
