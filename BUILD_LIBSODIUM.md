# Building the Cardano libsodium fork for VRF support

Meridian uses the Cardano libsodium fork for VRF (Verifiable Random Function)
operations. Without the fork, VRF functions gracefully return errors and
Meridian operates without VRF verification.

## Build instructions

```bash
git clone https://github.com/IntersectMBO/libsodium.git
cd libsodium
git checkout iquerejeta/vrf_batchverify
./configure --prefix=/usr/local   # or a local prefix
make -j$(nproc)
sudo make install
sudo ldconfig
```

## Verify

```bash
nm -D /usr/local/lib/libsodium.so | grep crypto_vrf_ietfdraft13
```

Should show symbols including `crypto_vrf_ietfdraft13_prove` and `_verify`.

## Usage with Meridian

Set `LD_LIBRARY_PATH` to include the fork's install location:

```bash
LD_LIBRARY_PATH=/usr/local/lib dune exec bin/sync.exe -- --full-validation
```

Meridian loads libsodium via `dlopen` at runtime. When the Cardano fork is
found, `Crypto.vrf_available` becomes `true` and all VRF operations use
real ECVRF-ED25519-SHA512-Elligator2.

## VRF functions provided

- `crypto_vrf_ietfdraft13_prove` — generate 80-byte VRF proof
- `crypto_vrf_ietfdraft13_verify` — verify proof, output 64-byte hash
- `crypto_vrf_ietfdraft13_proof_to_hash` — extract hash from proof

## Without the fork

Standard libsodium (e.g. from `apt-get install libsodium23`) provides
Ed25519 and Blake2b but not VRF. Meridian detects this at runtime and:
- `Crypto.vrf_available` = false
- VRF functions return `Error "VRF not available"`
- Slot leader checks use Blake2b-based deterministic test mode
- Block header VRF proof verification is skipped
