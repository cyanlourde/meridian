# Mainnet Mithril Snapshot Import

## Prerequisites

- ~120 GB free disk space (56 GB snapshot + 60 GB unpacked + overhead)
- ~4-8 hours for download (depends on bandwidth)
- ~2-4 hours for import (depends on disk speed)
- Cardano libsodium fork recommended (see BUILD_LIBSODIUM.md)

## Step 1: Download snapshot

```bash
# Get latest snapshot URL
curl -sL "https://aggregator.release-mainnet.api.mithril.network/aggregator/artifact/snapshots" | \
  python3 -c "import sys,json; s=json.load(sys.stdin)[0]; print(s['locations'][0])"

# Download with resume support
mkdir -p meridian-mainnet
curl -L -C - --progress-bar -o meridian-mainnet/snapshot.tar.zst \
  "$(curl -sL 'https://aggregator.release-mainnet.api.mithril.network/aggregator/artifact/snapshots' | \
     python3 -c "import sys,json; print(json.load(sys.stdin)[0]['locations'][0])")"
```

## Step 2: Verify digest

```bash
sha256sum meridian-mainnet/snapshot.tar.zst
# Compare with digest from aggregator
```

## Step 3: Import via Meridian

```bash
dune exec bin/import.exe -- snapshot \
  --file meridian-mainnet/snapshot.tar.zst \
  --data-dir meridian-mainnet/ \
  --network mainnet
```

## Step 4: Sync to tip

```bash
dune exec bin/sync.exe -- \
  --data-dir meridian-mainnet/ \
  --network mainnet \
  backbone.cardano.iog.io 3001
```

## What to expect

- Snapshot: ~56 GB compressed (tar.zst), ~8400+ immutable chunk files
- Import: processes ~120 million blocks across all 7 eras
- Each chunk file contains concatenated CBOR-encoded blocks
- Byron blocks (chunks 0-4489): 20-second slots, epoch boundary blocks
- Shelley blocks (chunks 4490+): 1-second slots, all subsequent eras
- Import time: ~2-4 hours on SSD, longer on HDD
- Memory: ~2-4 GB during import (blocks processed and discarded)

## Current Epoch (as of 2026-03-15)

- Epoch: 618
- Immutable file: 8424
- Chain tip: ~slot 156 million
- Era: Conway (era 6)
