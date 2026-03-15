#!/bin/bash
# Meridian integration test — sync real blocks with full validation
set -e

DIR="meridian-integration-test"
HOST="preview-node.play.dev.cardano.org"
PORT=3001
BLOCKS=1000

echo "=== Meridian Integration Test ==="
echo "Target: ${HOST}:${PORT}, ${BLOCKS} blocks"

# Clean previous test data
rm -rf "$DIR"
echo "Cleaned previous data"

# Phase 1: Initial sync
echo ""
echo "--- Phase 1: Initial sync (${BLOCKS} blocks) ---"
timeout 120 dune exec bin/sync.exe -- \
  --data-dir "$DIR" \
  --full-validation \
  --max-blocks "$BLOCKS" \
  --batch-size 50 \
  "$HOST" "$PORT" 2>&1 || true

DISK_BLOCKS=$(ls "$DIR"/blocks/*/*.block 2>/dev/null | wc -l)
echo ""
echo "Blocks on disk: ${DISK_BLOCKS}"

# Check validation log
if [ -f "$DIR/validation.log" ]; then
  TOTAL_ERRORS=$(wc -l < "$DIR/validation.log")
  echo "Validation log: ${TOTAL_ERRORS} entries"
  echo "Error categories:"
  grep -oP '(header|crypto|ledger|decode):' "$DIR/validation.log" | sort | uniq -c | sort -rn || echo "  (none)"
  echo ""
  echo "First 10 log entries:"
  head -10 "$DIR/validation.log"
else
  echo "No validation log (no errors)"
fi

# Check ledger snapshot
if [ -f "$DIR/ledger.snapshot" ]; then
  echo ""
  SNAP_SIZE=$(stat -c%s "$DIR/ledger.snapshot")
  echo "Ledger snapshot: ${SNAP_SIZE} bytes"
fi

# Phase 2: Resume test
echo ""
echo "--- Phase 2: Resume sync (500 more blocks) ---"
timeout 60 dune exec bin/sync.exe -- \
  --data-dir "$DIR" \
  --full-validation \
  --max-blocks 500 \
  --batch-size 50 \
  "$HOST" "$PORT" 2>&1 || true

DISK_BLOCKS_2=$(ls "$DIR"/blocks/*/*.block 2>/dev/null | wc -l)
echo ""
echo "Blocks after resume: ${DISK_BLOCKS_2}"

if [ "$DISK_BLOCKS_2" -gt "$DISK_BLOCKS" ]; then
  echo "PASS: Resume added blocks (${DISK_BLOCKS} -> ${DISK_BLOCKS_2})"
else
  echo "WARN: Resume did not add blocks"
fi

# Final summary
echo ""
echo "=== Integration Test Summary ==="
echo "Phase 1 blocks: ${DISK_BLOCKS}"
echo "Phase 2 blocks: ${DISK_BLOCKS_2}"
if [ -f "$DIR/validation.log" ]; then
  echo "Total log entries: $(wc -l < "$DIR/validation.log")"
fi
echo "Test completed."

# Cleanup
rm -rf "$DIR"
