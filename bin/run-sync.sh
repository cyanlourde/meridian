#!/bin/bash
# Auto-restart wrapper for Meridian sync
set -e

DATA_DIR="${1:-meridian-preview-full}"
NETWORK="${2:-preview}"
LOG="$DATA_DIR/sync.log"

echo "$(date): Starting Meridian sync wrapper (data=$DATA_DIR, network=$NETWORK)" >> "$LOG"

while true; do
    rm -f "$DATA_DIR/store.lock"
    echo "$(date): Launching sync..." >> "$LOG"
    dune exec bin/sync.exe -- \
        --data-dir "$DATA_DIR" \
        --network "$NETWORK" \
        --batch-size 500 \
        --follow 30 \
        >> "$LOG" 2>&1 || true

    EXIT_CODE=$?
    echo "$(date): Sync exited with code $EXIT_CODE" >> "$LOG"

    # Check if we should stop
    if [ -f "$DATA_DIR/STOP" ]; then
        echo "$(date): STOP file found, exiting." >> "$LOG"
        break
    fi

    echo "$(date): Restarting in 3 seconds..." >> "$LOG"
    sleep 3
done
