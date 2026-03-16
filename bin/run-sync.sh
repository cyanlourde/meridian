#!/bin/bash
# Auto-restart wrapper for Meridian sync
set -e

DATA_DIR="${1:-meridian-preview-full}"
NETWORK="${2:-preview}"
LOG="$DATA_DIR/sync.log"

echo "$(date): Starting Meridian sync wrapper (data=$DATA_DIR, network=$NETWORK)" >> "$LOG"

while true; do
    LOCKFILE="$DATA_DIR/store.lock"
    if [ -f "$LOCKFILE" ]; then
        LOCK_PID=$(cat "$LOCKFILE" 2>/dev/null | tr -d '[:space:]')
        if [ -n "$LOCK_PID" ] && kill -0 "$LOCK_PID" 2>/dev/null; then
            echo "$(date): Another sync process is running (PID $LOCK_PID). Exiting." >> "$LOG"
            exit 1
        fi
        echo "$(date): Removing stale lock (PID $LOCK_PID no longer running)" >> "$LOG"
        rm -f "$LOCKFILE"
    fi
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
