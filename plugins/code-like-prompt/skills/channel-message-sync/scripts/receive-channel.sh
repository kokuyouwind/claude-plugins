#!/bin/bash
# receive-channel.sh - Receive a message from a Go-style channel (blocking with timeout)

set -euo pipefail

# Usage: receive-channel.sh <channel-name> [timeout-seconds]
if [ $# -lt 1 ]; then
    echo "Usage: $0 <channel-name> [timeout-seconds]" >&2
    echo "Example: $0 ch1           # Receive from 'ch1'" >&2
    echo "Example: $0 ch1 5         # Receive with 5s timeout" >&2
    exit 1
fi

CHANNEL="$1"
TIMEOUT="${2:-30}"

CHANNEL_DIR="/tmp/go-channels/${CHANNEL}"
mkdir -p "${CHANNEL_DIR}"

START_TIME=$(date +%s)
SLEEP_INTERVAL=0.1

while true; do
    MESSAGE_FILES=$(find "${CHANNEL_DIR}" -name "*.txt" 2>/dev/null | sort | head -n 1 || true)

    if [ -n "${MESSAGE_FILES}" ]; then
        MESSAGE_FILE="${MESSAGE_FILES}"
        MESSAGE_CONTENT=$(cat "${MESSAGE_FILE}")
        rm -f "${MESSAGE_FILE}"

        echo "Received from channel ${CHANNEL}: ${MESSAGE_CONTENT}" >&2
        echo "${MESSAGE_CONTENT}"
        exit 0
    fi

    CURRENT_TIME=$(date +%s)
    ELAPSED=$((CURRENT_TIME - START_TIME))

    if [ ${ELAPSED} -ge ${TIMEOUT} ]; then
        echo "Timeout: No message on channel ${CHANNEL} after ${TIMEOUT}s" >&2
        exit 1
    fi

    sleep ${SLEEP_INTERVAL}
done
