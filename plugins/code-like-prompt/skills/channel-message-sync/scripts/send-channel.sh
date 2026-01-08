#!/bin/bash
# send-channel.sh - Send a message to a Go-style channel via /tmp

set -euo pipefail

# Usage: send-channel.sh <to-channel> <message>
if [ $# -ne 2 ]; then
    echo "Usage: $0 <to-channel> <message>" >&2
    echo "Example: $0 ch1 'hello'" >&2
    exit 1
fi

TO_CHANNEL="$1"
MESSAGE="$2"

CHANNEL_DIR="/tmp/go-channels/${TO_CHANNEL}"
mkdir -p "${CHANNEL_DIR}"

TIMESTAMP=$(date +%s%N)
MESSAGE_FILE="${CHANNEL_DIR}/${TIMESTAMP}.txt"

echo "${MESSAGE}" > "${MESSAGE_FILE}"

echo "Sent to channel ${TO_CHANNEL}: ${MESSAGE}" >&2
