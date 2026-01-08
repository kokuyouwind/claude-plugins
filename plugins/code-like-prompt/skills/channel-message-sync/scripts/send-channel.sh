#!/bin/bash
# send-channel.sh - Send a message to a Go-style channel via /tmp

set -euo pipefail

# Usage: send-channel.sh <from-goroutine> <to-channel> <message>
if [ $# -ne 3 ]; then
    echo "Usage: $0 <from-goroutine> <to-channel> <message>" >&2
    echo "Example: $0 main ch1 'hello'" >&2
    exit 1
fi

FROM_GOROUTINE="$1"
TO_CHANNEL="$2"
MESSAGE="$3"

CHANNEL_DIR="/tmp/go-channels/${TO_CHANNEL}"
mkdir -p "${CHANNEL_DIR}"

TIMESTAMP=$(date +%s%N)
MESSAGE_FILE="${CHANNEL_DIR}/${FROM_GOROUTINE}-${TIMESTAMP}.txt"

echo "${MESSAGE}" > "${MESSAGE_FILE}"

echo "Sent to channel ${TO_CHANNEL}: ${MESSAGE}" >&2
