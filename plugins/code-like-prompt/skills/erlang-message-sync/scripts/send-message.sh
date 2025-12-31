#!/bin/bash
# send-message.sh - Send a message to another Erlang-style process via /tmp

set -euo pipefail

# Usage: send-message.sh <from-pid> <to-pid> <message-json>
if [ $# -ne 3 ]; then
    echo "Usage: $0 <from-pid> <to-pid> <message-json>" >&2
    echo "Example: $0 main worker_1 '{\"type\":\"request\",\"data\":\"hello\"}'" >&2
    exit 1
fi

FROM_PID="$1"
TO_PID="$2"
MESSAGE_JSON="$3"

# Create mailbox directory for recipient
MAILBOX_DIR="/tmp/erlang-messages/${TO_PID}"
mkdir -p "${MAILBOX_DIR}"

# Generate unique message filename with timestamp
TIMESTAMP=$(date +%s%N)
MESSAGE_FILE="${MAILBOX_DIR}/${FROM_PID}-${TIMESTAMP}.json"

# Write message to file
echo "${MESSAGE_JSON}" > "${MESSAGE_FILE}"

# Output confirmation
echo "Message sent from ${FROM_PID} to ${TO_PID}" >&2
echo "Message file: ${MESSAGE_FILE}" >&2
echo "Message content: ${MESSAGE_JSON}" >&2
