#!/bin/bash
# receive-message.sh - Receive a message from the process mailbox (blocking with timeout)

set -euo pipefail

# Usage: receive-message.sh <pid> [from-pid-pattern] [timeout-seconds]
if [ $# -lt 1 ]; then
    echo "Usage: $0 <pid> [from-pid-pattern] [timeout-seconds]" >&2
    echo "Example: $0 main                    # Receive any message for 'main'" >&2
    echo "Example: $0 main worker_1           # Receive only from 'worker_1'" >&2
    echo "Example: $0 main worker_1 5         # Receive from 'worker_1' with 5s timeout" >&2
    exit 1
fi

PID="$1"
FROM_PATTERN="${2:-*}"  # Default: accept from any sender
TIMEOUT="${3:-30}"      # Default: 30 seconds timeout

MAILBOX_DIR="/tmp/erlang-messages/${PID}"

# Create mailbox directory if it doesn't exist
mkdir -p "${MAILBOX_DIR}"

# Track elapsed time
START_TIME=$(date +%s)
SLEEP_INTERVAL=0.1  # Check every 100ms

echo "Waiting for message to ${PID} from ${FROM_PATTERN} (timeout: ${TIMEOUT}s)..." >&2

while true; do
    # Check for messages matching the pattern
    # Pattern format: <from-pid>-<timestamp>.json
    MESSAGE_FILES=$(find "${MAILBOX_DIR}" -name "${FROM_PATTERN}-*.json" 2>/dev/null | sort | head -n 1 || true)

    if [ -n "${MESSAGE_FILES}" ]; then
        # Found a message!
        MESSAGE_FILE="${MESSAGE_FILES}"

        # Extract sender from filename
        FILENAME=$(basename "${MESSAGE_FILE}")
        FROM_PID="${FILENAME%%-*}"

        # Read message content
        MESSAGE_CONTENT=$(cat "${MESSAGE_FILE}")

        # Delete the message file (consume it)
        rm -f "${MESSAGE_FILE}"

        # Output result
        echo "Received message at ${PID} from ${FROM_PID}" >&2
        echo "Message content: ${MESSAGE_CONTENT}" >&2

        # Return the message content to stdout (for capturing)
        echo "${MESSAGE_CONTENT}"
        exit 0
    fi

    # Check timeout
    CURRENT_TIME=$(date +%s)
    ELAPSED=$((CURRENT_TIME - START_TIME))

    if [ ${ELAPSED} -ge ${TIMEOUT} ]; then
        echo "Timeout: No message received at ${PID} from ${FROM_PATTERN} after ${TIMEOUT}s" >&2
        exit 1
    fi

    # Sleep before next check
    sleep ${SLEEP_INTERVAL}
done
