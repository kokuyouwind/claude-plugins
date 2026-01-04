#!/bin/bash
# Append content to a log file, creating directory if needed
#
# Usage: append-log.sh <directory> <filename> <content>
#
# Arguments:
#   directory - Directory path (will be created if doesn't exist)
#   filename  - File name (without directory path)
#   content   - Content to append to the file

set -e

if [ $# -ne 3 ]; then
    echo "Error: Invalid number of arguments" >&2
    echo "Usage: $0 <directory> <filename> <content>" >&2
    exit 1
fi

DIRECTORY="$1"
FILENAME="$2"
CONTENT="$3"

# Create directory if it doesn't exist
mkdir -p "$DIRECTORY"

# Append content to file
echo "$CONTENT" >> "$DIRECTORY/$FILENAME"
