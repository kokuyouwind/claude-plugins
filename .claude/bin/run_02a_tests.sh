#!/bin/bash

# Test runner for 02a-dangling-else-outer commands

TEST_DIR="/tmp/code-like-prompt-test-20251215_111830"
cd "$TEST_DIR"

echo "=== 02a-dangling-else-outer Test Results ==="
echo "Date: $(date '+%Y-%m-%d')"
echo "Model: Claude Sonnet 4.5"
echo "Environment: Clean (/tmp, isolated from CLAUDE.md)"
echo ""

# Expected results:
# A=T, B=T: foo
# A=T, B=F: (none)
# A=F, B=T: bar
# A=F, B=F: bar

for style in indent block keyword; do
    echo ""
    echo "=== 02a-dangling-else-outer-$style ==="

    echo -n "A=T, B=T (expected: foo): "
    result=$(claude -p "/code-like-prompt:02a-dangling-else-outer-$style {\"condition_a\": true, \"condition_b\": true}" 2>&1 | grep -v "^/")
    echo "$result"

    echo -n "A=T, B=F (expected: none): "
    result=$(claude -p "/code-like-prompt:02a-dangling-else-outer-$style {\"condition_a\": true, \"condition_b\": false}" 2>&1 | grep -v "^/")
    if [ -z "$result" ]; then
        echo "(empty)"
    else
        echo "$result"
    fi

    echo -n "A=F, B=T (expected: bar): "
    result=$(claude -p "/code-like-prompt:02a-dangling-else-outer-$style {\"condition_a\": false, \"condition_b\": true}" 2>&1 | grep -v "^/")
    echo "$result"

    echo -n "A=F, B=F (expected: bar): "
    result=$(claude -p "/code-like-prompt:02a-dangling-else-outer-$style {\"condition_a\": false, \"condition_b\": false}" 2>&1 | grep -v "^/")
    echo "$result"
done

echo ""
echo "=== Test Complete ==="
