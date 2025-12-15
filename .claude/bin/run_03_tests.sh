#!/bin/bash

# Test runner for 03-loop commands

TEST_DIR="/tmp/code-like-prompt-test-20251215_111830"
cd "$TEST_DIR"

echo "=== 03-loop Test Results ==="
echo "Date: $(date '+%Y-%m-%d')"
echo "Model: Claude Sonnet 4.5"
echo "Environment: Clean (/tmp, isolated from CLAUDE.md)"
echo ""

echo "=== 03a-for-count ==="
echo -n "count=5 (expected: foo0, foo1, foo2, foo3, foo4): "
result=$(claude -p '/code-like-prompt:03a-for-count {"count": 5}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"

echo ""
echo "=== 03b-while-counter ==="
echo -n "max_count=3 (expected: bar, bar, bar, baz): "
result=$(claude -p '/code-like-prompt:03b-while-counter {"max_count": 3}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"

echo ""
echo "=== 03c-each-collection ==="
echo -n "items=[qux,quux,corge] (expected: qux, quux, corge): "
result=$(claude -p '/code-like-prompt:03c-each-collection {"items": ["qux", "quux", "corge"]}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"

echo ""
echo "=== 03d-loop-break ==="
echo -n "break_at=3 (expected: foo0, foo1, foo2, bar): "
result=$(claude -p '/code-like-prompt:03d-loop-break {"break_at": 3}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"

echo ""
echo "=== 03e-loop-continue ==="
echo -n "skip_at=2 (expected: foo0, foo1, foo3, foo4): "
result=$(claude -p '/code-like-prompt:03e-loop-continue {"skip_at": 2}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"

echo ""
echo "=== 03f-nested-loops ==="
echo -n "outer_count=3, inner_count=2 (expected: foo00, foo01, foo10, foo11, foo20, foo21): "
result=$(claude -p '/code-like-prompt:03f-nested-loops {"outer_count": 3, "inner_count": 2}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"

echo ""
echo "=== 03g-nested-break ==="
echo -n "outer_count=3, inner_break_at=2 (expected: bar00, bar01, baz0, bar10, bar11, baz1, bar20, bar21, baz2): "
result=$(claude -p '/code-like-prompt:03g-nested-break {"outer_count": 3, "inner_break_at": 2}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"

echo ""
echo "=== 03h-filesystem-glob ==="
echo -n "pattern=*.txt (testing with files in current dir): "
# Create some test files
touch test1.txt test2.txt test3.md
result=$(claude -p '/code-like-prompt:03h-filesystem-glob {"pattern": "*.txt"}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"
# Clean up test files
rm -f test1.txt test2.txt test3.md

echo ""
echo "=== 03i-accumulator ==="
echo -n "start=1, end=4 (expected: sum=10): "
result=$(claude -p '/code-like-prompt:03i-accumulator {"start": 1, "end": 4}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"

echo ""
echo "=== 03j-while-complex ==="
echo -n "x_limit=5, y_start=10, y_decrement=3 (expected: bar010, bar17, bar24, bar31): "
result=$(claude -p '/code-like-prompt:03j-while-complex {"x_limit": 5, "y_start": 10, "y_decrement": 3}' 2>&1 | grep -v "^/" | tr '\n' ', ')
echo "$result"

echo ""
echo "=== Test Complete ==="
