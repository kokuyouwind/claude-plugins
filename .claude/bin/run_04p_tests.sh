#!/bin/bash

# Test runner for 04p-prolog-backtrack commands

TEST_DIR="/tmp/code-like-prompt-test-20251215_111830"
cd "$TEST_DIR"

echo "=== 04p-prolog-backtrack Test Results ==="
echo "Date: $(date '+%Y-%m-%d')"
echo "Model: Claude Sonnet 4.5"
echo "Environment: Clean (/tmp, isolated from CLAUDE.md)"
echo ""

echo "=== 04p-a-basic-facts ==="
echo "Expected: corge, grault"
claude -p '/code-like-prompt:04p-a-basic-facts' 2>&1 | grep -v "^/"

echo ""
echo "=== 04p-b-multi-clause ==="
echo "Expected: b c c d d"
claude -p '/code-like-prompt:04p-b-multi-clause' 2>&1 | grep -v "^/"

echo ""
echo "=== 04p-c-cut ==="
echo "Expected: bar, corge"
claude -p '/code-like-prompt:04p-c-cut' 2>&1 | grep -v "^/"

echo ""
echo "=== 04p-d-tree-traverse ==="
echo "Expected: baz qux corge grault"
claude -p '/code-like-prompt:04p-d-tree-traverse' 2>&1 | grep -v "^/"

echo ""
echo "=== 04p-e-findall ==="
echo "Expected: [baz, quux]"
claude -p '/code-like-prompt:04p-e-findall' 2>&1 | grep -v "^/"

echo ""
echo "=== 04p-f-negation ==="
echo "Expected: bar"
claude -p '/code-like-prompt:04p-f-negation' 2>&1 | grep -v "^/"

echo ""
echo "=== 04p-g-constraints ==="
echo "Expected: foo-bar-baz, foo-baz-bar, bar-foo-baz, bar-baz-foo, baz-foo-bar, baz-bar-foo"
claude -p '/code-like-prompt:04p-g-constraints' 2>&1 | grep -v "^/"

echo ""
echo "=== Test Complete ==="
