#!/bin/bash

# Test runner for 04-pattern-match commands

TEST_DIR="/tmp/code-like-prompt-test-20251215_111830"
cd "$TEST_DIR"

echo "=== 04-pattern-match Test Results ==="
echo "Date: $(date '+%Y-%m-%d')"
echo "Model: Claude Sonnet 4.5"
echo "Environment: Clean (/tmp, isolated from CLAUDE.md)"
echo ""

run_test() {
    local cmd=$1
    local args=$2
    local expected=$3

    echo -n "$args -> $expected: "
    result=$(claude -p "/code-like-prompt:$cmd $args" 2>&1 | grep -v "^/" | head -1)
    echo "$result"
}

echo "=== 04a-regex-match ==="
run_test "04a-regex-match" '{"text": "fooXXXbar"}' "qux"
run_test "04a-regex-match" '{"text": "foobar"}' "qux"
run_test "04a-regex-match" '{"text": "bazanything"}' "quux"
run_test "04a-regex-match" '{"text": "hello"}' "corge"

echo ""
echo "=== 04b-structural-match ==="
run_test "04b-structural-match" '{"type": "foo", "value": "123"}' "bar123"
run_test "04b-structural-match" '{"type": "baz", "value": "15"}' "qux15"
run_test "04b-structural-match" '{"type": "baz", "value": "5"}' "quux5"
run_test "04b-structural-match" '{"type": "other", "value": "x"}' "corge"

echo ""
echo "=== 04c-list-destructure ==="
run_test "04c-list-destructure" '{"item1": "foo", "item2": "X", "item3": "bar"}' "quxX"
run_test "04c-list-destructure" '{"item1": "foo", "item2": "a", "item3": "b"}' "quux2"
run_test "04c-list-destructure" '{"item1": "other", "item2": "x", "item3": "y"}' "corgeother"

echo ""
echo "=== 04d-nested-match ==="
run_test "04d-nested-match" '{"left": "foo", "right_left": "bar", "right_right": "X"}' "qux"
run_test "04d-nested-match" '{"left": "A", "right_left": "same", "right_right": "same"}' "quuxA"
run_test "04d-nested-match" '{"left": "A", "right_left": "B", "right_right": "C"}' "corgeA"

echo ""
echo "=== 04e-multi-guard ==="
run_test "04e-multi-guard" '{"x": 1, "y": 1}' "foo"
run_test "04e-multi-guard" '{"x": 1, "y": -1}' "bar"
run_test "04e-multi-guard" '{"x": -1, "y": 1}' "bar"
run_test "04e-multi-guard" '{"x": 0, "y": 0}' "baz"
run_test "04e-multi-guard" '{"x": -1, "y": -1}' "qux"

echo ""
echo "=== 04f-exhaustive ==="
run_test "04f-exhaustive" '{"color": "Red"}' "foo"
run_test "04f-exhaustive" '{"color": "Green"}' "bar"
run_test "04f-exhaustive" '{"color": "Blue"}' "baz"
run_test "04f-exhaustive" '{"color": "Custom", "r": 255, "g": 100, "b": 50}' "qux"
run_test "04f-exhaustive" '{"color": "Custom", "r": 100, "g": 150, "b": 200}' "quux100150200"

echo ""
echo "=== Test Complete ==="
