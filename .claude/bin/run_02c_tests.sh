#!/bin/bash

# Test runner for 02c-deep-nesting commands

TEST_DIR="/tmp/code-like-prompt-test-20251215_111830"
cd "$TEST_DIR"

echo "=== 02c-deep-nesting Test Results ==="
echo "Date: $(date '+%Y-%m-%d')"
echo "Model: Claude Sonnet 4.5"
echo "Environment: Clean (/tmp, isolated from CLAUDE.md)"
echo ""

# Expected results:
# L1=T, L2=T, L3=T, L4=*: foo
# L1=T, L2=T, L3=F, L4=T: bar
# L1=T, L2=T, L3=F, L4=F: baz
# L1=T, L2=F, L3=T, L4=T: qux
# L1=T, L2=F, L3=T, L4=F: (none) - dangling else
# L1=T, L2=F, L3=F, L4=*: quux
# L1=F, L2=T, L3=*, L4=*: corge
# L1=F, L2=F, L3=T, L4=*: grault
# L1=F, L2=F, L3=F, L4=*: garply

run_test() {
    local style=$1
    local l1=$2
    local l2=$3
    local l3=$4
    local l4=$5
    local expected=$6

    echo -n "L1=$l1, L2=$l2, L3=$l3, L4=$l4 (expected: $expected): "
    result=$(claude -p "/code-like-prompt:02c-deep-nesting-$style {\"level1\": $l1, \"level2\": $l2, \"level3\": $l3, \"level4\": $l4}" 2>&1 | grep -v "^/" | head -1)
    if [ -z "$result" ]; then
        echo "(empty)"
    else
        echo "$result"
    fi
}

for style in indent block keyword; do
    echo ""
    echo "=== 02c-deep-nesting-$style ==="

    run_test $style true true true false "foo"
    run_test $style true true false true "bar"
    run_test $style true true false false "baz"
    run_test $style true false true true "qux"
    run_test $style true false true false "none"
    run_test $style true false false false "quux"
    run_test $style false true false false "corge"
    run_test $style false false true false "grault"
    run_test $style false false false false "garply"
done

echo ""
echo "=== Test Complete ==="
