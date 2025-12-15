#!/bin/bash

# Run all code-like-prompt tests in clean environment
# This script executes all test categories in sequence

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
TEST_DIR="/tmp/code-like-prompt-test-${TIMESTAMP}"

echo "=== Code-Like-Prompt Test Suite ==="
echo "Date: $(date '+%Y-%m-%d %H:%M:%S')"
echo "Model: Claude Sonnet 4.5"
echo "Environment: Clean (/tmp, isolated from CLAUDE.md)"
echo "Test Directory: ${TEST_DIR}"
echo ""

# Create clean test directory
mkdir -p "${TEST_DIR}"
cd "${TEST_DIR}"

echo "Running tests in clean environment..."
echo ""

# Run each test category
echo "======================================"
echo "Running 02a tests..."
echo "======================================"
bash "${SCRIPT_DIR}/run_02a_tests.sh"

echo ""
echo "======================================"
echo "Running 02b tests..."
echo "======================================"
bash "${SCRIPT_DIR}/run_02b_tests.sh"

echo ""
echo "======================================"
echo "Running 02c tests..."
echo "======================================"
bash "${SCRIPT_DIR}/run_02c_tests.sh"

echo ""
echo "======================================"
echo "Running 03 tests..."
echo "======================================"
bash "${SCRIPT_DIR}/run_03_tests.sh"

echo ""
echo "======================================"
echo "Running 04 tests..."
echo "======================================"
bash "${SCRIPT_DIR}/run_04_tests.sh"

echo ""
echo "======================================"
echo "Running 04p tests..."
echo "======================================"
bash "${SCRIPT_DIR}/run_04p_tests.sh"

echo ""
echo "======================================"
echo "All tests complete!"
echo "======================================"
echo "Test directory: ${TEST_DIR}"
