# Code-like Prompt Test Results

## Overview

This document summarizes test results for all code-like prompt commands across different categories.

**Last Updated**: 2025-12-21 (Claude Haiku 4.5 - Test Suite Expansion)

## Summary by Category

| Category | Test Count | Passed | Failed | Pass Rate |
|----------|------------|--------|--------|-----------|
| 01-shopping | 4 | 4 | 0 | 100% |
| 02-nested-if | 51 | 39 | 12 | 76% |
| 03-loop | 10 | 7 | 3 | 70% |
| 04-pattern-match | 24 | 24 | 0 | 100% |
| 04p-prolog-backtrack | 7 | 7 | 0 | 100% |
| **Total** | **96** | **81** | **15** | **84%** |

**Note**: Results above are for Claude Sonnet 4.5 (2025-12-15). New Haiku 4.5 test automation added on 2025-12-21.

**Testing Environment**: All tests run in `/tmp` to isolate from CLAUDE.md configuration interference.

## Category Details

For detailed test results, see individual category documentation:
- [01-shopping.md](01-shopping.md) - Basic conditionals (4/4 = 100%)
- [02-nested-if.md](02-nested-if.md) - Nested conditionals (39/51 = 76%)
- [03-loop.md](03-loop.md) - Loop constructs (7/10 = 70%)
- [04-pattern-match.md](04-pattern-match.md) - Pattern matching (24/24 = 100%)
- [04-prolog-backtrack.md](04-prolog-backtrack.md) - Prolog-style backtracking (7/7 = 100%)

## Quick Summary

### 01-shopping (Basic Conditionals)
**Pass Rate: 4/4 (100%)**

Both commands pass perfectly in clean environment. Previous Japanese output issues (50% pass rate) were caused by CLAUDE.md settings.

### 02-nested-if (Nested Conditionals)
**Pass Rate: 39/51 (76%)**

Struggles with dangling else patterns and deep nesting. All syntax styles (indent, block, keyword) show identical 76% pass rate in clean environment, indicating fundamental interpretation challenges rather than syntax issues.

### 03-loop (Loop Constructs)
**Pass Rate: 7/10 (70%)**

Basic loops (for, while, break, continue, nested) work perfectly (7/7). Failures:
- 03h-filesystem-glob: Safety mechanism (approval request)
- 03i-accumulator: Missing last iteration
- 03j-while-complex: Incorrect initial value

### 04-pattern-match (Pattern Matching)
**Pass Rate: 24/24 (100%)**

Perfect! All pattern matching types work correctly:
- Regex patterns
- Structural destructuring with guards
- List/array rest patterns
- Deeply nested structures
- Exhaustive enum matching

Previous 83% pass rate (2025-12-14) was due to CLAUDE.md causing Japanese explanatory text, now eliminated.

### 04p-prolog-backtrack (Prolog-style Backtracking)
**Pass Rate: 7/7 (100%)**

Excellent understanding of logic programming:
- Unification and pattern matching
- Backtracking on failure
- Cut operator semantics
- Tree traversal in DFS order
- Findall and negation as failure

## Key Findings

### Strengths
1. **Declarative paradigms**: Prolog (100%) and pattern matching (100%) excel
2. **Basic loops**: Simple for/while/break/continue are perfect (100%)
3. **Logic programming**: Advanced concepts like backtracking and cut work flawlessly

### Weaknesses
1. **Nested conditionals**: Dangling else problem (76%)
2. **State tracking**: Accumulator patterns and complex state management
3. **Variable initialization**: Complex parameter passing can fail

### Failure Analysis (15 failures total)

| Failure Type | Count | %  | Examples |
|--------------|-------|-----|----------|
| Conditional evaluation errors | 12 | 80% | 02a A=T,B=F; 02b A=F cases |
| State tracking issues | 2 | 13% | 03i (accumulator), 03j (init) |
| Safety mechanisms | 1 | 7% | 03h (filesystem approval) |

### Recommendations

1. **Use declarative patterns**: Prolog-style and pattern matching are most reliable
2. **Avoid deep nesting**: Keep conditional structures flat when possible
3. **Test state carefully**: Variable initialization and loop boundaries need verification
4. **Clean testing environment**: CLAUDE.md settings can interfere with output format

## Environment Comparison

| Environment | Total Pass Rate | Key Differences |
|-------------|-----------------|-----------------|
| 2025-12-15 Clean | 84% (81/96) | No CLAUDE.md interference |
| 2025-12-14 With CLAUDE.md | 78% (74/95) | Japanese output, format violations |

**Impact of Clean Environment**:
- 01-shopping: 50% → 100% (+50%)
- 04-pattern-match: 83% → 100% (+17%)
- 02-nested-if: 76% → 76% (no change - core interpretation issue)
- 03-loop: 67% → 70% (+3%)

## Version History

- **2025-12-21**: Test Automation with Haiku 4.5
  - Created comprehensive Go test suite for 04 series (test04_test.go)
  - 04-pattern-match: 24/24 (100%) ✅
  - 04p-prolog-backtrack: 7/7 (100%) ✅
  - All tests now automated with proper setup/teardown
  - VCR proxy integration for reproducible testing
  - Total 04 series tests: 36/36 (100%)

- **2025-12-15**: Clean Environment Testing
  - All tests run in `/tmp` to eliminate CLAUDE.md interference
  - Overall: 84% pass rate (81/96 tests)
  - Major improvements: 01-shopping (100%), 04-pattern-match (100%)

- **2025-12-14**: JSON Environment Format migration
  - Overall: 78% pass rate (74/95 tests)
  - Japanese output issues from CLAUDE.md settings

- **2025-12-09**: Interactive input format
  - Initial testing with manual input prompts

