# Refactoring Procedure

## Overview

This document provides the procedure for code refactoring tasks.

## Resolution Steps

### 1. Understand Refactoring Goals
- Read the issue to understand the refactoring objectives
- Identify what needs to be improved (readability, maintainability, performance, etc.)
- Understand the scope of refactoring
- Clarify any constraints or requirements

### 2. Analyze Current Code
- Read the code sections targeted for refactoring
- Understand the current implementation and its purpose
- Identify code smells or problematic patterns
- Note any dependencies or side effects

### 3. Plan Refactoring Approach
- Choose appropriate refactoring techniques
- Plan incremental changes to minimize risk
- Ensure existing functionality will be preserved
- Consider impact on other parts of the codebase

### 4. Implement Refactoring
- Make targeted improvements to code structure
- Preserve existing functionality exactly
- Follow established code patterns and conventions
- Keep changes focused on the refactoring goals
- Don't add new features or change behavior

### 5. Verify Functionality
- Ensure refactored code behaves identically to original
- Run tests to verify no regressions
- Check that all use cases still work
- Review code quality improvements

### 6. Create Pull Request
- Use `gh pr create` command to create the PR
- Title the PR clearly (e.g., "Refactor: [brief description]")
- Include in PR description (in Japanese):
  - 概要: What was refactored
  - 変更の背景: Why refactoring was needed
  - 変更詳細: What improvements were made
- Reference the original issue number

## Important Notes

- Maintain existing behavior exactly - no functional changes
- Keep refactoring scope focused and bounded
- Don't mix refactoring with feature additions or bug fixes
- Ensure tests pass after refactoring
- Make the code cleaner, not just different
