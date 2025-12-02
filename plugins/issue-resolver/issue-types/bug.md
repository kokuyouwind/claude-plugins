# Bug Resolution Procedure

## Overview

This document provides the procedure for resolving bugs, errors, and defects.

## Resolution Steps

### 1. Understand the Bug
- Read the issue description carefully
- Identify the reported symptoms and error messages
- Understand the reproduction steps if provided
- Clarify expected vs actual behavior

### 2. Investigate Root Cause
- Search for relevant code using file search and grep tools
- Read the affected code sections
- Trace the execution flow to identify the source of the problem
- Use debugging process guidelines to analyze the issue systematically

### 3. Develop Fix Strategy
- Identify the specific code changes required
- Consider potential side effects and edge cases
- Plan minimal changes that address the root cause
- Avoid over-engineering or unnecessary refactoring

### 4. Implement Fix
- Make the targeted code changes
- Ensure the fix addresses the root cause, not just symptoms
- Follow existing code patterns and conventions
- Add or update tests if necessary

### 5. Create Pull Request
- Use `gh pr create` command to create the PR
- Title the PR clearly (e.g., "Fix: [brief description]")
- Include in PR description (in Japanese):
  - 概要: What bug was fixed
  - 変更の背景: Why this bug occurred
  - 変更詳細: How the fix works
- Reference the original issue number

## Important Notes

- Focus on fixing the specific bug reported
- Don't introduce unrelated changes or improvements
- Ensure the fix doesn't break existing functionality
- Test thoroughly before creating PR
