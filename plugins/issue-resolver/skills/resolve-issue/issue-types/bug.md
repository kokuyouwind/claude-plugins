# Bug Resolution Procedure

Follow these steps to resolve bug-type issues:

## 1. Understand the Bug

- Read the issue description carefully to understand the bug behavior
- Identify the expected behavior vs. actual behavior
- Note any reproduction steps provided

## 2. Investigate Root Cause

- Locate the relevant code sections
- Trace the code execution path
- Identify the root cause of the bug
- Document your findings

## 3. Implement Fix

- Design a minimal fix that addresses the root cause
- Avoid over-engineering or adding unnecessary features
- Maintain consistency with existing code patterns
- Ensure the fix doesn't introduce new issues

## 4. Create Pull Request

- Use `gh pr create` to create the PR
- Write PR description in Japanese
- Include:
  - Bug description and root cause analysis
  - Explanation of the fix
  - Testing performed (if applicable)
- Reference the issue in the PR description with "Closes #<issue-number>"
