# type:bug - Bug Fix Procedures

## Overview

Detailed procedures for fixing bugs, errors, and defects.

## Implementation Steps

### 1. Understand the bug and reproduction steps

- Review the bug symptoms described in the Issue
- Understand the reproduction steps if provided
- If reproduction steps are unclear, infer from code or ask the Issue creator

### 2. Investigate related code to identify the root cause

- Identify related code from error messages or stack traces
- Analyze conditions and situations where the bug occurs
- Add debug logs as needed to narrow down the cause

### 3. Plan and implement the fix

- Consider appropriate fix methods targeting the root cause
- Verify no side effects
- Confirm existing tests still pass
- Add new tests as necessary

### 4. Create fix PR

- Use commit message format: `fix: <concise bug description>`
- Include in PR description:
  - Bug symptoms
  - Cause explanation
  - Fix details
  - Test method (if applicable)

## Notes

- Keep bug fixes to minimal changes
- Separate unnecessary refactoring into a different Issue
- Handle security-related bugs with special care
