# type:bug - Bug Fix

## Process

### 1. Understand the Bug

- Review symptoms described in the Issue
- Understand reproduction steps (ask Issue author if unclear)

### 2. Investigate Root Cause

Use **`dev-guidelines:debugging-process`** skill for systematic investigation:
- Trace from error messages/stack traces
- Analyze conditions that trigger the bug
- Add debug logs if needed

### 3. Implement Fix

- Fix the root cause, not symptoms
- Verify existing tests still pass
- Add regression tests as needed

### 4. Create PR

Use **`dev-guidelines:pr-description-format`** skill. Include:
- Bug symptoms
- Root cause explanation
- Fix description
- Test method (if applicable)

Commit message format: `fix: <brief description>`

## Guidelines

- Keep changes minimal
- Separate unrelated refactoring into separate Issues
- Handle security-related bugs with extra care
