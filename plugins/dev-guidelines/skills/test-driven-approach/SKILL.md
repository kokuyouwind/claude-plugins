---
name: test-driven-approach
description: Provides TDD methodology guidance based on established practices (t_wada, Kent Beck). Use this skill when implementing tests, practicing TDD, creating test cases, or planning test strategy.
---

# Test-Driven Approach

## Instructions

### Core Principles

1. **Reference Specific Methodologies**: Cite t_wada's TDD, Kent Beck's TDD, or framework-specific practices (Jest, pytest, RSpec). Avoid generic "best practices".
2. **Test Skeleton First**: Provide structure before implementation to identify edge cases early.
3. **Red-Green-Refactor**: Write failing test → Minimal code to pass → Refactor

### What to Test

- **Happy path**: Expected usage scenarios
- **Edge cases**: Boundary conditions, empty inputs
- **Error cases**: Invalid inputs, error handling

### Test Naming

✅ `should return 404 when user not found`
❌ `works correctly` / `test1`

### DO / DON'T

**DO**: Reference specific methodologies, provide skeleton first, use descriptive names, test behavior not implementation

**DON'T**: Use vague best practices, skip structure, test implementation details, create dependent tests

## Examples

### Test Skeleton Example

```typescript
describe('UserAuthentication', () => {
  describe('login', () => {
    it('should successfully login with valid credentials', () => {});
    it('should reject invalid credentials', () => {});
    it('should lock account after multiple failed attempts', () => {});
  });
});
```
