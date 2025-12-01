---
name: Test-Driven Approach
description: Use this skill when implementing tests, practicing TDD, creating test cases, or planning test strategy. Reference specific methodologies (t_wada's TDD, Kent Beck's TDD) and provide test skeleton before implementation.
version: 1.0.0
---

# Test-Driven Development Methodology

## Core Principles

### 1. Reference Specific Methodologies

When implementing tests, cite **concrete methodologies**:
- **t_wada's TDD** - Test-Driven Development approach by t_wada
- **Kent Beck's Test-Driven Development** - Classic TDD methodology
- Framework-specific best practices (Jest, pytest, RSpec, etc.)

Generic "best practices" are less effective than specific, established approaches.

### 2. Test Skeleton First

Provide the **structure** of test cases before implementation:

```typescript
describe('UserAuthentication', () => {
  describe('login', () => {
    it('should successfully login with valid credentials', () => {
      // TODO: Implement
    });

    it('should reject invalid credentials', () => {
      // TODO: Implement
    });

    it('should handle missing credentials', () => {
      // TODO: Implement
    });

    it('should lock account after multiple failed attempts', () => {
      // TODO: Implement
    });
  });

  describe('logout', () => {
    it('should clear session on logout', () => {
      // TODO: Implement
    });
  });
});
```

**Benefits**: Structure guides implementation, identifies edge cases early, ensures comprehensive coverage, facilitates scope discussion.

### 3. Red-Green-Refactor Cycle

1. **Red**: Write a failing test (feature doesn't exist yet)
2. **Green**: Write minimal code to make test pass
3. **Refactor**: Improve code while keeping tests green

## Test Case Design

### What to Test
- **Happy path**: Expected usage scenarios
- **Edge cases**: Boundary conditions, empty inputs
- **Error cases**: Invalid inputs, error handling
- **Integration points**: Interaction with dependencies

### Test Naming
Use descriptive names that explain the expected behavior:

✅ `should return 404 when user not found`
✅ `should validate email format before saving`
❌ `works correctly`
❌ `test1`

## Example Workflow

```markdown
1. "マスター、t_wadaのTDDアプローチを使ってテストを実装するね"
2. "まず、テストケースの構造を作るよ"
   [Show test skeleton with describe/it blocks]
3. "これでカバーすべきケースは揃っているかな？"
4. "それじゃあ、各テストケースを実装していくね"
   [Implement tests following Red-Green-Refactor]
```

## Best Practices

### DO:
- Reference specific methodologies (not generic "best practices")
- Provide test skeleton before details
- Write tests before implementation (in TDD)
- Use descriptive test names
- Test behavior, not implementation details

### DON'T:
- Use vague "best practices" without specifics
- Jump to implementation without structure
- Test internal implementation details
- Create dependent tests

## Integration with Other Skills

- Works with **implementation-workflow** for proper branch setup before writing tests
- Complements **debugging-process** (write failing test → investigate → fix → verify)
