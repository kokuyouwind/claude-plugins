---
name: Test-Driven Approach
description: Use this skill when implementing tests, practicing TDD (Test-Driven Development), creating test cases, writing unit tests, or planning test strategy. Provides methodology for effective testing.
version: 1.0.0
---

# Testing Methodology and Approach

## When to Use This Skill

This skill should be used when:
- Implementing tests for code
- Practicing Test-Driven Development (TDD)
- Creating test cases
- Writing unit tests, integration tests, or E2E tests
- Planning test strategy
- Designing test suites

## Testing Approach

### Reference Specific Methodologies

When implementing tests, reference **specific, concrete testing methodologies**:

#### Recommended References:
- **"t_wada's TDD"** - Test-Driven Development approach by t_wada
- **"Kent Beck's Test-Driven Development"** - Classic TDD methodology
- **Specific testing frameworks' best practices** (Jest, pytest, RSpec, etc.)

#### Why Specific References Matter:
- Generic terms like "best practices" are less effective
- Concrete methodologies provide clear guidelines
- Established approaches have proven track records
- Specific references enable deeper learning

### Test Implementation Process

When implementing tests, follow this pattern:

#### 1. Provide Test Skeleton First

Start with the **structure** of test cases:

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

#### 2. Framework Guides Implementation

Benefits of skeleton-first approach:
- Provides structure for actual implementation
- Helps identify edge cases early
- Ensures comprehensive coverage
- Makes it clear what needs to be tested
- Facilitates discussion of test scope

#### 3. Fill in Details After Confirmation

Once the skeleton is confirmed:
- Implement each test case
- Add setup and teardown logic
- Include assertions and expectations
- Add test data and fixtures

## Test-Driven Development (TDD) Cycle

Follow the classic TDD cycle:

### Red-Green-Refactor

1. **Red**: Write a failing test
   - Write test first, before implementation
   - Test should fail because feature doesn't exist yet
   - Verify that test actually fails

2. **Green**: Make the test pass
   - Write minimal code to make test pass
   - Don't worry about perfection yet
   - Focus on making it work

3. **Refactor**: Improve the code
   - Clean up implementation
   - Remove duplication
   - Improve design
   - Ensure tests still pass

### Benefits of TDD

- Drives design from usage perspective
- Ensures testable code
- Provides immediate feedback
- Creates regression test suite
- Documents expected behavior

## Test Case Design

### What to Test

Focus on:
- **Happy path**: Expected usage scenarios
- **Edge cases**: Boundary conditions, empty inputs
- **Error cases**: Invalid inputs, error handling
- **Integration points**: Interaction with dependencies

### Test Naming

Use descriptive test names:

```typescript
// Good
it('should return 404 when user not found')
it('should validate email format before saving')

// Less clear
it('works correctly')
it('test1')
```

## Best Practices

### DO:
- Reference specific testing methodologies (t_wada's TDD, Kent Beck's TDD)
- Provide test skeleton/structure first
- Write tests before implementation (TDD)
- Test behavior, not implementation details
- Keep tests focused and independent
- Use descriptive test names

### DON'T:
- Use generic "best practices" without specifics
- Jump straight to implementation without structure
- Write implementation before tests (in TDD)
- Test internal implementation details
- Create dependent tests
- Use vague test names

## Example Workflow

```markdown
1. **Discuss test strategy**:
   "マスター、t_wadaのTDDアプローチを使ってテストを実装するね"

2. **Provide skeleton**:
   "まず、テストケースの構造を作るよ"
   [Show test skeleton with describe/it blocks]

3. **Confirm scope**:
   "これでカバーすべきケースは揃っているかな？"

4. **Implement tests**:
   "それじゃあ、各テストケースを実装していくね"
   [Implement test cases one by one]

5. **Follow TDD cycle**:
   "テストが失敗することを確認してから、実装を書くよ"
```

## Important Notes

- Concrete methodologies > Generic "best practices"
- Structure before details
- TDD is a design tool, not just a testing tool
- Tests document expected behavior
- Good tests enable confident refactoring
