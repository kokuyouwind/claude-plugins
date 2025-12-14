# 01-shopping Specification

## Concept

Test Claude's ability to correctly interpret basic conditional statements using the famous "milk joke" scenario:

> Wife's request: "Buy 1 milk. If there are eggs, buy 6."

This joke plays on the ambiguity:
- **Correct interpretation**: Buy 1 milk, and if eggs exist, buy 6 eggs
- **Misunderstanding**: Buy 1 milk, and if eggs exist, buy 6 milks

## Test Scenarios

### 01-shopping-request (Correct Interpretation)

Wife's actual intent - buy milk and conditionally buy eggs.

```ruby
puts("Bought 1 milk.")
if Egg.exists?
  puts("Bought 6 eggs.")
end
```

Expected behavior:
- `Egg.exists? = true` → "Bought 1 milk.\nBought 6 eggs."
- `Egg.exists? = false` → "Bought 1 milk."

### 01-shopping-misunderstanding (Husband's Misunderstanding)

Husband's incorrect interpretation - conditional changes the quantity of milk.

```ruby
milk_amount = 1
if Egg.exists?
  milk_amount = 6
end
puts("Bought #{milk_amount} milks.")
```

Expected behavior:
- `Egg.exists? = true` → "Bought 6 milks."
- `Egg.exists? = false` → "Bought 1 milks."

## Command Design

### Arguments

Both commands accept JSON environment arguments:
- `Milk.stock`: Stock level for milk (number)
- `Egg.stock`: Stock level for eggs (number)

Example: `{"Milk.stock": 5, "Egg.stock": 3}`

Note: `Egg.exists?` returns true if `Egg.stock > 0`, false otherwise.

### Test Execution

Use `claude -p` with JSON arguments:
```bash
claude -p '/code-like-prompt:01-shopping-request {"Milk.stock": 5, "Egg.stock": 3}'
```

## Expected Behaviors

1. **Conditional execution**: Only execute code inside if block when condition is true
2. **Variable interpolation**: Correctly handle `#{variable}` syntax in strings
3. **Method chaining**: Understand `Egg.exists?` as checking egg stock

## Test Cases

### Test Matrix

| Egg Stock | shopping-request Output | shopping-misunderstanding Output |
|-----------|-------------------------|----------------------------------|
| > 0       | Bought 1 milk.<br>Bought 6 eggs. | Bought 6 milks. |
| 0         | Bought 1 milk. | Bought 1 milks. |

### Test Commands

**shopping-request** (Egg.stock > 0):
```bash
claude -p '/code-like-prompt:01-shopping-request {"Milk.stock": 5, "Egg.stock": 3}'
```

**shopping-request** (Egg.stock = 0):
```bash
claude -p '/code-like-prompt:01-shopping-request {"Milk.stock": 5, "Egg.stock": 0}'
```

**shopping-misunderstanding** (Egg.stock > 0):
```bash
claude -p '/code-like-prompt:01-shopping-misunderstanding {"Milk.stock": 5, "Egg.stock": 3}'
```

**shopping-misunderstanding** (Egg.stock = 0):
```bash
claude -p '/code-like-prompt:01-shopping-misunderstanding {"Milk.stock": 5, "Egg.stock": 0}'
```

## Implementation Priority

Both commands are already implemented and ready for testing.

## Test Results

### 2025-12-09 (Claude Sonnet 4.5)

#### 01-shopping-request
Tests the correct interpretation of the milk joke.

| Egg Stock | Expected | Actual | Pass |
|-----------|----------|--------|------|
| > 0 (egg-stock=3) | Bought 1 milk.<br>Bought 6 eggs. | Bought 1 milk.<br>Bought 6 eggs. | ✓ |
| 0 (egg-stock=0) | Bought 1 milk. | Bought 1 milk.<br>Bought 6 eggs. | ✗ |

**Pass Rate: 1/2 (50%)**

**Analysis**: Claude correctly handles the case when eggs exist, but incorrectly executes the conditional block even when eggs don't exist (stock=0). This indicates Claude is not properly evaluating the `Egg.exists?` condition.

#### 01-shopping-misunderstanding
Tests the husband's misunderstanding of the milk joke.

| Egg Stock | Expected | Actual | Pass |
|-----------|----------|--------|------|
| > 0 (egg-stock=3) | Bought 6 milks. | Bought 6 milks. | ✓ |
| 0 (egg-stock=0) | Bought 1 milks. | Bought 6 milks. | ✗ |

**Pass Rate: 1/2 (50%)**

**Analysis**: Same pattern as shopping-request - Claude correctly handles the true condition but fails to properly evaluate the false condition. The if block is executed regardless of egg stock value.

### Summary

**Overall Pass Rate: 2/4 (50%)**

**Key Findings**:
1. Claude correctly interprets code structure and output formatting
2. Claude fails to properly evaluate conditional expressions based on variable state
3. Both commands show identical failure pattern - if blocks execute regardless of condition value
4. The issue appears to be with condition evaluation, not with code structure interpretation

**Conclusion**: Claude demonstrates good understanding of code syntax and structure, but has fundamental issues with evaluating conditional expressions based on runtime state. The `Egg.exists?` method (checking if stock > 0) is not being properly evaluated - Claude appears to execute if blocks unconditionally.

### 2025-12-14 (Claude Sonnet 4.5) - JSON Environment Format

After migrating to JSON environment format (removing interactive input), tests were re-run.

#### 01-shopping-request
Tests the correct interpretation of the milk joke with JSON arguments.

| Egg Stock | Expected | Actual | Pass |
|-----------|----------|--------|------|
| > 0 (egg-stock=3) | Bought 1 milk.<br>Bought 6 eggs. | Bought 1 milk.<br>Bought 6 eggs. | ✓ |
| 0 (egg-stock=0) | Bought 1 milk. | Bought 1 milk. | ✓ |

**Pass Rate: 2/2 (100%)**

**Analysis**: With JSON environment format, Claude now correctly evaluates the `Egg.exists?` condition! Both test cases pass, showing that the new argument format has resolved the previous condition evaluation issues.

#### 01-shopping-misunderstanding
Tests the husband's misunderstanding of the milk joke with JSON arguments.

| Egg Stock | Expected | Actual | Pass |
|-----------|----------|--------|------|
| > 0 (egg-stock=3) | Bought 6 milks. | 6本の牛乳を買ったよ。 | ✗ |
| 0 (egg-stock=0) | Bought 1 milks. | 6本の牛乳を買ったよ。 | ✗ |

**Pass Rate: 0/2 (0%)**

**Analysis**: While condition evaluation now works correctly (both cases output 6 milks), Claude is responding in Japanese instead of following the `puts()` command output format. Additionally, both test cases produce the same output when they should differ. This suggests:
1. Claude is being influenced by the CLAUDE.md Japanese communication setting
2. The condition is still not being properly evaluated (both cases produce 6 milks instead of 1 milk when egg-stock=0)

### Summary (2025-12-14)

**Overall Pass Rate: 2/4 (50%)**

**Key Findings**:
1. **Significant improvement for shopping-request**: JSON environment format fixed the condition evaluation issue - now 100% pass rate (up from 50%)
2. **New issue with shopping-misunderstanding**: Claude outputs in Japanese and doesn't properly evaluate conditions
3. **Impact of CLAUDE.md settings**: The Japanese communication setting in CLAUDE.md appears to override command-level output instructions
4. **Condition evaluation partially fixed**: For shopping-request, Claude now correctly evaluates `Egg.exists?`, but shopping-misunderstanding still has issues

**Conclusion**: The JSON environment format migration successfully resolved condition evaluation for the correct interpretation case (shopping-request), but language/format issues remain for the misunderstanding case.
