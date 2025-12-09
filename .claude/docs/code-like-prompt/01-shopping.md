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

Both commands accept:
- `--milk-stock=N`: Sets Milk.stock to N (default: asks via gets)
- `--egg-stock=N`: Sets Egg.stock to N (default: asks via gets)

Note: `Egg.exists?` returns true if `Egg.stock > 0`, false otherwise.

### Interactive vs Non-Interactive Mode

**Interactive mode** (no arguments): Commands will prompt for input:
```
Input: Milk
[user enters stock value]
Input: Egg
[user enters stock value]
```

**Non-interactive mode** (with arguments):
```bash
claude -p "/code-like-prompt:01-shopping-request --milk-stock=5 --egg-stock=3"
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
