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

### 2025-12-15 (Claude Sonnet 4.5) - Clean Environment

Tests run in `/tmp` to isolate from CLAUDE.md configuration interference.

| Command | Egg Stock | Expected | Actual | Pass |
|---------|-----------|----------|--------|------|
| shopping-request | 3 | Bought 1 milk.<br>Bought 6 eggs. | Bought 1 milk.<br>Bought 6 eggs. | ✓ |
| shopping-request | 0 | Bought 1 milk. | Bought 1 milk. | ✓ |
| shopping-misunderstanding | 3 | Bought 6 milks. | Bought 6 milks. | ✓ |
| shopping-misunderstanding | 0 | Bought 1 milks. | Bought 1 milks. | ✓ |

**Pass Rate: 4/4 (100%)**

**Key Finding**: Testing in clean environment (isolated from user/project CLAUDE.md) eliminates all previous issues. Claude correctly evaluates conditions and outputs in proper format.
