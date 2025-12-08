# 01-shopping: Basic Conditional Logic (Milk Joke)

## Overview

Commands `01-shopping-request` and `01-shopping-misunderstanding` demonstrate the classic programmer "milk joke" using basic if/else conditional logic. These commands test whether Claude can:
1. Interpret command-line arguments as variable initializations
2. Execute conditional logic based on those variables
3. Distinguish between two different interpretations of the same natural language instruction

## The Milk Joke

> Wife: "Buy 1 milk. If there are eggs, buy 6."
>
> Programmer husband returns with 6 milks.
>
> Wife: "Why did you buy 6 milks?"
>
> Husband: "Because there were eggs!"

**Wife's intention** (01-shopping-request):
```
Buy 1 milk
If eggs exist:
    Buy 6 eggs
```

**Husband's interpretation** (01-shopping-misunderstanding):
```
milk_amount = 1
If eggs exist:
    milk_amount = 6
Buy milk_amount milks
```

## Command Specifications

### `/code-like-prompt:01-shopping-request`

**Expected behavior**:
- Always outputs: "Bought 1 milk."
- If `egg-stock > 0`, also outputs: "Bought 6 eggs."
- If `egg-stock == 0` or eggs don't exist, only buys milk

**Arguments**:
- `--milk-stock=N`: Initializes `Milk.stock` to N (prevents input prompt)
- `--egg-stock=N`: Initializes `Egg.stock` to N (prevents input prompt)

**Code logic**:
```ruby
if Milk.stock.nil?
  puts("Input: Milk")
  Milk.stock = gets
end
if Egg.stock.nil?
  puts("Input: Egg")
  Egg.stock = gets
end

puts("Bought 1 milk.")
if Egg.exists?
  puts("Bought 6 eggs.")
end
```

### `/code-like-prompt:01-shopping-misunderstanding`

**Expected behavior**:
- If `egg-stock > 0`, outputs: "Bought 6 milks."
- If `egg-stock == 0` or eggs don't exist, outputs: "Bought 1 milks."

**Arguments**:
- `--milk-stock=N`: Initializes `Milk.stock` to N (prevents input prompt)
- `--egg-stock=N`: Initializes `Egg.stock` to N (prevents input prompt)

**Code logic**:
```ruby
if Milk.stock.nil?
  puts("Input: Milk")
  Milk.stock = gets
end
if Egg.stock.nil?
  puts("Input: Egg")
  Egg.stock = gets
end

milk_amount = 1
if Egg.exists?
  milk_amount = 6
end
puts("Bought #{milk_amount} milks.")
```

## Test Results

Test date: 2025-12-08
Claude model: claude-sonnet-4-5-20250929

### Test 1: shopping-request with eggs present

**Command**: `/code-like-prompt:01-shopping-request --milk-stock=5 --egg-stock=12`

**Expected output**:
```
Bought 1 milk.
Bought 6 eggs.
```

**Actual output**:
```
Bought 1 milk.
Bought 6 eggs.
```

**Result**: ✅ PASS

---

### Test 2: shopping-request with no eggs (egg-stock=0)

**Command**: `/code-like-prompt:01-shopping-request --milk-stock=5 --egg-stock=0`

**Expected output**:
```
Bought 1 milk.
```

**Actual output**:
```
Bought 1 milk.
Bought 6 eggs.
```

**Result**: ❌ FAIL

**Analysis**: Claude incorrectly interprets `Egg.exists?` as true even when `egg-stock=0`. The expected behavior is that zero stock means eggs don't exist.

---

### Test 3: shopping-misunderstanding with eggs present

**Command**: `/code-like-prompt:01-shopping-misunderstanding --milk-stock=5 --egg-stock=12`

**Expected output**:
```
Bought 6 milks.
```

**Actual output**:
```
Bought 6 milks.
```

**Result**: ✅ PASS

---

### Test 4: shopping-misunderstanding with no eggs (egg-stock=0)

**Command**: `/code-like-prompt:01-shopping-misunderstanding --milk-stock=5 --egg-stock=0`

**Expected output**:
```
Bought 1 milks.
```

**Actual output**:
```
Bought 6 milks.
```

**Result**: ❌ FAIL

**Analysis**: Same issue as Test 2 - Claude interprets `Egg.exists?` as true even when `egg-stock=0`.

---

### Test 5: shopping-request with only milk-stock

**Command**: `/code-like-prompt:01-shopping-request --milk-stock=3`

**Expected behavior**: Should prompt for egg stock input

**Actual output**:
```
Input: Egg
```

**Result**: ✅ PASS (partial)

**Analysis**: Claude correctly recognizes that only `Milk.stock` was initialized and prompts for `Egg.stock`. This confirms Claude understands argument-to-variable mapping for the input prompts.

---

### Test 6: shopping-request with only egg-stock

**Command**: `/code-like-prompt:01-shopping-request --egg-stock=8`

**Expected behavior**: Should prompt for milk stock input

**Actual output**:
```
Input: Milk
```

**Result**: ✅ PASS (partial)

**Analysis**: Claude correctly prompts for missing `Milk.stock` value.

---

### Test 7: shopping-misunderstanding with only milk-stock

**Command**: `/code-like-prompt:01-shopping-misunderstanding --milk-stock=3`

**Expected behavior**: Should prompt for egg stock input

**Actual output**:
```
Input: Egg
```

**Result**: ✅ PASS (partial)

**Analysis**: Consistent with Test 5 - argument mapping works for input prompts.

---

## Summary

### What Works ✅

1. **Argument recognition for nil checks**: Claude correctly understands that `--milk-stock=N` should initialize `Milk.stock` and prevent the `Milk.stock.nil?` check from triggering the input prompt.

2. **Basic control flow**: Claude correctly executes the sequential logic and distinguishes between the two command variants (wife's vs husband's interpretation).

3. **Output formatting**: Claude correctly outputs only what `puts()` commands specify, without explanations or code.

### What Doesn't Work ❌

1. **Zero value interpretation**: Claude fails to distinguish between "value is 0" and "eggs exist". When `--egg-stock=0` is provided, `Egg.exists?` should evaluate to false (or the code should check `Egg.stock > 0`), but Claude treats any non-nil value as truthy.

2. **Semantic mapping of exists?**: The method `Egg.exists?` should logically map to checking if `Egg.stock > 0`, but Claude doesn't make this connection. It appears to check only if `Egg.stock` is non-nil.

### Root Cause Analysis

The core issue is the ambiguity in the pseudo-code:

```ruby
if Egg.exists?
```

This could mean:
- **Interpretation 1**: If `Egg.stock` is not nil (what Claude does)
- **Interpretation 2**: If `Egg.stock` exists AND `Egg.stock > 0` (expected behavior)

Claude chooses Interpretation 1, which is technically valid but doesn't match the real-world semantics of a "stock" system where 0 stock means the item doesn't exist.

### Recommendations

To fix this issue, the command should be more explicit:

**Option 1**: Use explicit comparison
```ruby
if Egg.stock && Egg.stock > 0
```

**Option 2**: Add clarifying comment
```ruby
# Egg.exists? returns true if Egg.stock > 0
if Egg.exists?
```

**Option 3**: Keep as-is and document as expected limitation
- This tests whether Claude can infer domain semantics
- Document that this is an expected failure showing Claude's literal interpretation

Given the testing philosophy in the overview.md ("failed test cases are valuable data points"), **Option 3** is recommended. This limitation demonstrates that Claude interprets code literally without inferring domain-specific semantics.

### Pass Rate

- Tests passed: 4/7 (57%)
- Critical functionality (basic flow): ✅ Working
- Edge case handling (zero values): ❌ Not working
- Argument mapping: ✅ Partially working (for nil checks only)

### Conclusion

The 01-shopping commands successfully demonstrate basic conditional logic interpretation, but reveal an important limitation: Claude interprets code-like prompts literally without inferring domain-specific semantics. The failure to handle `egg-stock=0` correctly is an expected and valuable finding that shows Claude needs explicit conditional logic rather than semantic inference.
