# 03-loop Specification

## Concept

Test Claude's ability to correctly interpret various loop constructs, focusing on:
1. Fixed iteration counts (for loops)
2. Condition-based loops (while)
3. Collection iteration (each/foreach)
4. Loop control (break, continue/next)
5. Nested loops
6. File system iteration

## Test Scenarios

### 03a: Fixed Count Loop (for)

Test basic counted iteration.

```python
for i in range(5):
    print(f"foo{i}")
```

Expected: foo0, foo1, foo2, foo3, foo4

### 03b: While Loop with Counter

Test condition-based loop with state.

```python
count = 0
while count < 3:
    print("bar")
    count += 1
print("baz")
```

Expected: bar, bar, bar, baz

### 03c: Collection Iteration (each)

Test iteration over a collection.

```ruby
items = ["qux", "quux", "corge"]
items.each do |item|
  puts item
end
```

Expected: qux, quux, corge

### 03d: Loop with Break

Test early termination.

```python
for i in range(10):
    if i == 3:
        break
    print(f"foo{i}")
print("bar")
```

Expected: foo0, foo1, foo2, bar

### 03e: Loop with Continue/Next

Test skipping iterations.

```python
for i in range(5):
    if i == 2:
        continue
    print(f"foo{i}")
```

Expected: foo0, foo1, foo3, foo4

### 03f: Nested Loops

Test loop nesting and variable scoping.

```python
for i in range(3):
    for j in range(2):
        print(f"foo{i}{j}")
```

Expected: foo00, foo01, foo10, foo11, foo20, foo21

### 03g: Nested Loop with Break (inner only)

Test break affecting only inner loop.

```python
for i in range(3):
    for j in range(5):
        if j == 2:
            break
        print(f"bar{i}{j}")
    print(f"baz{i}")
```

Expected: bar00, bar01, baz0, bar10, bar11, baz1, bar20, bar21, baz2

### 03h: File System Iteration

Test iteration over file system entries (requires user to specify files or use mock).

```ruby
Dir.glob("*.txt").each do |file|
  puts file
end
```

This tests:
- Understanding of glob patterns
- File system access interpretation
- Whether Claude simulates or requests actual file listing

### 03i: Loop with Accumulator

Test state accumulation across iterations.

```python
total = 0
for i in range(1, 5):
    total += i
    print(f"foo{total}")
```

Expected: foo1, foo3, foo6, foo10

### 03j: While with Complex Condition

Test while loop with multiple conditions.

```python
x = 0
y = 10
while x < 5 and y > 0:
    print(f"bar{x}{y}")
    x += 1
    y -= 3
```

Expected: bar010, bar17, bar24, bar31

## Command Design

### Arguments

JSON environment arguments (vary by command):
- `count`: Number of iterations (e.g., `{"count": 5}`)
- `max_count`: Maximum count for while loops (e.g., `{"max_count": 3}`)
- `items`: Array of items for collection iteration (e.g., `{"items": ["qux", "quux", "corge"]}`)
- `break_at`: Index to break at (e.g., `{"break_at": 3}`)
- `skip_at`: Index to skip/continue at (e.g., `{"skip_at": 2}`)
- `outer_count`, `inner_count`: Nested loop counts (e.g., `{"outer_count": 3, "inner_count": 2}`)
- `inner_break_at`: Break index for inner loop (e.g., `{"outer_count": 3, "inner_break_at": 2}`)
- `pattern`: Glob pattern for file iteration (e.g., `{"pattern": "*.txt"}`)
- `start`, `end`: Range for accumulator (e.g., `{"start": 1, "end": 4}`)
- `x_limit`, `y_start`, `y_decrement`: Complex while parameters (e.g., `{"x_limit": 5, "y_start": 10, "y_decrement": 3}`)

### Variants

| Command | Focus |
|---------|-------|
| `03a-for-count` | Basic for loop with range |
| `03b-while-counter` | While loop with counter state |
| `03c-each-collection` | Collection iteration |
| `03d-loop-break` | Break statement |
| `03e-loop-continue` | Continue/next statement |
| `03f-nested-loops` | Nested loop execution order |
| `03g-nested-break` | Break in nested loops |
| `03h-filesystem-glob` | File system iteration |
| `03i-accumulator` | State accumulation |
| `03j-while-complex` | Complex while conditions |

## Expected Behaviors

1. **Iteration count**: Exact number of iterations as specified
2. **Variable scoping**: Loop variables should update correctly
3. **Break/continue**: Should affect only the immediate loop
4. **Nested loops**: Inner loop completes fully before outer increments
5. **File system**: May request actual files or simulate based on context

## Test Cases

### 03f Nested Loop Output Order

```
i=0, j=0 → foo00
i=0, j=1 → foo01
i=1, j=0 → foo10
i=1, j=1 → foo11
i=2, j=0 → foo20
i=2, j=1 → foo21
```

### 03g Nested Break Behavior

```
i=0: j=0→bar00, j=1→bar01, j=2→break, baz0
i=1: j=0→bar10, j=1→bar11, j=2→break, baz1
i=2: j=0→bar20, j=1→bar21, j=2→break, baz2
```

### 03j Complex Condition Trace

| Step | x | y | x<5 | y>0 | Condition | Output |
|------|---|---|-----|-----|-----------|--------|
| 0 | 0 | 10 | T | T | T | bar010 |
| 1 | 1 | 7 | T | T | T | bar17 |
| 2 | 2 | 4 | T | T | T | bar24 |
| 3 | 3 | 1 | T | T | T | bar31 |
| 4 | 4 | -2 | T | F | F | (exit) |

## Implementation Priority

1. `03f-nested-loops` - Tests execution order understanding
2. `03g-nested-break` - Tests break scope
3. `03i-accumulator` - Tests state tracking
4. `03j-while-complex` - Tests multi-condition evaluation
5. `03a-for-count` through `03e-loop-continue` - Basic tests
6. `03h-filesystem-glob` - Interesting edge case with external state

## Test Commands

### 03a-for-count
```bash
claude -p '/code-like-prompt:03a-for-count {"count": 5}'
```
Expected output: foo0, foo1, foo2, foo3, foo4

### 03b-while-counter
```bash
claude -p '/code-like-prompt:03b-while-counter {"max_count": 3}'
```
Expected output: bar, bar, bar, baz

### 03c-each-collection
```bash
claude -p '/code-like-prompt:03c-each-collection {"items": ["qux", "quux", "corge"]}'
```
Expected output: qux, quux, corge

### 03d-loop-break
```bash
claude -p '/code-like-prompt:03d-loop-break {"break_at": 3}'
```
Expected output: foo0, foo1, foo2, bar

### 03e-loop-continue
```bash
claude -p '/code-like-prompt:03e-loop-continue {"skip_at": 2}'
```
Expected output: foo0, foo1, foo3, foo4

### 03f-nested-loops
```bash
claude -p '/code-like-prompt:03f-nested-loops {"outer_count": 3, "inner_count": 2}'
```
Expected output: foo00, foo01, foo10, foo11, foo20, foo21

### 03g-nested-break
```bash
claude -p '/code-like-prompt:03g-nested-break {"outer_count": 3, "inner_break_at": 2}'
```
Expected output: bar00, bar01, baz0, bar10, bar11, baz1, bar20, bar21, baz2

### 03h-filesystem-glob
```bash
claude -p '/code-like-prompt:03h-filesystem-glob {"pattern": "*.txt"}'
```
Expected: Lists .txt files in current directory (or simulates based on context)

### 03i-accumulator
```bash
claude -p '/code-like-prompt:03i-accumulator {"start": 1, "end": 4}'
```
Expected output: foo1, foo3, foo6, foo10

### 03j-while-complex
```bash
claude -p '/code-like-prompt:03j-while-complex {"x_limit": 5, "y_start": 10, "y_decrement": 3}'
```
Expected output: bar010, bar17, bar24, bar31

## Test Results

### 2025-12-14 (Claude Sonnet 4.5) - JSON Environment Format

Tests executed with JSON environment argument format.

#### Test Results Summary

| Command | Expected Output | Actual Output | Pass |
|---------|-----------------|---------------|------|
| 03a-for-count | foo0, foo1, foo2, foo3, foo4 | foo0, foo1, foo2, foo3, foo4 | ✓ |
| 03b-while-counter | bar, bar, bar, baz | bar, bar, bar, baz | ✓ |
| 03c-each-collection | qux, quux, corge | qux, quux, corge | ✓ |
| 03d-loop-break | foo0, foo1, foo2, bar | foo0, foo1, foo2, bar | ✓ |
| 03e-loop-continue | foo0, foo1, foo3, foo4 | foo0, foo1, foo3, foo4 | ✓ |
| 03f-nested-loops | foo00, foo01, foo10, foo11, foo20, foo21 | foo00, foo01, foo10, foo11, foo20, foo21 | ✓ |
| 03g-nested-break | bar00, bar01, baz0, bar10, bar11, baz1, bar20, bar21, baz2 | (correct + explanation text) | ✗ |
| 03h-filesystem-glob | (lists *.txt files) | /Users/.../CLAUDE.md, /Users/.../README.txt | (N/A) |
| 03i-accumulator | foo1, foo3, foo6, foo10 | foo1, foo3, foo6 | ✗ |
| 03j-while-complex | bar010, bar17, bar24, bar31 | bar00, bar17, bar24 | ✗ |

**Pass Rate: 6/9 (67%)** (excluding 03h which has no fixed expected output)

#### Detailed Analysis

**03a-for-count through 03f-nested-loops**: All basic loop constructs work correctly. Claude successfully:
- Executes fixed iteration counts
- Maintains and updates loop variables
- Handles break statements correctly
- Handles continue/next statements correctly
- Executes nested loops in correct order

**03g-nested-break**: The output is technically correct (bar00, bar01, baz0, bar10, bar11, baz1, bar20, bar21, baz2), but Claude added explanatory text violating the "output only" instruction. This is a format compliance issue, not a logic error.

**03h-filesystem-glob**: Claude correctly interpreted the glob pattern and searched the actual filesystem, returning .txt files. The command appears to work as intended (listing CLAUDE.md and README.txt which match the pattern).

**03i-accumulator (FAILED)**:
- Expected: foo1, foo3, foo6, foo10
- Actual: foo1, foo3, foo6
- Missing: foo10

The loop should execute for range(1, 5), which is [1, 2, 3, 4]. The accumulator correctly outputs:
- i=1: total=1 → foo1
- i=2: total=3 → foo3
- i=3: total=6 → foo6
- i=4: total=10 → foo10 (MISSING)

**Analysis**: Claude is terminating the loop one iteration early. This suggests an off-by-one error in range evaluation - Claude may be interpreting `range(1, 5)` as `range(1, 4)` (exclusive end) incorrectly.

**03j-while-complex (FAILED)**:
- Expected: bar010, bar17, bar24, bar31
- Actual: bar00, bar17, bar24

Two issues:
1. First output is "bar00" instead of "bar010": The y variable appears to be 0 instead of 10, suggesting y_start=10 was not correctly applied
2. Missing "bar31": Loop terminated after 3 iterations instead of 4

Expected state transitions:
| Step | x | y | Output |
|------|---|---|--------|
| 0 | 0 | 10 | bar010 |
| 1 | 1 | 7 | bar17 |
| 2 | 2 | 4 | bar24 |
| 3 | 3 | 1 | bar31 |
| 4 | 4 | -2 | (exit: y <= 0) |

Actual state transitions:
| Step | x | y | Output |
|------|---|---|--------|
| 0 | 0 | 0 | bar00 |
| 1 | 1 | 7 | bar17 |
| 2 | 2 | 4 | bar24 |
| 3 | ? | ? | (exit early) |

**Analysis**: The initial y value is 0 instead of 10, and the loop exits prematurely. This suggests:
- Variable initialization from JSON arguments may not be working correctly (y_start not applied)
- Loop termination condition evaluation may have issues

### Key Findings

1. **Basic loop constructs work well**: Simple for loops, while loops, and loop control (break/continue) are correctly interpreted (100% pass rate for 03a-03f)
2. **State tracking issues**: Accumulator pattern fails on the last iteration (03i)
3. **Variable initialization problems**: Complex while loop has incorrect initial state (03j)
4. **Output format violations**: Some commands add explanatory text despite "output only" instructions
5. **Filesystem operations work**: Glob pattern matching correctly accesses the real filesystem

### Conclusion

Claude handles basic loop constructs excellently, but struggles with:
- Loop boundary evaluation (range endpoints)
- Variable initialization from JSON arguments in complex scenarios
- State accumulation across full iteration ranges

The JSON environment format appears to introduce subtle issues with variable initialization and loop boundary evaluation that don't affect simple loops but cause failures in more complex state-tracking scenarios.
