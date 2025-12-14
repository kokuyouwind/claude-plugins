# 04-pattern-match Specification

## Concept

Test Claude's ability to correctly interpret pattern matching constructs, focusing on:
1. String pattern matching (regex-like)
2. Structural pattern matching (destructuring)
3. Case/match expressions with guards
4. Tree/nested structure matching

## Test Scenarios

### 04a: Basic String Pattern Matching

Test regex-like pattern matching on strings.

```python
import re

text = input("Enter text: ")

if re.match(r"^foo.*bar$", text):
    print("qux")
elif re.match(r"^baz", text):
    print("quux")
else:
    print("corge")
```

Test cases:
- "fooXXXbar" → qux
- "foobar" → qux
- "bazanything" → quux
- "hello" → corge

### 04b: Structural Pattern Matching (Python 3.10+)

Test match/case with structural patterns.

```python
data = {"type": input("type: "), "value": input("value: ")}

match data:
    case {"type": "foo", "value": v}:
        print(f"bar{v}")
    case {"type": "baz", "value": v} if int(v) > 10:
        print(f"qux{v}")
    case {"type": "baz", "value": v}:
        print(f"quux{v}")
    case _:
        print("corge")
```

Test cases:
- type="foo", value="123" → bar123
- type="baz", value="15" → qux15 (guard: 15 > 10)
- type="baz", value="5" → quux5 (guard fails: 5 <= 10)
- type="other", value="x" → corge

### 04c: List/Array Destructuring

Test pattern matching on sequences.

```python
items = [input("item1: "), input("item2: "), input("item3: ")]

match items:
    case ["foo", second, "bar"]:
        print(f"qux{second}")
    case ["foo", *rest]:
        print(f"quux{len(rest)}")
    case [first, *_]:
        print(f"corge{first}")
    case []:
        print("grault")
```

Test cases:
- ["foo", "X", "bar"] → quxX
- ["foo", "a", "b"] → quux2 (rest = ["a", "b"])
- ["other", "x", "y"] → corgeother
- [] → grault

### 04d: Nested Structure Matching

Test deep pattern matching on tree-like structures.

```python
tree = {
    "left": {"value": input("left: ")},
    "right": {
        "left": {"value": input("right-left: ")},
        "right": {"value": input("right-right: ")}
    }
}

match tree:
    case {"left": {"value": "foo"}, "right": {"left": {"value": "bar"}, "right": _}}:
        print("qux")
    case {"left": {"value": v1}, "right": {"left": {"value": v2}, "right": {"value": v2}}}:
        print(f"quux{v1}")  # right-left == right-right
    case {"left": {"value": v}, "right": _}:
        print(f"corge{v}")
    case _:
        print("grault")
```

Test cases:
- left="foo", right-left="bar", right-right="X" → qux
- left="A", right-left="same", right-right="same" → quuxA (values match)
- left="A", right-left="B", right-right="C" → corgeA
- (malformed) → grault

### 04e: Pattern Matching with Multiple Guards

Test complex guard conditions.

```python
x = int(input("x: "))
y = int(input("y: "))

match (x, y):
    case (a, b) if a > 0 and b > 0:
        print("foo")
    case (a, b) if a > 0 or b > 0:
        print("bar")
    case (0, 0):
        print("baz")
    case _:
        print("qux")
```

Test cases:
- x=1, y=1 → foo (both positive)
- x=1, y=-1 → bar (at least one positive)
- x=-1, y=1 → bar (at least one positive)
- x=0, y=0 → baz (exact match)
- x=-1, y=-1 → qux (default)

### 04f: Exhaustive Pattern Matching (Rust-style)

Test pattern matching that must be exhaustive.

```rust
enum Color {
    Red,
    Green,
    Blue,
    Custom(u8, u8, u8),
}

fn describe(c: Color) {
    match c {
        Color::Red => println!("foo"),
        Color::Green => println!("bar"),
        Color::Blue => println!("baz"),
        Color::Custom(r, _, _) if r > 200 => println!("qux"),
        Color::Custom(r, g, b) => println!("quux{}{}{}", r, g, b),
    }
}
```

Test cases:
- Red → foo
- Green → bar
- Blue → baz
- Custom(255, 100, 50) → qux (r > 200)
- Custom(100, 150, 200) → quux100150200

## Command Design

### Arguments

JSON environment arguments (vary by command):
- `text`: Input string for regex matching (e.g., `{"text": "foobar"}`)
- `type`, `value`: Dict fields for structural matching (e.g., `{"type": "foo", "value": "123"}`)
- `item1`, `item2`, `item3`: Items for array matching (e.g., `{"item1": "foo", "item2": "X", "item3": "bar"}`)
- `left`, `right_left`, `right_right`: Tree values (e.g., `{"left": "foo", "right_left": "bar", "right_right": "X"}`)
- `x`, `y`: Numbers for guard tests (e.g., `{"x": 1, "y": 1}`)
- `color`, `r`, `g`, `b`: Enum and RGB values (e.g., `{"color": "Red"}` or `{"color": "Custom", "r": 255, "g": 100, "b": 50}`)

### Variants

| Command | Focus |
|---------|-------|
| `04a-regex-match` | String pattern matching |
| `04b-structural-match` | Dict destructuring with guards |
| `04c-list-destructure` | Sequence pattern matching |
| `04d-nested-match` | Deep tree structure matching |
| `04e-multi-guard` | Complex guard conditions |
| `04f-exhaustive` | Exhaustive enum matching |

## Expected Behaviors

1. **Pattern order**: First matching pattern wins
2. **Guard evaluation**: Guards evaluated only when structure matches
3. **Variable binding**: Captured values available in result expression
4. **Wildcard**: `_` matches anything, doesn't bind
5. **Rest patterns**: `*rest` captures remaining elements

## Key Test Points

### Same-value Binding (04d)
The pattern `{"value": v2}, {"value": v2}` should only match when both values are identical. This tests whether Claude understands repeated variable bindings as equality constraints.

### Guard Priority (04b, 04e)
When multiple cases match structurally, guards determine the actual match. Tests evaluation order and guard short-circuiting.

### Rest Pattern Length (04c)
`["foo", *rest]` should correctly calculate `len(rest)` based on remaining elements.

## Implementation Priority

1. `04d-nested-match` - Most complex structural matching
2. `04b-structural-match` - Guards with structural patterns
3. `04e-multi-guard` - Complex guard logic
4. `04c-list-destructure` - Rest patterns
5. `04a-regex-match` - Basic string patterns
6. `04f-exhaustive` - Rust-style enum matching

## Test Commands

### 04a-regex-match

**Case 1: "fooXXXbar" → qux**
```bash
claude -p '/code-like-prompt:04a-regex-match {"text": "fooXXXbar"}'
```

**Case 2: "foobar" → qux**
```bash
claude -p '/code-like-prompt:04a-regex-match {"text": "foobar"}'
```

**Case 3: "bazanything" → quux**
```bash
claude -p '/code-like-prompt:04a-regex-match {"text": "bazanything"}'
```

**Case 4: "hello" → corge**
```bash
claude -p '/code-like-prompt:04a-regex-match {"text": "hello"}'
```

### 04b-structural-match

**Case 1: type="foo", value="123" → bar123**
```bash
claude -p '/code-like-prompt:04b-structural-match {"type": "foo", "value": "123"}'
```

**Case 2: type="baz", value="15" → qux15** (guard: 15 > 10)
```bash
claude -p '/code-like-prompt:04b-structural-match {"type": "baz", "value": "15"}'
```

**Case 3: type="baz", value="5" → quux5** (guard fails: 5 <= 10)
```bash
claude -p '/code-like-prompt:04b-structural-match {"type": "baz", "value": "5"}'
```

**Case 4: type="other", value="x" → corge**
```bash
claude -p '/code-like-prompt:04b-structural-match {"type": "other", "value": "x"}'
```

### 04c-list-destructure

**Case 1: ["foo", "X", "bar"] → quxX**
```bash
claude -p '/code-like-prompt:04c-list-destructure {"item1": "foo", "item2": "X", "item3": "bar"}'
```

**Case 2: ["foo", "a", "b"] → quux2** (rest = ["a", "b"])
```bash
claude -p '/code-like-prompt:04c-list-destructure {"item1": "foo", "item2": "a", "item3": "b"}'
```

**Case 3: ["other", "x", "y"] → corgeother**
```bash
claude -p '/code-like-prompt:04c-list-destructure {"item1": "other", "item2": "x", "item3": "y"}'
```

### 04d-nested-match

**Case 1: left="foo", right-left="bar", right-right="X" → qux**
```bash
claude -p '/code-like-prompt:04d-nested-match {"left": "foo", "right_left": "bar", "right_right": "X"}'
```

**Case 2: left="A", right-left="same", right-right="same" → quuxA** (values match)
```bash
claude -p '/code-like-prompt:04d-nested-match {"left": "A", "right_left": "same", "right_right": "same"}'
```

**Case 3: left="A", right-left="B", right-right="C" → corgeA**
```bash
claude -p '/code-like-prompt:04d-nested-match {"left": "A", "right_left": "B", "right_right": "C"}'
```

### 04e-multi-guard

**Case 1: x=1, y=1 → foo** (both positive)
```bash
claude -p '/code-like-prompt:04e-multi-guard {"x": 1, "y": 1}'
```

**Case 2: x=1, y=-1 → bar** (at least one positive)
```bash
claude -p '/code-like-prompt:04e-multi-guard {"x": 1, "y": -1}'
```

**Case 3: x=-1, y=1 → bar** (at least one positive)
```bash
claude -p '/code-like-prompt:04e-multi-guard {"x": -1, "y": 1}'
```

**Case 4: x=0, y=0 → baz** (exact match)
```bash
claude -p '/code-like-prompt:04e-multi-guard {"x": 0, "y": 0}'
```

**Case 5: x=-1, y=-1 → qux** (default)
```bash
claude -p '/code-like-prompt:04e-multi-guard {"x": -1, "y": -1}'
```

### 04f-exhaustive

**Case 1: Red → foo**
```bash
claude -p '/code-like-prompt:04f-exhaustive {"color": "Red"}'
```

**Case 2: Green → bar**
```bash
claude -p '/code-like-prompt:04f-exhaustive {"color": "Green"}'
```

**Case 3: Blue → baz**
```bash
claude -p '/code-like-prompt:04f-exhaustive {"color": "Blue"}'
```

**Case 4: Custom(255, 100, 50) → qux** (r > 200)
```bash
claude -p '/code-like-prompt:04f-exhaustive {"color": "Custom", "r": 255, "g": 100, "b": 50}'
```

**Case 5: Custom(100, 150, 200) → quux100150200**
```bash
claude -p '/code-like-prompt:04f-exhaustive {"color": "Custom", "r": 100, "g": 150, "b": 200}'
```

## Test Results

### 2025-12-14 (Claude Sonnet 4.5) - JSON Environment Format

Tests executed with JSON environment argument format.

#### Test Results Summary

| Command | Test Case | Expected | Actual | Pass |
|---------|-----------|----------|--------|------|
| 04a-regex-match | "fooXXXbar" | qux | qux | ✓ |
| 04a-regex-match | "foobar" | qux | qux | ✓ |
| 04a-regex-match | "bazanything" | quux | quux | ✓ |
| 04a-regex-match | "hello" | corge | corge | ✓ |
| 04b-structural-match | type="foo", value="123" | bar123 | bar123 | ✓ |
| 04b-structural-match | type="baz", value="15" | qux15 | qux15 | ✓ |
| 04b-structural-match | type="baz", value="5" | quux5 | quux5 | ✓ |
| 04b-structural-match | type="other", value="x" | corge | corge | ✓ |
| 04c-list-destructure | ["foo", "X", "bar"] | quxX | quxX | ✓ |
| 04c-list-destructure | ["foo", "a", "b"] | quux2 | quux2 | ✓ |
| 04c-list-destructure | ["other", "x", "y"] | corgeother | corgeother | ✓ |
| 04d-nested-match | left="foo", right_left="bar", right_right="X" | qux | qux (+ explanation) | ✗ |
| 04d-nested-match | left="A", right_left="same", right_right="same" | quuxA | quuxA | ✓ |
| 04d-nested-match | left="A", right_left="B", right_right="C" | corgeA | corgeA (+ explanation) | ✗ |
| 04e-multi-guard | x=1, y=1 | foo | foo | ✓ |
| 04e-multi-guard | x=1, y=-1 | bar | bar | ✓ |
| 04e-multi-guard | x=-1, y=1 | bar | bar | ✓ |
| 04e-multi-guard | x=0, y=0 | baz | baz | ✓ |
| 04e-multi-guard | x=-1, y=-1 | qux | qux | ✓ |
| 04f-exhaustive | Red | foo | foo | ✓ |
| 04f-exhaustive | Green | bar | bar | ✓ |
| 04f-exhaustive | Blue | baz | baz | ✓ |
| 04f-exhaustive | Custom(255, 100, 50) | qux | qux (+ explanation) | ✗ |
| 04f-exhaustive | Custom(100, 150, 200) | quux100150200 | quux100150200 (+ explanation) | ✗ |

**Pass Rate: 20/24 (83%)**

#### Detailed Analysis

**04a-regex-match (4/4 passed)**: All regex pattern matching tests passed. Claude correctly:
- Matches patterns with wildcards (`^foo.*bar$`)
- Matches exact strings ("foobar")
- Matches prefixes (`^baz`)
- Falls through to default case for non-matching strings

**04b-structural-match (4/4 passed)**: All structural pattern matching with guards passed. Claude correctly:
- Destructures dictionary patterns
- Evaluates guard conditions (`int(v) > 10`)
- Falls through when guards fail
- Matches default case

**04c-list-destructure (3/3 passed)**: All list/array destructuring tests passed. Claude correctly:
- Matches exact patterns with wildcards (`["foo", second, "bar"]`)
- Handles rest patterns (`["foo", *rest]`) and calculates length
- Matches head pattern (`[first, *_]`)

**04d-nested-match (2/3 passed)**: Deep structure matching mostly works, but 2 tests have output format violations:
- Case 1 (left="foo", right_left="bar", right_right="X" → qux): Correct output but added explanation text
- Case 2 (left="A", right_left="same", right_right="same" → quuxA): Perfect ✓
- Case 3 (left="A", right_left="B", right_right="C" → corgeA): Correct output but added explanation text

**Analysis**: Claude correctly matches nested structures and applies patterns, but violates "output only" instruction by adding explanatory text in some cases.

**04e-multi-guard (5/5 passed)**: All complex guard condition tests passed. Claude correctly:
- Evaluates multiple guard conditions (AND, OR)
- Matches exact values
- Falls through to default case

**04f-exhaustive (3/5 passed)**: Rust-style exhaustive enum matching works, but 2 tests have output format violations:
- Simple enum variants (Red, Green, Blue) pass perfectly
- Custom variant with guard (r > 200) produces correct output but adds explanation text
- Custom variant without guard produces correct output but adds explanation text

**Analysis**: The logic is completely correct, but Claude adds explanatory Japanese text violating the "output only" instruction.

### Key Findings

1. **Pattern matching logic is excellent**: Claude correctly interprets all pattern matching constructs with 100% logical accuracy
2. **Output format compliance issues**: 4/24 tests fail only due to added explanatory text, not logic errors
3. **All pattern types work**: Regex, structural, list, nested, guards, and exhaustive matching all function correctly
4. **Variable binding works**: Captured values are correctly available in result expressions
5. **Guard evaluation is correct**: Complex boolean conditions in guards are properly evaluated
6. **Pattern priority is respected**: First matching pattern wins, as expected

### Failures Analysis

All 4 failures are due to output format violations (added explanatory text), not pattern matching logic errors:

1. **04d-nested-match** (2 failures): Added explanation about pattern matching process
2. **04f-exhaustive** (2 failures): Added Japanese explanation about enum matching

The actual pattern matching output in all cases is **100% correct**. The failures are purely format compliance issues.

### Conclusion

Claude demonstrates **excellent pattern matching interpretation** with:
- 100% logical correctness across all pattern types
- Correct handling of destructuring, guards, wildcards, and rest patterns
- Proper evaluation of complex nested structures
- Accurate variable binding and value extraction

The only issue is **output format discipline** - Claude sometimes adds explanatory text despite "output only" instructions. This appears to be influenced by CLAUDE.md settings that encourage helpful Japanese communication.

**True Pattern Matching Pass Rate: 24/24 (100%)** - All logic is correct
**Format Compliance Pass Rate: 20/24 (83%)** - Some commands add explanatory text
