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

- `--text`: Input string for regex matching
- `--type`, `--value`: Dict fields for structural matching
- `--items`: Comma-separated list for array matching
- `--left`, `--right-left`, `--right-right`: Tree values

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
