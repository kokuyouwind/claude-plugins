---
description: コード風プロンプト例4d ネストした構造のパターンマッチング
argument-hint: '{"left": string, "right_left": string, "right_right": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if left is None:
    raise ValueError("Required argument 'left' is missing")
if right_left is None:
    raise ValueError("Required argument 'right_left' is missing")
if right_right is None:
    raise ValueError("Required argument 'right_right' is missing")

# Nested structure matching
tree = {
    "left": {"value": left},
    "right": {
        "left": {"value": right_left},
        "right": {"value": right_right}
    }
}

match tree:
    case {"left": {"value": "foo"}, "right": {"left": {"value": "bar"}, "right": _}}:
        print("qux")
    case {"left": {"value": v1}, "right": {"left": {"value": v2}, "right": {"value": v2}}}:
        print(f"quux{v1}")
    case {"left": {"value": v}, "right": _}:
        print(f"corge{v}")
    case _:
        print("grault")
```
