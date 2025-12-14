---
description: コード風プロンプト例4e 複数ガード条件のパターンマッチング
argument-hint: '{"x": number, "y": number}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if x is None:
    raise ValueError("Required argument 'x' is missing")
if y is None:
    raise ValueError("Required argument 'y' is missing")

# Pattern matching with guards
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
