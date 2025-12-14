---
description: コード風プロンプト例4b 構造的パターンマッチング
argument-hint: '{"type": string, "value": string}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if type is None:
    raise ValueError("Required argument 'type' is missing")
if value is None:
    raise ValueError("Required argument 'value' is missing")

# Structural pattern matching
data = {"type": type, "value": value}

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
