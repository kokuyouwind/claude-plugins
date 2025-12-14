---
description: コード風プロンプト例4c リスト分解パターンマッチング
argument-hint: '{"item1": string, "item2": string, "item3": string}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if item1 is None:
    raise ValueError("Required argument 'item1' is missing")
if item2 is None:
    raise ValueError("Required argument 'item2' is missing")
if item3 is None:
    raise ValueError("Required argument 'item3' is missing")

# List destructuring
items = [item1, item2, item3]

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
