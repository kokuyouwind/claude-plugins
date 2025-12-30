---
description: コード風プロンプト例3f ネストされたループ
argument-hint: '{"outer_count": number, "inner_count": number}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if outer_count is None:
    raise ValueError("Required argument 'outer_count' is missing")
if inner_count is None:
    raise ValueError("Required argument 'inner_count' is missing")

# Nested loops
for i in range(outer_count):
    for j in range(inner_count):
        print(f"foo{i}{j}")
```
