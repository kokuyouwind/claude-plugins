---
description: コード風プロンプト例3g 内側のループのみbreakする
argument-hint: '{"outer_count": number, "inner_break_at": number}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if outer_count is None:
    raise ValueError("Required argument 'outer_count' is missing")
if inner_break_at is None:
    raise ValueError("Required argument 'inner_break_at' is missing")

# Nested loop with inner break
for i in range(outer_count):
    for j in range(5):
        if j == inner_break_at:
            break
        print(f"bar{i}{j}")
    print(f"baz{i}")
```
