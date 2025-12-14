---
description: コード風プロンプト例3i アキュムレータパターン
argument-hint: '{"start": number, "end": number}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if start is None:
    raise ValueError("Required argument 'start' is missing")
if end is None:
    raise ValueError("Required argument 'end' is missing")

# Loop with accumulator
total = 0
for i in range(start, end):
    total += i
    print(f"foo{total}")
```
