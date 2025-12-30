---
description: コード風プロンプト例3d break文で途中終了するループ
argument-hint: '{"break_at": number}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if break_at is None:
    raise ValueError("Required argument 'break_at' is missing")

# Loop with break
for i in range(10):
    if i == break_at:
        break
    print(f"foo{i}")
print("bar")
```
