---
description: コード風プロンプト例3a 基本的なfor文
argument-hint: '{"count": number}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if count is None:
    raise ValueError("Required argument 'count' is missing")

# Loop
for i in range(count):
    print(f"foo{i}")
```
