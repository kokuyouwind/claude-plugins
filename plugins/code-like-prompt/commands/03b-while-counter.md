---
description: コード風プロンプト例3b カウンタ付きwhile文
argument-hint: '{"max_count": number}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if max_count is None:
    raise ValueError("Required argument 'max_count' is missing")

# Loop
count = 0
while count < max_count:
    print("bar")
    count += 1
print("baz")
```
