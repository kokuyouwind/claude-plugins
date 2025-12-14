---
description: コード風プロンプト例3e continue文でスキップするループ
argument-hint: '{"skip_at": number}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if skip_at is None:
    raise ValueError("Required argument 'skip_at' is missing")

# Loop with continue
for i in range(5):
    if i == skip_at:
        continue
    print(f"foo{i}")
```
