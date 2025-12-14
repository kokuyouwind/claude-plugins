---
description: コード風プロンプト例2b ネストされたif:elseが内側に属する (インデントベース)
argument-hint: '{"condition_a": boolean, "condition_b": boolean}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if condition_a is None:
    raise ValueError("Required argument 'condition_a' is missing")
if condition_b is None:
    raise ValueError("Required argument 'condition_b' is missing")

# Logic
if condition_a:
    if condition_b:
        print("foo")
    else:
        print("bar")
```
