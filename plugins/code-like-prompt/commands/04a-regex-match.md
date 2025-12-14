---
description: コード風プロンプト例4a 正規表現マッチング
argument-hint: '{"text": string}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
import re

# Validate required arguments
if text is None:
    raise ValueError("Required argument 'text' is missing")

# Pattern matching
if re.match(r"^foo.*bar$", text):
    print("qux")
elif re.match(r"^baz", text):
    print("quux")
else:
    print("corge")
```
