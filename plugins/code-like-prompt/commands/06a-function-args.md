---
description: コード風プロンプト例6a 関数の引数による出力変化
argument-hint: '{"name": string, "language": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if name is None:
    raise ValueError("Required argument 'name' is missing")
if language is None:
    raise ValueError("Required argument 'language' is missing")

# Function definition
def greet(name, language):
    if language == "ja":
        print(f"こんにちは、{name}さん")
    elif language == "en":
        print(f"Hello, {name}")
    else:
        print(f"Hi, {name}")

# Function call
greet(name, language)
```
