---
description: コード風プロンプト例6b 関数の返り値の評価
argument-hint: '{"base": number, "bonus": boolean}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if base is None:
    raise ValueError("Required argument 'base' is missing")
if bonus is None:
    raise ValueError("Required argument 'bonus' is missing")

# Function definition
def calculate_score(base, bonus):
    if bonus:
        return base * 2
    else:
        return base

# Function call and variable assignment
score = calculate_score(base, bonus)

# Conditional evaluation using the returned value
if score >= 100:
    print("Excellent")
elif score >= 50:
    print("Good")
else:
    print("Try again")
```
