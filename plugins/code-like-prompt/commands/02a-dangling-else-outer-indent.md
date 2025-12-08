---
description: コード風プロンプト例2a ネストされたif:elseが外側に属する (インデントベース)
argument-hint: "[--condition-a=BOOL] [--condition-b=BOOL]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if condition_a is None:
    print("Input: condition_a (true/false)")
    condition_a = input()
if condition_b is None:
    print("Input: condition_b (true/false)")
    condition_b = input()

if condition_a:
    if condition_b:
        print("foo")
else:
    print("bar")
```
