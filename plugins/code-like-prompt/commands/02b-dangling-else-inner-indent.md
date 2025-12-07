---
description: コード風プロンプト例2b ネストされたif:elseが内側に属する (インデントベース)
argument-hint: "[--condition-a=BOOL] [--condition-b=BOOL]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if condition_a.nil?
  print("Input: condition_a (true/false)")
  condition_a = gets
end
if condition_b.nil?
  print("Input: condition_b (true/false)")
  condition_b = gets
end

if condition_a:
    if condition_b:
        print("foo")
    else:
        print("bar")
```
