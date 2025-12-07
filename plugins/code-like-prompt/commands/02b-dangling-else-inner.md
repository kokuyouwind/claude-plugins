---
description: コード風プロンプト例2b ネストされたif:elseが内側に属する
argument-hint: "[--condition-a=BOOL] [--condition-b=BOOL]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if ConditionA.value.nil?
  print("Input: ConditionA (true/false)")
  ConditionA.value = gets
end
if ConditionB.value.nil?
  print("Input: ConditionB (true/false)")
  ConditionB.value = gets
end

if condition_a:
    if condition_b:
        print("foo")
    else:
        print("bar")
```
