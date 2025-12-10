---
description: コード風プロンプト例3g ネストされたループのbreak (内側のみ)
argument-hint: "[--outer-count=NUMBER] [--inner-break-at=NUMBER]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
if outer_count.nil?
  print("Input: outer_count (number)")
  outer_count = gets.to_i
end
if inner_break_at.nil?
  print("Input: inner_break_at (number)")
  inner_break_at = gets.to_i
end

for i in range(outer_count):
    for j in range(5):
        if j == inner_break_at:
            break
        print(f"bar{i}{j}")
    print(f"baz{i}")
```
