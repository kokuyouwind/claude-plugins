---
description: コード風プロンプト例3f ネストされたループの実行順序
argument-hint: "[--outer-count=NUMBER] [--inner-count=NUMBER]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
if outer_count.nil?
  print("Input: outer_count (number)")
  outer_count = gets.to_i
end
if inner_count.nil?
  print("Input: inner_count (number)")
  inner_count = gets.to_i
end

for i in range(outer_count):
    for j in range(inner_count):
        print(f"foo{i}{j}")
```
