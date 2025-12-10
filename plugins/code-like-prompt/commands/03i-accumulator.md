---
description: コード風プロンプト例3i アキュムレータパターン
argument-hint: "[--start=NUMBER] [--end=NUMBER]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
if start.nil?
  print("Input: start (number)")
  start = gets.to_i
end
if end.nil?
  print("Input: end (number)")
  end = gets.to_i
end

total = 0
for i in range(start, end):
    total += i
    print(f"foo{total}")
```
