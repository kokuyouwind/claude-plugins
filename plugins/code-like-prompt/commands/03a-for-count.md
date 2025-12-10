---
description: コード風プロンプト例3a 基本的なfor文
argument-hint: "[--count=NUMBER]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
if count.nil?
  print("Input: count (number)")
  count = gets.to_i
end

for i in range(count):
    print(f"foo{i}")
```
