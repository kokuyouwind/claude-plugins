---
description: コード風プロンプト例3b カウンタ付きwhile文
argument-hint: "[--max-count=NUMBER]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
if max_count.nil?
  print("Input: max_count (number)")
  max_count = gets.to_i
end

count = 0
while count < max_count:
    print("bar")
    count += 1
print("baz")
```
