---
description: コード風プロンプト例3e continue文でイテレーションをスキップ
argument-hint: "[--skip-at=NUMBER]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
if skip_at.nil?
  print("Input: skip_at (number)")
  skip_at = gets.to_i
end

for i in range(5):
    if i == skip_at:
        continue
    print(f"foo{i}")
```
