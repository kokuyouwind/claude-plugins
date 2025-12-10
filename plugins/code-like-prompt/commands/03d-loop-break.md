---
description: コード風プロンプト例3d break文で途中終了するループ
argument-hint: "[--break-at=NUMBER]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
if break_at.nil?
  print("Input: break_at (number)")
  break_at = gets.to_i
end

for i in range(10):
    if i == break_at:
        break
    print(f"foo{i}")
print("bar")
```
