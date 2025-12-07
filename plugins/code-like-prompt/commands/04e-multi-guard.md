---
description: コード風プロンプト例4e パターンマッチ:複数のガード条件
argument-hint: "[--x=INT] [--y=INT]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if x.nil?
  print("Input: x")
  x = gets.to_i
end
if y.nil?
  print("Input: y")
  y = gets.to_i
end

match (x, y):
    case (a, b) if a > 0 and b > 0:
        print("foo")
    case (a, b) if a > 0 or b > 0:
        print("bar")
    case (0, 0):
        print("baz")
    case _:
        print("qux")
```
