---
description: コード風プロンプト例4d パターンマッチ:ネストした構造のマッチング
argument-hint: "[--left=STRING] [--right-left=STRING] [--right-right=STRING]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if left.nil?
  print("Input: left")
  left = gets
end
if right_left.nil?
  print("Input: right-left")
  right_left = gets
end
if right_right.nil?
  print("Input: right-right")
  right_right = gets
end

tree = {
    "left": {"value": left},
    "right": {
        "left": {"value": right_left},
        "right": {"value": right_right}
    }
}

match tree:
    case {"left": {"value": "foo"}, "right": {"left": {"value": "bar"}, "right": _}}:
        print("qux")
    case {"left": {"value": v1}, "right": {"left": {"value": v2}, "right": {"value": v2}}}:
        print(f"quux{v1}")
    case {"left": {"value": v}, "right": _}:
        print(f"corge{v}")
    case _:
        print("grault")
```
