---
description: コード風プロンプト例4b パターンマッチ:構造的パターンマッチングとガード
argument-hint: "[--type=STRING] [--value=STRING]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if type.nil?
  print("Input: type")
  type = gets
end
if value.nil?
  print("Input: value")
  value = gets
end

data = {"type": type, "value": value}

match data:
    case {"type": "foo", "value": v}:
        print(f"bar{v}")
    case {"type": "baz", "value": v} if int(v) > 10:
        print(f"qux{v}")
    case {"type": "baz", "value": v}:
        print(f"quux{v}")
    case _:
        print("corge")
```
