---
description: コード風プロンプト例4c パターンマッチ:リスト/配列の分解
argument-hint: "[--item1=STRING] [--item2=STRING] [--item3=STRING]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if item1.nil?
  print("Input: item1")
  item1 = gets
end
if item2.nil?
  print("Input: item2")
  item2 = gets
end
if item3.nil?
  print("Input: item3")
  item3 = gets
end

items = [item1, item2, item3]

match items:
    case ["foo", second, "bar"]:
        print(f"qux{second}")
    case ["foo", *rest]:
        print(f"quux{len(rest)}")
    case [first, *_]:
        print(f"corge{first}")
    case []:
        print("grault")
```
