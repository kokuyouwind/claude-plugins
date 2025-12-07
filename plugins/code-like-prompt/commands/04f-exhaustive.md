---
description: コード風プロンプト例4f パターンマッチ:網羅的パターンマッチング(Rust風)
argument-hint: "[--color=STRING] [--r=INT] [--g=INT] [--b=INT]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if color.nil?
  print("Input: color (Red/Green/Blue/Custom)")
  color = gets
end

if color == "Custom":
    if r.nil?
      print("Input: r")
      r = gets.to_i
    end
    if g.nil?
      print("Input: g")
      g = gets.to_i
    end
    if b.nil?
      print("Input: b")
      b = gets.to_i
    end

match color:
    case "Red":
        print("foo")
    case "Green":
        print("bar")
    case "Blue":
        print("baz")
    case "Custom" if r > 200:
        print("qux")
    case "Custom":
        print(f"quux{r}{g}{b}")
```
