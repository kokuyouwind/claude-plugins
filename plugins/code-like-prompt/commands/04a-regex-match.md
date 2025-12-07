---
description: コード風プロンプト例4a パターンマッチ:基本的な文字列パターンマッチング
argument-hint: "[--text=STRING]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
import re

if text.nil?
  print("Input: text")
  text = gets
end

if re.match(r"^foo.*bar$", text):
    print("qux")
elif re.match(r"^baz", text):
    print("quux")
else:
    print("corge")
```
