---
description: コード風プロンプト例2c ネストされたif:複雑な5階層ネスト (インデントベース)
argument-hint: "[--level1=BOOL] [--level2=BOOL] [--level3=BOOL] [--level4=BOOL]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if level1 is None:
    print("Input: level1 (true/false)")
    level1 = input()
if level2 is None:
    print("Input: level2 (true/false)")
    level2 = input()
if level3 is None:
    print("Input: level3 (true/false)")
    level3 = input()
if level4 is None:
    print("Input: level4 (true/false)")
    level4 = input()

if level1:
    if level2:
        if level3:
            print("foo")
        else:
            if level4:
                print("bar")
            else:
                print("baz")
    else:
        if level3:
            if level4:
                print("qux")
        else:
            print("quux")
else:
    if level2:
        print("corge")
    else:
        if level3:
            print("grault")
        else:
            print("garply")
```
