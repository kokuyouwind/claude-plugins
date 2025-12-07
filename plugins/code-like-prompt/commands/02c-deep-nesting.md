---
description: コード風プロンプト例2c ネストされたif:複雑な5階層ネスト
argument-hint: "[--level1=BOOL] [--level2=BOOL] [--level3=BOOL] [--level4=BOOL]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if Level1.value.nil?
  print("Input: Level1 (true/false)")
  Level1.value = gets
end
if Level2.value.nil?
  print("Input: Level2 (true/false)")
  Level2.value = gets
end
if Level3.value.nil?
  print("Input: Level3 (true/false)")
  Level3.value = gets
end
if Level4.value.nil?
  print("Input: Level4 (true/false)")
  Level4.value = gets
end

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
