---
description: コード風プロンプト例2c ネストされたif:複雑な5階層ネスト (キーワードスタイル)
argument-hint: "[--level1=BOOL] [--level2=BOOL] [--level3=BOOL] [--level4=BOOL]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if level1.nil?
  print("Input: level1 (true/false)")
  level1 = gets
end
if level2.nil?
  print("Input: level2 (true/false)")
  level2 = gets
end
if level3.nil?
  print("Input: level3 (true/false)")
  level3 = gets
end
if level4.nil?
  print("Input: level4 (true/false)")
  level4 = gets
end

if level1
  if level2
    if level3
      print "foo"
    else
      if level4
        print "bar"
      else
        print "baz"
      end
    end
  else
    if level3
      if level4
        print "qux"
      end
    else
      print "quux"
    end
  end
else
  if level2
    print "corge"
  else
    if level3
      print "grault"
    else
      print "garply"
    end
  end
end
```
