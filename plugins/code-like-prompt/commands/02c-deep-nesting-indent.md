---
description: コード風プロンプト例2c ネストされたif:複雑な5階層ネスト (インデントベース)
argument-hint: '{"level1": boolean, "level2": boolean, "level3": boolean, "level4": boolean}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if level1 is None:
    raise ValueError("Required argument 'level1' is missing")
if level2 is None:
    raise ValueError("Required argument 'level2' is missing")
if level3 is None:
    raise ValueError("Required argument 'level3' is missing")
if level4 is None:
    raise ValueError("Required argument 'level4' is missing")

# Logic
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
