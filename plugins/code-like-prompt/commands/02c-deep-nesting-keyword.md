---
description: コード風プロンプト例2c ネストされたif:複雑な5階層ネスト (キーワードスタイル)
argument-hint: '{"level1": boolean, "level2": boolean, "level3": boolean, "level4": boolean}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages. If there is nothing to output, output () instead.

```ruby
# Validate required arguments
raise "Required argument 'level1' is missing" if level1.nil?
raise "Required argument 'level2' is missing" if level2.nil?
raise "Required argument 'level3' is missing" if level3.nil?
raise "Required argument 'level4' is missing" if level4.nil?

# Logic
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
