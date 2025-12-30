---
description: コード風プロンプト例2a ネストされたif:elseが外側に属する (キーワードスタイル)
argument-hint: '{"condition_a": boolean, "condition_b": boolean}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages.

```ruby
# Validate required arguments
raise "Required argument 'condition_a' is missing" if condition_a.nil?
raise "Required argument 'condition_b' is missing" if condition_b.nil?

# Logic
if condition_a
  if condition_b
    print "foo"
  end
else
  print "bar"
end
```
