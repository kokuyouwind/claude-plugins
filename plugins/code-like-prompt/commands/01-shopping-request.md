---
description: コード風プロンプト例1 牛乳ジョーク:妻の依頼
argument-hint: '{"Milk.stock": number, "Egg.stock": number}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what puts() commands would output. Do not show any explanations, code, variables, or other messages.

```ruby
# Validate required arguments
raise "Required argument 'Milk.stock' is missing" if Milk.stock.nil?
raise "Required argument 'Egg.stock' is missing" if Egg.stock.nil?

# Shopping logic
puts("Bought 1 milk.")
if Egg.exists?  # Egg.exists? means Egg.stock > 0
  puts("Bought 6 eggs.")
end
```
