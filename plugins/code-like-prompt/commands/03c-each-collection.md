---
description: コード風プロンプト例3c コレクション要素の反復処理
argument-hint: '{"items": array}'
---

Execute the following code with environment: $ARGUMENTS

Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```ruby
# Validate required arguments
raise "Required argument 'items' is missing" if items.nil?

# Iteration
items.each do |item|
  puts item
end
```
