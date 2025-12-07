---
description: コード風プロンプト例3c コレクション要素の反復処理
argument-hint: "[--items=ITEM1,ITEM2,ITEM3]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```ruby
if items.nil?
  print("Input: items (comma-separated)")
  items = gets.split(',')
end

items.each do |item|
  print(item)
end
```
