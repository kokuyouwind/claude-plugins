---
description: コード風プロンプト例1 牛乳ジョーク:妻の依頼
argument-hint: "[--milk-stock=N] [--egg-stock=N]"
---

Execute the following code. Output only what puts() commands specify. Do not show any explanations, code, variables, or other messages.

```
if Milk.stock.nil?
  puts("Input: Milk")
  Milk.stock = gets
end
if Egg.stock.nil?
  puts("Input: Egg")
  Egg.stock = gets
end

puts("Bought 1 milk.")
if Egg.exists?
  puts("Bought 6 eggs.")
end
```
