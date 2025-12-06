---
description: コード風プロンプト例1 牛乳ジョーク:夫の理解
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

milk_amount = 1
if Egg.exists?
  milk_amount = 6
end
puts("Bought #{milk_amount} milks.")
```
