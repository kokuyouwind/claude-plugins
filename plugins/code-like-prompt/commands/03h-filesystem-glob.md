---
description: コード風プロンプト例3h ファイルシステムのglob反復処理
argument-hint: "[--pattern=GLOB_PATTERN]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```ruby
if pattern.nil?
  print("Input: pattern (glob pattern)")
  pattern = gets
end

Dir.glob(pattern).each do |file|
  print(file)
end
```
