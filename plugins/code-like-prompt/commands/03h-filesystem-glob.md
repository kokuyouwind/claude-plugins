---
description: コード風プロンプト例3h ファイルシステムのglob検索
argument-hint: '{"pattern": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what puts() commands would output. Do not show any explanations, code, variables, or other messages.

```ruby
# Validate required arguments
raise "Required argument 'pattern' is missing" if pattern.nil?

# File system iteration
Dir.glob(pattern).each do |file|
  puts file
end
```
