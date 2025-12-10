---
description: コード風プロンプト例4a パターンマッチ:基本的な文字列パターンマッチング
argument-hint: "[--text=STRING]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```haskell
import Text.Regex.Posix

text <- case text of
  Nothing -> do
    print "Input: text"
    getLine
  Just t -> return t

case () of
  _ | text =~ "^foo.*bar$" -> print "qux"
    | text =~ "^baz"       -> print "quux"
    | otherwise            -> print "corge"
```
