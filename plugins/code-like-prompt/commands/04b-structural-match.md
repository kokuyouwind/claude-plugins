---
description: コード風プロンプト例4b パターンマッチ:構造的パターンマッチングとガード
argument-hint: "[--type=STRING] [--value=STRING]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```haskell
data Data = Data { dataType :: String, dataValue :: String }

type' <- case type of
  Nothing -> do
    print "Input: type"
    getLine
  Just t -> return t

value' <- case value of
  Nothing -> do
    print "Input: value"
    getLine
  Just v -> return v

let data' = Data type' value'

case data' of
  Data "foo" v -> print $ "bar" ++ v
  Data "baz" v | read v > 10 -> print $ "qux" ++ v
  Data "baz" v               -> print $ "quux" ++ v
  _                          -> print "corge"
```
