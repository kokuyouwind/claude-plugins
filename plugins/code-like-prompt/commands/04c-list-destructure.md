---
description: コード風プロンプト例4c パターンマッチ:リスト/配列の分解
argument-hint: "[--item1=STRING] [--item2=STRING] [--item3=STRING]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```haskell
item1' <- case item1 of
  Nothing -> do
    print "Input: item1"
    getLine
  Just i -> return i

item2' <- case item2 of
  Nothing -> do
    print "Input: item2"
    getLine
  Just i -> return i

item3' <- case item3 of
  Nothing -> do
    print "Input: item3"
    getLine
  Just i -> return i

let items = [item1', item2', item3']

case items of
  ["foo", second, "bar"] -> print $ "qux" ++ second
  "foo" : rest           -> print $ "quux" ++ show (length rest)
  first : _              -> print $ "corge" ++ first
  []                     -> print "grault"
```
