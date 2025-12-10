---
description: コード風プロンプト例4e パターンマッチ:複数のガード条件
argument-hint: "[--x=INT] [--y=INT]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```haskell
x' <- case x of
  Nothing -> do
    print "Input: x"
    readLn
  Just n -> return n

y' <- case y of
  Nothing -> do
    print "Input: y"
    readLn
  Just n -> return n

case (x', y') of
  (a, b) | a > 0 && b > 0 -> print "foo"
         | a > 0 || b > 0 -> print "bar"
  (0, 0)                  -> print "baz"
  _                       -> print "qux"
```
