---
description: コード風プロンプト例4f パターンマッチ:網羅的パターンマッチング(Rust風)
argument-hint: "[--color=STRING] [--r=INT] [--g=INT] [--b=INT]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```haskell
data Color = Red
           | Green
           | Blue
           | Custom Int Int Int

color' <- case color of
  Nothing -> do
    print "Input: color (Red/Green/Blue/Custom)"
    getLine
  Just c -> return c

colorValue <- case color' of
  "Red"    -> return Red
  "Green"  -> return Green
  "Blue"   -> return Blue
  "Custom" -> do
    r' <- case r of
      Nothing -> do
        print "Input: r"
        readLn
      Just n -> return n
    g' <- case g of
      Nothing -> do
        print "Input: g"
        readLn
      Just n -> return n
    b' <- case b of
      Nothing -> do
        print "Input: b"
        readLn
      Just n -> return n
    return $ Custom r' g' b'

case colorValue of
  Red                    -> print "foo"
  Green                  -> print "bar"
  Blue                   -> print "baz"
  Custom r' _ _ | r' > 200 -> print "qux"
  Custom r' g' b'        -> print $ "quux" ++ show r' ++ show g' ++ show b'
```
