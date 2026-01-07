---
description: コード風プロンプト例4e 複数ガード条件のパターンマッチング
argument-hint: '{"x": number, "y": number}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
main :: IO ()
main = do
    -- Validate required arguments
    let x = case lookupArg "x" of
            Nothing -> error "Required argument 'x' is missing"
            Just v -> read v :: Int
    let y = case lookupArg "y" of
            Nothing -> error "Required argument 'y' is missing"
            Just v -> read v :: Int

    -- Pattern matching with guards
    case (x, y) of
        (a, b) | a > 0 && b > 0 -> putStrLn "foo"
               | a > 0 || b > 0 -> putStrLn "bar"
        (0, 0) -> putStrLn "baz"
        _ -> putStrLn "qux"
```
