---
description: コード風プロンプト例4g Maybeのパターンマッチング
argument-hint: '{"has_value": boolean, "value": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
main :: IO ()
main = do
    -- Validate required arguments
    let hasValue = case lookupArg "has_value" of
            Nothing -> error "Required argument 'has_value' is missing"
            Just v -> read v :: Bool

    -- Build Maybe value
    let maybeValue = if hasValue
        then case lookupArg "value" of
            Nothing -> error "Required argument 'value' is missing when has_value is true"
            Just v -> Just v
        else Nothing

    -- Pattern matching on Maybe
    case maybeValue of
        Just "foo" -> putStrLn "bar"
        Just "baz" -> putStrLn "qux"
        Just v -> putStrLn $ "quux" ++ v
        Nothing -> putStrLn "corge"
```
