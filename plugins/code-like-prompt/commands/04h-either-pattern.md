---
description: コード風プロンプト例4h Eitherのパターンマッチング
argument-hint: '{"is_error": boolean, "message": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
main :: IO ()
main = do
    -- Validate required arguments
    let isError = case lookupArg "is_error" of
            Nothing -> error "Required argument 'is_error' is missing"
            Just v -> read v :: Bool
    let message = case lookupArg "message" of
            Nothing -> error "Required argument 'message' is missing"
            Just v -> v

    -- Build Either value
    let result = if isError
        then Left message :: Either String String
        else Right message :: Either String String

    -- Pattern matching on Either
    case result of
        Left "timeout" -> putStrLn "foo"
        Left err -> putStrLn $ "bar" ++ err
        Right "success" -> putStrLn "baz"
        Right msg -> putStrLn $ "qux" ++ msg
```
