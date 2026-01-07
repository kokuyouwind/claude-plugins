---
description: コード風プロンプト例4b 構造的パターンマッチング
argument-hint: '{"type": string, "value": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- Data type definition
data Data = Data { dataType :: String, dataValue :: String }

main :: IO ()
main = do
    -- Validate required arguments
    let typ = case lookupArg "type" of
            Nothing -> error "Required argument 'type' is missing"
            Just t -> t
    let val = case lookupArg "value" of
            Nothing -> error "Required argument 'value' is missing"
            Just v -> v

    -- Structural pattern matching
    let record = Data typ val
    case record of
        Data "foo" v -> putStrLn $ "bar" ++ v
        Data "baz" v | read v > 10 -> putStrLn $ "qux" ++ v
        Data "baz" v -> putStrLn $ "quux" ++ v
        _ -> putStrLn "corge"
```
