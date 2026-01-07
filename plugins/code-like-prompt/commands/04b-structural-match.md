---
description: コード風プロンプト例4b 構造的パターンマッチング
argument-hint: '{"record": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- Record data type definition
data Record = Record { label :: String, value :: Int }
            deriving (Show, Eq)

-- Assume record :: Record is directly provided as Haskell data type
-- Example: Record "foo" 5

main :: IO ()
main = do
    -- record argument is provided as Record type
    let record = getRecord "record"

    -- Structural pattern matching with guards
    case record of
        Record "foo" v -> putStrLn $ "foo:" ++ show v
        Record "bar" v | v > 10 -> putStrLn "bar-large"
        Record "bar" v -> putStrLn "bar-small"
        Record l _ -> putStrLn $ "other:" ++ l
```
