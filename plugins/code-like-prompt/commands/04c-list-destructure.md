---
description: コード風プロンプト例4c リスト分解パターンマッチング
argument-hint: '{"list": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- Assume list :: [String] is directly provided as Haskell data type
-- Example: ["foo", "bar", "baz"]

main :: IO ()
main = do
    -- list argument is provided as [String] type
    let list = getList "list"

    -- List pattern matching with destructuring
    case list of
        [] -> putStrLn "empty"
        [x] -> putStrLn $ "single:" ++ x
        ["foo", "bar"] -> putStrLn "foo-bar"
        ["foo", x, "baz"] -> putStrLn $ "foo-x-baz:" ++ x
        (x:y:_) -> putStrLn $ "multi:" ++ x ++ "," ++ y
        _ -> putStrLn "other"
```
