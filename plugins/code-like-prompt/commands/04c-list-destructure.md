---
description: コード風プロンプト例4c リスト分解パターンマッチング
argument-hint: '{"item1": string, "item2": string, "item3": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
main :: IO ()
main = do
    -- Validate required arguments
    let item1 = case lookupArg "item1" of
            Nothing -> error "Required argument 'item1' is missing"
            Just i -> i
    let item2 = case lookupArg "item2" of
            Nothing -> error "Required argument 'item2' is missing"
            Just i -> i
    let item3 = case lookupArg "item3" of
            Nothing -> error "Required argument 'item3' is missing"
            Just i -> i

    -- List destructuring
    let items = [item1, item2, item3]

    case items of
        ["foo", second, "bar"] -> putStrLn $ "qux" ++ second
        ("foo":rest) -> putStrLn $ "quux" ++ show (length rest)
        (first:_) -> putStrLn $ "corge" ++ first
        [] -> putStrLn "grault"
```
