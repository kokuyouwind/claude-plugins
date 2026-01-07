---
description: コード風プロンプト例4g Maybeのパターンマッチングとdo記法
argument-hint: '{"maybe_value": string, "style": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- Assume maybeValue :: Maybe String is directly provided

-- Process with pattern matching
processPattern :: Maybe String -> IO ()
processPattern maybeVal =
    case maybeVal of
        Nothing -> putStrLn "none"
        Just x -> putStrLn $ "value:" ++ x

-- Process with do notation
processDo :: Maybe String -> IO ()
processDo maybeVal = do
    let result = do
        x <- maybeVal          -- Extract from Maybe
        return $ "processed:" ++ x
    case result of
        Nothing -> putStrLn "empty"
        Just r -> putStrLn r

main :: IO ()
main = do
    -- Assume maybeValue is provided as Maybe String
    -- "Nothing" → Nothing, "Just:foo" → Just "foo"
    let maybeValue = case lookupArg "maybe_value" of
            Just "Nothing" -> Nothing
            Just s | take 5 s == "Just:" -> Just (drop 5 s)
            _ -> error "Invalid maybe_value format"

    let style = case lookupArg "style" of
            Nothing -> "pattern"
            Just s -> s

    case style of
        "pattern" -> processPattern maybeValue
        "do" -> processDo maybeValue
        _ -> error "Invalid style"
```
