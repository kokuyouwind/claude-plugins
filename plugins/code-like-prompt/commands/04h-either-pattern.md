---
description: コード風プロンプト例4h Eitherのパターンマッチングとdo記法
argument-hint: '{"either_value": string, "style": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- Assume eitherValue :: Either String String is directly provided

-- Process with pattern matching
processPattern :: Either String String -> IO ()
processPattern eitherVal =
    case eitherVal of
        Left err -> putStrLn $ "error:" ++ err
        Right val -> putStrLn $ "success:" ++ val

-- Process with do notation (Either monad)
processDo :: Either String String -> IO ()
processDo eitherVal = do
    let result = do
        x <- eitherVal                -- Extract Right or propagate Left
        return $ "processed:" ++ x    -- Transform and wrap in Right
    case result of
        Left err -> putStrLn $ "failed:" ++ err
        Right r -> putStrLn r

main :: IO ()
main = do
    -- Assume eitherValue is provided as Either String String
    -- "Left:error" → Left "error", "Right:value" → Right "value"
    let eitherValue = case lookupArg "either_value" of
            Just s | take 5 s == "Left:" -> Left (drop 5 s)
                   | take 6 s == "Right:" -> Right (drop 6 s)
            _ -> error "Invalid either_value format"

    let style = case lookupArg "style" of
            Nothing -> "pattern"
            Just s -> s

    case style of
        "pattern" -> processPattern eitherValue
        "do" -> processDo eitherValue
        _ -> error "Invalid style"
```
