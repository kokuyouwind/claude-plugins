---
description: コード風プロンプト例4a 正規表現マッチング
argument-hint: '{"text": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
import Text.Regex.Posix ((=~))

main :: IO ()
main = do
    -- Validate required arguments
    let text = case lookupArg "text" of
            Nothing -> error "Required argument 'text' is missing"
            Just t -> t

    -- Pattern matching with guards
    case text of
        _ | text =~ "^foo.*bar$" -> putStrLn "qux"
          | text =~ "^baz" -> putStrLn "quux"
          | otherwise -> putStrLn "corge"
```
