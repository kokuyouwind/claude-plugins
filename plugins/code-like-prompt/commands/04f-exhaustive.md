---
description: コード風プロンプト例4f 網羅的なenumマッチング
argument-hint: '{"color": string, "r": number, "g": number, "b": number}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- Color data type definition
data Color = Red
           | Green
           | Blue
           | Custom Int Int Int
           deriving (Show, Eq)

main :: IO ()
main = do
    -- Validate required arguments
    let colorStr = case lookupArg "color" of
            Nothing -> error "Required argument 'color' is missing"
            Just c -> c

    -- Parse color
    let c = case colorStr of
            "Red" -> Red
            "Green" -> Green
            "Blue" -> Blue
            _ -> let r = case lookupArg "r" of
                        Nothing -> error "Required arguments 'r', 'g', 'b' are missing for Custom color"
                        Just v -> read v :: Int
                     g = case lookupArg "g" of
                        Nothing -> error "Required arguments 'r', 'g', 'b' are missing for Custom color"
                        Just v -> read v :: Int
                     b = case lookupArg "b" of
                        Nothing -> error "Required arguments 'r', 'g', 'b' are missing for Custom color"
                        Just v -> read v :: Int
                 in Custom r g b

    -- Exhaustive matching
    case c of
        Red -> putStrLn "foo"
        Green -> putStrLn "bar"
        Blue -> putStrLn "baz"
        Custom r _ _ | r > 200 -> putStrLn "qux"
        Custom r g b -> putStrLn $ "quux" ++ show r ++ show g ++ show b
```
