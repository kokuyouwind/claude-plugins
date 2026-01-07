---
description: コード風プロンプト例4f 網羅的なenumマッチング
argument-hint: '{"color": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- Color data type definition
data Color = Red
           | Green
           | Blue
           | RGB Int Int Int
           deriving (Show, Eq)

-- Assume color :: Color is directly provided as Haskell data type
-- Example: Red, Green, Blue, or RGB 255 100 50

main :: IO ()
main = do
    -- color argument is provided as Color type
    let color = getColor "color"

    -- Exhaustive pattern matching
    case color of
        Red -> putStrLn "red"
        Green -> putStrLn "green"
        Blue -> putStrLn "blue"
        RGB r _ _ | r > 200 -> putStrLn "bright"
        RGB r g b -> putStrLn $ "rgb:" ++ show r ++ "," ++ show g ++ "," ++ show b
```
