---
description: コード風プロンプト例4d ネストした構造のパターンマッチング
argument-hint: '{"tree": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- Tree data type definition
data Tree = Leaf String
          | Branch Tree Tree
          deriving (Show, Eq)

-- Assume tree :: Tree is directly provided as Haskell data type
-- Example: Branch (Leaf "foo") (Leaf "bar")

main :: IO ()
main = do
    -- tree argument is provided as Tree type
    let tree = getTree "tree"

    -- Pattern matching on nested tree structure
    case tree of
        Leaf x -> putStrLn $ "leaf:" ++ x
        Branch (Leaf "foo") (Leaf "bar") -> putStrLn "foo-bar"
        Branch (Leaf "foo") _ -> putStrLn "foo-any"
        Branch _ (Leaf "bar") -> putStrLn "any-bar"
        Branch (Branch _ _) _ -> putStrLn "nested"
        Branch _ _ -> putStrLn "other"
```
