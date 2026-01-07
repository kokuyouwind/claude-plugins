---
description: コード風プロンプト例4d ネストした構造のパターンマッチング
argument-hint: '{"left": string, "right_left": string, "right_right": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- Tree data type definition
data Node = Node { value :: String }
data Tree = Tree { left :: Node, right :: SubTree }
data SubTree = SubTree { subLeft :: Node, subRight :: Node }

main :: IO ()
main = do
    -- Validate required arguments
    let leftVal = case lookupArg "left" of
            Nothing -> error "Required argument 'left' is missing"
            Just v -> v
    let rightLeftVal = case lookupArg "right_left" of
            Nothing -> error "Required argument 'right_left' is missing"
            Just v -> v
    let rightRightVal = case lookupArg "right_right" of
            Nothing -> error "Required argument 'right_right' is missing"
            Just v -> v

    -- Nested structure matching
    let tree = Tree
            (Node leftVal)
            (SubTree (Node rightLeftVal) (Node rightRightVal))

    case tree of
        Tree (Node "foo") (SubTree (Node "bar") _) ->
            putStrLn "qux"
        Tree (Node v1) (SubTree (Node v2) (Node v2')) | v2 == v2' ->
            putStrLn $ "quux" ++ v1
        Tree (Node v) _ ->
            putStrLn $ "corge" ++ v
        _ ->
            putStrLn "grault"
```
