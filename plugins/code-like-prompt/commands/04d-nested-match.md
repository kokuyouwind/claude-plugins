---
description: コード風プロンプト例4d パターンマッチ:ネストした構造のマッチング
argument-hint: "[--left=STRING] [--right-left=STRING] [--right-right=STRING]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```haskell
data Node = Node { value :: String }
data Tree = Tree { left :: Node, right :: Branch }
data Branch = Branch { branchLeft :: Node, branchRight :: Node }

left' <- case left of
  Nothing -> do
    print "Input: left"
    getLine
  Just l -> return l

rightLeft' <- case right_left of
  Nothing -> do
    print "Input: right-left"
    getLine
  Just rl -> return rl

rightRight' <- case right_right of
  Nothing -> do
    print "Input: right-right"
    getLine
  Just rr -> return rr

let tree = Tree
      { left = Node left'
      , right = Branch
          { branchLeft = Node rightLeft'
          , branchRight = Node rightRight'
          }
      }

case tree of
  Tree (Node "foo") (Branch (Node "bar") _) ->
    print "qux"
  Tree (Node v1) (Branch (Node v2) (Node v2')) | v2 == v2' ->
    print $ "quux" ++ v1
  Tree (Node v) _ ->
    print $ "corge" ++ v
  _ ->
    print "grault"
```
