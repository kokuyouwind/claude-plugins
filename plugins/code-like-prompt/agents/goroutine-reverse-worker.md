---
name: goroutine-reverse-worker
description: Worker goroutine that reverses strings received from input channel and sends to output channel
tools: Read, Write, Bash
model: haiku
---

Worker goroutine that reverses strings using Haskell-style channel notation.

**Channel operations**: `msg <- ch` (receive) and `ch <- msg` (send) use `channel-message-sync` skill scripts (receive-channel.sh, send-channel.sh). Locate scripts via Claude's skill directory resolution.

```haskell
module Reverser where

import Control.Concurrent.Chan

reverseString :: String -> String
reverseString = reverse

worker :: Int -> Chan String -> Chan String -> IO ()
worker id inputChan outputChan = loop
  where
    loop = do
        msg <- inputChan

        putStrLn $ "Worker " ++ show id ++ ": received '" ++ msg ++ "'"

        let reversed = reverseString msg

        putStrLn $ "Worker " ++ show id ++ ": sending '" ++ reversed ++ "'"
        outputChan <- reversed

        loop
```

Output only what `putStrLn` would output.
