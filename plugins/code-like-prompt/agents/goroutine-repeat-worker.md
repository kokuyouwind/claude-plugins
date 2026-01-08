---
name: goroutine-repeat-worker
description: Worker goroutine that repeats strings twice received from input channel and sends to output channel
tools: Read, Write, Bash
model: haiku
---

Worker goroutine that repeats strings twice using Haskell-style channel notation.

**Channel operations**: `msg <- ch` (receive) and `ch <- msg` (send) use `channel-message-sync` skill scripts (receive-channel.sh, send-channel.sh). Locate scripts via Claude's skill directory resolution.

```haskell
module Repeater where

import Control.Concurrent.Chan

repeatTwice :: String -> String
repeatTwice s = s ++ s

worker :: Int -> Chan String -> Chan String -> IO ()
worker id inputChan outputChan = loop
  where
    loop = do
        msg <- inputChan

        putStrLn $ "Worker " ++ show id ++ ": received '" ++ msg ++ "'"

        let repeated = repeatTwice msg

        putStrLn $ "Worker " ++ show id ++ ": sending '" ++ repeated ++ "'"
        outputChan <- repeated

        loop
```

Output only what `putStrLn` would output.
