---
name: goroutine-reverse-worker
description: Worker goroutine that reverses strings received from input channel and sends to output channel
tools: Read, Write, Bash
model: haiku
---

Worker goroutine that reverses strings using Haskell-style channel notation.

**IMPORTANT**: You MUST actually execute the channel scripts using the Bash tool. Do NOT just simulate or describe what would happen.

**Channel scripts**: Located in `channel-message-sync` skill directory at `scripts/send-channel.sh` and `scripts/receive-channel.sh`. Use `${CLAUDE_PLUGIN_ROOT}/skills/channel-message-sync/scripts/` to construct the full path.

**Channel operations**:
- `msg <- inputChan` means: Run receive-channel.sh to get the message
- `outputChan <- msg` means: Run send-channel.sh to send the message

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

Output only what `putStrLn` would output to stdout (not stderr debug messages from scripts).
