---
description: コード風プロンプト例5m Goのgoroutine:複数goroutine間のチャンネル通信パイプライン(ファイル経由)
argument-hint: "[MESSAGE1] [MESSAGE2]"
---

Emulate the following Haskell-style code. Output only what `print` commands would output. Do not show explanations, code, variables, or other messages.

**Subagents**: Spawn two workers using Task tool:
- "code-like-prompt:goroutine-reverse-worker" to execute `reverser.worker 1 ch1 ch2`
- "code-like-prompt:goroutine-repeat-worker" to execute `repeater.worker 2 ch2 ch3`

**Channel operations**: `ch <- msg` (send) and `msg <- ch` (receive) use `channel-message-sync` skill scripts (send-channel.sh, receive-channel.sh). Locate scripts via Claude's skill directory resolution.

**Setup/Cleanup**: Before execution, run `rm -rf /tmp/go-channels` to clean state. After execution, run `rm -rf /tmp/go-channels` to cleanup.

```haskell
module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let msg1 = if length args > 0 then args !! 0 else "ab"
    let msg2 = if length args > 1 then args !! 1 else "xyz"

    ch1 <- newChan
    ch2 <- newChan
    ch3 <- newChan

    forkIO $ Reverser.worker 1 ch1 ch2
    forkIO $ Repeater.worker 2 ch2 ch3

    ch1 <- msg1
    ch1 <- msg2

    result1 <- ch3
    print result1

    result2 <- ch3
    print result2
```
