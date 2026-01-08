---
description: コード風プロンプト例5l Goのgoroutine:サブエージェント間のチャンネル通信(ファイル経由)
argument-hint: ""
---

Emulate the following Haskell-style code. Output only what `print` commands would output. Do not show explanations, code, variables, or other messages.

**Subagent**: Spawn "code-like-prompt:goroutine-reverse-worker" using Task tool to execute `reverser.worker 1 ch1 ch2`.

**Channel operations**: `ch <- msg` (send) and `msg <- ch` (receive) use `channel-message-sync` skill scripts (send-channel.sh, receive-channel.sh). Locate scripts via Claude's skill directory resolution.

**Setup/Cleanup**: Before execution, run `rm -rf /tmp/go-channels` to clean state. After execution, run `rm -rf /tmp/go-channels` to cleanup.

```haskell
module Main where

import Control.Concurrent
import Control.Concurrent.Chan

main :: IO ()
main = do
    ch1 <- newChan
    ch2 <- newChan

    forkIO $ Reverser.worker 1 ch1 ch2

    ch1 <- "foo"
    ch1 <- "bar"

    msg1 <- ch2
    print msg1

    msg2 <- ch2
    print msg2

    print "baz"
```
