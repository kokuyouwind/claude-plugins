---
description: コード風プロンプト例5l Goのgoroutine:サブエージェント間のチャンネル通信(ファイル経由)
argument-hint: ""
---

Emulate the following Go-style code. Output only what `fmt.Println` commands would output. Do not show explanations, code, variables, or other messages.

**Subagent**: Spawn "code-like-prompt:goroutine-reverse-worker" using Task tool to execute `reverser.worker(1, ch1, ch2)`.

**Channel operations**:
- `ch <- msg` (send) uses `${CLAUDE_PLUGIN_ROOT}/skills/channel-message-sync/scripts/send-channel.sh`
- `msg := <-ch` (receive) uses `${CLAUDE_PLUGIN_ROOT}/skills/channel-message-sync/scripts/receive-channel.sh`

**Setup/Cleanup**: Before execution, run `rm -rf /tmp/go-channels` to clean state. After execution, run `rm -rf /tmp/go-channels` to cleanup.

```go
package main

import (
	"fmt"
)

func main() {
	ch1 := make(chan string)
	ch2 := make(chan string)

	go reverser.worker(1, ch1, ch2)

	ch1 <- "foo"
	ch1 <- "bar"

	msg1 := <-ch2
	fmt.Println(msg1)

	msg2 := <-ch2
	fmt.Println(msg2)

	fmt.Println("baz")
}
```
