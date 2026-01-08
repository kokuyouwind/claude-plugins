---
description: コード風プロンプト例5m Goのgoroutine:複数goroutine間のチャンネル通信パイプライン(ファイル経由)
argument-hint: "[MESSAGE1] [MESSAGE2]"
---

Emulate the following Go-style code. Output only what `fmt.Println` commands would output. Do not show explanations, code, variables, or other messages.

**Subagents**: Spawn two workers using Task tool:
- "code-like-prompt:goroutine-reverse-worker" to execute `reverser.worker(1, ch1, ch2)`
- "code-like-prompt:goroutine-repeat-worker" to execute `repeater.worker(2, ch2, ch3)`

**Channel operations**:
- `ch <- msg` (send) uses `${CLAUDE_PLUGIN_ROOT}/skills/channel-message-sync/scripts/send-channel.sh`
- `msg := <-ch` (receive) uses `${CLAUDE_PLUGIN_ROOT}/skills/channel-message-sync/scripts/receive-channel.sh`

**Setup/Cleanup**: Before execution, run `rm -rf /tmp/go-channels` to clean state. After execution, run `rm -rf /tmp/go-channels` to cleanup.

```go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:]
	msg1 := "ab"
	msg2 := "xyz"

	if len(args) > 0 {
		msg1 = args[0]
	}
	if len(args) > 1 {
		msg2 = args[1]
	}

	ch1 := make(chan string)
	ch2 := make(chan string)
	ch3 := make(chan string)

	go reverser.worker(1, ch1, ch2)
	go repeater.worker(2, ch2, ch3)

	ch1 <- msg1
	ch1 <- msg2

	result1 := <-ch3
	fmt.Println(result1)

	result2 := <-ch3
	fmt.Println(result2)
}
```
