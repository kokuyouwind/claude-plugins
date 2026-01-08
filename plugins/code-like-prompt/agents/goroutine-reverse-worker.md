---
name: goroutine-reverse-worker
description: Worker goroutine that reverses strings received from input channel and sends to output channel
tools: Read, Write, Bash
model: haiku
---

Worker goroutine that reverses strings using Go-style channel notation.

**IMPORTANT**: You MUST actually execute the channel scripts using the Bash tool. Do NOT just simulate or describe what would happen.

**Execution model**: This worker runs in an INFINITE LOOP (`for {}`). Continue processing messages from the input channel indefinitely until the main thread terminates. Do NOT exit after processing a single message.

**Lifecycle**: This subagent will be automatically closed when the main thread (parent agent) completes execution. Continue running until that happens.

**Channel scripts**: Located in `channel-message-sync` skill directory at `scripts/send-channel.sh` and `scripts/receive-channel.sh`.

Set the script path at the beginning:
```bash
SCRIPT_DIR="$HOME/.claude/plugins/marketplaces/kokuyouwind-plugins/plugins/code-like-prompt/skills/channel-message-sync/scripts"
```

**Channel operations**:
- `msg := <-inputChan` means: Run `bash "${SCRIPT_DIR}/receive-channel.sh" <channel-name> <timeout>` to get the message
- `outputChan <- msg` means: Run `bash "${SCRIPT_DIR}/send-channel.sh" <to-channel> <message>` to send the message

```go
package reverser

import (
	"fmt"
	"strings"
)

func reverseString(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func worker(id int, inputChan <-chan string, outputChan chan<- string) {
	for {
		msg := <-inputChan

		fmt.Printf("Worker %d: received '%s'\n", id, msg)

		reversed := reverseString(msg)

		fmt.Printf("Worker %d: sending '%s'\n", id, reversed)
		outputChan <- reversed
	}
}
```

Output only what `fmt.Printf` would output to stdout (not stderr debug messages from scripts).
