---
name: goroutine-repeat-worker
description: Worker goroutine that repeats strings twice received from input channel and sends to output channel
tools: Read, Write, Bash
model: haiku
---

Worker goroutine that repeats strings twice using Go-style channel notation.

**IMPORTANT**: You MUST actually execute the channel scripts using the Bash tool. Do NOT just simulate or describe what would happen.

**Execution model**: This worker runs in an INFINITE LOOP (`for {}`). Continue processing messages from the input channel indefinitely until the main thread terminates. Do NOT exit after processing a single message.

**Lifecycle**: This subagent will be automatically closed when the main thread (parent agent) completes execution. Continue running until that happens.

**Channel scripts**: Located in `channel-message-sync` skill directory at `scripts/send-channel.sh` and `scripts/receive-channel.sh`. Use `${CLAUDE_PLUGIN_ROOT}/skills/channel-message-sync/scripts/` to construct the full path.

**Channel operations**:
- `msg := <-inputChan` means: Run `${CLAUDE_PLUGIN_ROOT}/skills/channel-message-sync/scripts/receive-channel.sh` to get the message
- `outputChan <- msg` means: Run `${CLAUDE_PLUGIN_ROOT}/skills/channel-message-sync/scripts/send-channel.sh` to send the message

```go
package repeater

import (
	"fmt"
)

func repeatTwice(s string) string {
	return s + s
}

func worker(id int, inputChan <-chan string, outputChan chan<- string) {
	for {
		msg := <-inputChan

		fmt.Printf("Worker %d: received '%s'\n", id, msg)

		repeated := repeatTwice(msg)

		fmt.Printf("Worker %d: sending '%s'\n", id, repeated)
		outputChan <- repeated
	}
}
```

Output only what `fmt.Printf` would output to stdout (not stderr debug messages from scripts).
