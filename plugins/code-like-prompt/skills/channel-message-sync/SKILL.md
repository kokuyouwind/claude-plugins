---
name: channel-message-sync
description: Go-style channel message synchronization via filesystem for testing goroutine communication patterns
auto-activate: false
---

This skill provides bash scripts to simulate Go channel communication via filesystem:

- `scripts/send-channel.sh` - Send message to channel
- `scripts/receive-channel.sh` - Receive message from channel (blocking)

Used by code-like-prompt commands (05l, 05m) to demonstrate goroutine-to-goroutine channel communication via filesystem.
