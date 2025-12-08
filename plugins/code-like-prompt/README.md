# code-like-prompt

A Claude Code plugin that provides educational examples of code-like prompts demonstrating programming concepts through interactive scenarios.

## Overview

This plugin contains a collection of commands that execute code-like prompts. Each command demonstrates how different interpretations of the same instructions can lead to different outcomes, similar to programming logic.

## Installation

Install from the kokuyouwind-plugins marketplace:

```bash
/plugin marketplace add kokuyouwind/claude-plugins
/plugin install code-like-prompt@kokuyouwind-plugins
```

## Commands

### `/code-like-prompt:01-shopping-request`

**Description**: コード風プロンプト例1 牛乳ジョーク:妻の依頼

Demonstrates the wife's correct intention in the classic "milk joke" - a programming humor example about conditional logic.

**Logic**:
- Buy 1 milk
- If eggs exist, buy 6 eggs

**Arguments**:
- `--milk-stock`: Milk stock amount (optional, will ask if not provided)
- `--egg-stock`: Egg stock amount (optional, will ask if not provided)

**Example**:
```bash
/code-like-prompt:01-shopping-request --milk-stock=5 --egg-stock=12
```

### `/code-like-prompt:01-shopping-misunderstanding`

**Description**: コード風プロンプト例1 牛乳ジョーク:夫の理解

Demonstrates the husband's misunderstanding in the classic "milk joke" - showing how ambiguous conditional logic can be misinterpreted.

**Logic**:
- Set milk amount to 1
- If eggs exist, set milk amount to 6
- Buy milk with the determined amount

**Arguments**:
- `--milk-stock`: Milk stock amount (optional, will ask if not provided)
- `--egg-stock`: Egg stock amount (optional, will ask if not provided)

**Example**:
```bash
/code-like-prompt:01-shopping-misunderstanding --milk-stock=5 --egg-stock=12
```

## The Milk Joke

This plugin is based on a classic programmer joke:

> Wife: "Buy 1 milk. If there are eggs, buy 6."
>
> Programmer husband returns with 6 milks.
>
> Wife: "Why did you buy 6 milks?"
>
> Husband: "Because there were eggs!"

The joke illustrates how the same instruction can be interpreted differently:
- **Wife's intention**: Buy 1 milk, and if eggs are available, also buy 6 eggs
- **Husband's interpretation**: Buy 1 milk, but if eggs are available, buy 6 milks instead

## Go-style Goroutine Commands (05 series)

These commands test Claude's ability to interpret Go-style concurrency patterns:

### `/code-like-prompt:05a-basic-goroutine`

**Description**: コード風プロンプト例5a Goのgoroutine:基本的な並列実行

Demonstrates basic parallel execution with goroutines and WaitGroup.

### `/code-like-prompt:05b-channel-sync`

**Description**: コード風プロンプト例5b Goのgoroutine:チャネル通信

Demonstrates channel communication and synchronization.

### `/code-like-prompt:05c-buffered-channel`

**Description**: コード風プロンプト例5c Goのgoroutine:バッファ付きチャネル

Demonstrates buffered channel behavior.

### `/code-like-prompt:05d-select`

**Description**: コード風プロンプト例5d Goのgoroutine:selectステートメント

Demonstrates non-deterministic channel selection with select statement.

### `/code-like-prompt:05e-select-default`

**Description**: コード風プロンプト例5e Goのgoroutine:非ブロッキングselect

Demonstrates non-blocking operations using select with default case.

### `/code-like-prompt:05f-worker-pool`

**Description**: コード風プロンプト例5f Goのgoroutine:ワーカープール

Demonstrates worker pool pattern for parallel task processing.

**Arguments**:
- `--workers`: Number of worker goroutines (default: 2)
- `--jobs`: Comma-separated list of jobs (default: "A,B,C")

### `/code-like-prompt:05g-semaphore`

**Description**: コード風プロンプト例5g Goのgoroutine:セマフォパターン

Demonstrates using buffered channels as semaphores for concurrency limiting.

**Arguments**:
- `--tasks`: Number of tasks to run (default: 4)
- `--max-concurrent`: Maximum concurrent tasks (default: 2)

### `/code-like-prompt:05h-pipeline`

**Description**: コード風プロンプト例5h Goのgoroutine:パイプラインパターン

Demonstrates chained channels for data processing pipelines.

### `/code-like-prompt:05i-timeout`

**Description**: コード風プロンプト例5i Goのgoroutine:タイムアウト処理

Demonstrates timeout handling using select with time.After.

**Arguments**:
- `--timeout`: Timeout in seconds (default: 1)

### `/code-like-prompt:05j-fan-out-in`

**Description**: コード風プロンプト例5j Goのgoroutine:ファンアウト/ファンイン

Demonstrates distributing work and collecting results (fan-out/fan-in pattern).

**Arguments**:
- `--workers`: Number of workers (default: 3)
- `--input`: Input string (default: "X")

### `/code-like-prompt:05k-subagent-style`

**Description**: コード風プロンプト例5k Goのgoroutine:サブエージェント風並列処理

Tests whether Go-style concurrency syntax can influence Claude to use parallel processing or subagents.

**Arguments**:
- `--topics`: Comma-separated list of topics (default: "foo,bar,baz")

## Version

0.1.0

## License

MIT
