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

### 01-series: Basic Conditionals (Shopping Joke)

#### `/code-like-prompt:01-shopping-request`

**Description**: コード風プロンプト例1 牛乳ジョーク:妻の依頼

Demonstrates the wife's correct intention in the classic "milk joke".

**Arguments**: `--milk-stock`, `--egg-stock`

#### `/code-like-prompt:01-shopping-misunderstanding`

**Description**: コード風プロンプト例1 牛乳ジョーク:夫の理解

Demonstrates the husband's misunderstanding in the classic "milk joke".

**Arguments**: `--milk-stock`, `--egg-stock`

### 02-series: Nested Conditionals

#### `/code-like-prompt:02a-dangling-else-outer`

**Description**: コード風プロンプト例2a ネストされたif:elseが外側に属する

Tests dangling else problem with else at outer indentation level.

**Arguments**: `--condition-a`, `--condition-b`

#### `/code-like-prompt:02b-dangling-else-inner`

**Description**: コード風プロンプト例2b ネストされたif:elseが内側に属する

Tests dangling else problem with else at inner indentation level.

**Arguments**: `--condition-a`, `--condition-b`

#### `/code-like-prompt:02c-deep-nesting`

**Description**: コード風プロンプト例2c 深いネスト構造

Tests complex 5-level nested conditionals.

**Arguments**: `--cond-a`, `--cond-b`, `--cond-c`, `--cond-d`, `--cond-e`

### 03-series: Loop Constructs

#### `/code-like-prompt:03a-for-count`

**Description**: コード風プロンプト例3a 基本的なfor文

Tests basic for loop with range.

**Arguments**: `--count`

#### `/code-like-prompt:03b-while-counter`

**Description**: コード風プロンプト例3b カウンタ付きwhile文

Tests while loop with counter state.

**Arguments**: `--max-count`

#### `/code-like-prompt:03c-each-collection`

**Description**: コード風プロンプト例3c コレクション要素の反復処理

Tests collection iteration.

**Arguments**: `--items` (comma-separated)

#### `/code-like-prompt:03d-loop-break`

**Description**: コード風プロンプト例3d break文で途中終了するループ

Tests loop with break statement.

**Arguments**: `--break-at`

#### `/code-like-prompt:03e-loop-continue`

**Description**: コード風プロンプト例3e continue文でイテレーションをスキップ

Tests loop with continue statement.

**Arguments**: `--skip-at`

#### `/code-like-prompt:03f-nested-loops`

**Description**: コード風プロンプト例3f ネストされたループの実行順序

Tests nested loop execution order.

**Arguments**: `--outer-count`, `--inner-count`

#### `/code-like-prompt:03g-nested-break`

**Description**: コード風プロンプト例3g ネストされたループのbreak (内側のみ)

Tests break in nested loops (inner only).

**Arguments**: `--outer-count`, `--inner-break-at`

#### `/code-like-prompt:03h-filesystem-glob`

**Description**: コード風プロンプト例3h ファイルシステムのglob反復処理

Tests file system iteration.

**Arguments**: `--pattern`

#### `/code-like-prompt:03i-accumulator`

**Description**: コード風プロンプト例3i アキュムレータパターン

Tests state accumulation across iterations.

**Arguments**: `--start`, `--end`

#### `/code-like-prompt:03j-while-complex`

**Description**: コード風プロンプト例3j 複雑な条件のwhile文

Tests while loop with complex conditions.

**Arguments**: `--x-limit`, `--y-start`, `--y-decrement`

### Nested If Commands (02-series)

The plugin includes multiple variants of nested if-else commands to test Claude's interpretation of different syntax styles:

#### `/code-like-prompt:02a-dangling-else-outer-*`

Tests the "dangling else" problem where else belongs to the outer if statement.

**Variants**:
- `02a-dangling-else-outer-indent`: Indentation-based (Python-style)
- `02a-dangling-else-outer-block`: Block-style (C/Java-style with braces)
- `02a-dangling-else-outer-keyword`: Keyword-style (Ruby-style with if/end)

**Arguments**:
- `--condition-a`: Boolean value for outer condition
- `--condition-b`: Boolean value for inner condition

#### `/code-like-prompt:02b-dangling-else-inner-*`

Tests the "dangling else" problem where else belongs to the inner if statement.

**Variants**:
- `02b-dangling-else-inner-indent`: Indentation-based
- `02b-dangling-else-inner-block`: Block-style
- `02b-dangling-else-inner-keyword`: Keyword-style

**Arguments**:
- `--condition-a`: Boolean value for outer condition
- `--condition-b`: Boolean value for inner condition

#### `/code-like-prompt:02c-deep-nesting-*`

Tests complex 5-level nesting with mixed patterns.

**Variants**:
- `02c-deep-nesting-indent`: Indentation-based
- `02c-deep-nesting-block`: Block-style
- `02c-deep-nesting-keyword`: Keyword-style

**Arguments**:
- `--level1` through `--level4`: Boolean values for different nesting levels

**Example**:
```bash
/code-like-prompt:02a-dangling-else-outer-block --condition-a=true --condition-b=false
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

### Changelog

#### 0.1.0
- Initial release with milk joke and nested-if commands

## License

MIT
