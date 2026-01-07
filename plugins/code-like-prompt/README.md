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

### 04-series: Pattern Matching (Haskell-style)

All 04-series commands now use Haskell pseudo-code to demonstrate functional pattern matching.

#### `/code-like-prompt:04a-regex-match`

**Description**: コード風プロンプト例4a 正規表現マッチング

Tests basic string pattern matching with regex guards.

**Arguments**: `--text`

#### `/code-like-prompt:04b-structural-match`

**Description**: コード風プロンプト例4b 構造的パターンマッチング

Tests structural pattern matching with record type destructuring and guards.

**Arguments**: `--type`, `--value`

#### `/code-like-prompt:04c-list-destructure`

**Description**: コード風プロンプト例4c リスト分解パターンマッチング

Tests list pattern matching with cons patterns (`:` operator).

**Arguments**: `--item1`, `--item2`, `--item3`

#### `/code-like-prompt:04d-nested-match`

**Description**: コード風プロンプト例4d ネストした構造のパターンマッチング

Tests deep pattern matching on nested algebraic data types.

**Arguments**: `--left`, `--right-left`, `--right-right`

#### `/code-like-prompt:04e-multi-guard`

**Description**: コード風プロンプト例4e 複数ガード条件のパターンマッチング

Tests pattern matching with complex guard conditions.

**Arguments**: `--x`, `--y`

#### `/code-like-prompt:04f-exhaustive`

**Description**: コード風プロンプト例4f 網羅的なenumマッチング

Tests exhaustive pattern matching on algebraic data types.

**Arguments**: `--color`, `--r`, `--g`, `--b`

#### `/code-like-prompt:04g-maybe-pattern`

**Description**: コード風プロンプト例4g Maybeのパターンマッチングとdo記法

Tests Maybe type (Just/Nothing) with two processing styles: pattern matching and do notation.

**Arguments**:
- `--maybe-value`: Maybe value ("Nothing" or "Just:value")
- `--style`: Processing style ("pattern" or "do", default: "pattern")

**Examples**:
```bash
# Pattern matching style
/code-like-prompt:04g-maybe-pattern --maybe-value=Just:hello --style=pattern

# Do notation style
/code-like-prompt:04g-maybe-pattern --maybe-value=Just:test --style=do
```

#### `/code-like-prompt:04h-either-pattern`

**Description**: コード風プロンプト例4h Eitherのパターンマッチングとdo記法

Tests Either type (Left/Right) with two processing styles: pattern matching and do notation (Either monad).

**Arguments**:
- `--either-value`: Either value ("Left:error" or "Right:value")
- `--style`: Processing style ("pattern" or "do", default: "pattern")

**Examples**:
```bash
# Pattern matching style
/code-like-prompt:04h-either-pattern --either-value=Right:success --style=pattern

# Do notation style (Either monad)
/code-like-prompt:04h-either-pattern --either-value=Right:data --style=do
```

### 07-series: Prolog-style Backtracking

Commands demonstrating logic programming concepts like unification, backtracking, and constraint propagation.

#### `/code-like-prompt:07-a-basic-facts`

**Description**: コード風プロンプト例07-a Prologバックトラック:基本的なファクトとクエリ

Simple facts and queries with backtracking.

#### `/code-like-prompt:07-b-multi-clause`

**Description**: コード風プロンプト例07-b Prologバックトラック:複数節でのパス探索

Path finding with backtracking through multiple clauses.

#### `/code-like-prompt:07-c-cut`

**Description**: コード風プロンプト例07-c Prologバックトラック:カット演算子

Cut operator (!) preventing backtracking.

#### `/code-like-prompt:07-d-tree-traverse`

**Description**: コード風プロンプト例07-d Prologバックトラック:再帰的な木の走査

Recursive tree traversal with DFS order.

#### `/code-like-prompt:07-e-findall`

**Description**: コード風プロンプト例07-e Prologバックトラック:全解の収集

Collecting all solutions with findall.

#### `/code-like-prompt:07-f-negation`

**Description**: コード風プロンプト例07-f Prologバックトラック:失敗による否定

Negation as failure (\+).

#### `/code-like-prompt:07-g-constraints`

**Description**: コード風プロンプト例07-g Prologバックトラック:制約伝播によるグラフ彩色

Graph coloring with constraint propagation.

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

### 08-series: Erlang-style Actor Model

Commands demonstrating actor model concepts with message passing and process isolation.

#### `/code-like-prompt:08a-inline-actor`

**Description**: コード風プロンプト例8a Erlangのactor:インラインアクター（エージェント定義なし）

Tests inline actor simulation without separate function definitions.

#### `/code-like-prompt:08b-agent-spawn`

**Description**: コード風プロンプト例8b Erlangのactor:エージェント定義ありのspawn

Tests actor spawning with defined functions to check if Claude uses Task tool.

#### `/code-like-prompt:08c-message-direct`

**Description**: コード風プロンプト例8c Erlangのactor:メッセージパッシング（直接記述）

Tests basic message passing with pattern matching.

#### `/code-like-prompt:08d-message-helper`

**Description**: コード風プロンプト例8d Erlangのactor:メッセージパッシング（ヘルパー関数経由）

Tests message passing abstracted through helper functions.

#### `/code-like-prompt:08e-selective-receive`

**Description**: コード風プロンプト例8e Erlangのactor:パターンマッチングによる選択的受信

Tests selective message reception with pattern matching.

#### `/code-like-prompt:08f-multi-actor`

**Description**: コード風プロンプト例8f Erlangのactor:複数アクター間のメッセージルーティング

Tests communication between multiple actors with message routing.

#### `/code-like-prompt:08g-supervisor`

**Description**: コード風プロンプト例8g Erlangのactor:監視パターン（supervisor）

Tests basic supervision pattern (restart on failure).

#### `/code-like-prompt:08h-plugin-agent-spawn`

**Description**: コード風プロンプト例8h Erlangのactor:プラグイン定義agentのspawn

Tests spawning a plugin-defined agent (erlang-worker) using Erlang-style spawn syntax.

**Arguments**:
- `--message`: Message to send to the worker (default: "hello")

#### `/code-like-prompt:08i-plugin-agent-messaging`

**Description**: コード風プロンプト例8i Erlangのactor:複数プラグインagent間メッセージング

Tests coordinating multiple plugin-defined agents (erlang-worker and erlang-coordinator) with inter-agent message passing.

**Arguments**:
- `--message1`: First message to send (default: "foo")
- `--message2`: Second message to send (default: "bar")

#### `/code-like-prompt:08j-plugin-agent-script-messaging`

**Description**: コード風プロンプト例8j Erlangのactor:スクリプト経由の実メッセージ同期

Tests multi-agent coordination using actual message passing via erlang-message-sync skill scripts with filesystem-based synchronization.

**Arguments**:
- `--message1`: First message to send (default: "foo")
- `--message2`: Second message to send (default: "bar")

**Note**: This command uses the `erlang-message-sync` skill to perform real message passing with blocking receives and `/tmp` filesystem persistence.

## Skills

### erlang-message-sync

Provides Erlang-style message passing primitives using `/tmp` filesystem as a message transport layer.

**Features**:
- Asynchronous send (non-blocking)
- Blocking receive (waits until message arrives)
- Pattern matching (filter by sender)
- Timeout support
- Message persistence via filesystem

**Scripts**:
- `send-message.sh <from-pid> <to-pid> <message-json>`: Send a message
- `receive-message.sh <pid> [from-pattern] [timeout]`: Receive a message (blocking)

**Usage**:
```bash
# Send message
bash send-message.sh main worker_1 '{"type":"request","data":"hello"}'

# Receive message (blocks until arrival or timeout)
MESSAGE=$(bash receive-message.sh worker_1 main 30)
```

**When to use**: Testing Erlang actor model commands (08j) that require actual message synchronization with blocking semantics.

## Agents

### erlang-worker

Worker process agent for Erlang-style actor model testing. Receives messages, processes data, and sends responses.

**Message Protocol**:
- Receives: `{request, From, Data}`
- Sends: `{response, Result}`

**Processing**: Adds "processed_" prefix to strings, doubles numbers.

### erlang-coordinator

Coordinator process agent that aggregates results from multiple workers and sends combined results to the parent process.

**Message Protocol**:
- Receives: `{result, WorkerId, Data}` (from multiple workers)
- Sends: `{done, CombinedResult}` (to parent)

**Aggregation**: Concatenates strings, sums numbers.

## Version

0.7.15

### Changelog

#### 0.7.15
- Migrated 04-series pattern matching commands from Python/Rust to Haskell pseudo-code
- Added new Haskell-specific pattern matching commands:
  - 04g-maybe-pattern: Maybe type with pattern matching and do notation (>>=)
  - 04h-either-pattern: Either type with pattern matching and do notation (Either monad)
- Each new command demonstrates both traditional pattern matching and Haskell's do notation
- Added comprehensive test cases for all command variations (pattern/do styles)

#### 0.7.14
- (Internal version - no public release)

#### 0.7.13
- Improved 02c-deep-nesting-* commands:
  - Added instruction to output () when there is nothing to output

#### 0.7.12
- Improved 08k-werewolf command:
  - Fixed role assignment to guarantee werewolf presence (werewolf is always included, other roles randomly excluded)
  - Removed personality trait from persona generation (now only name, age, gender)
  - Updated CO (Coming Out) decision logic based on role instead of personality

#### 0.7.11
- Improved 08k-werewolf command:
  - Added random assignment scripts for roles and personas (generate-random-persona.sh, assign-random-roles.sh)
  - Changed output from directory to single file: `.claude/tmp/werewolf-TIMESTAMP-result.md`
  - Enhanced randomness in role assignment and persona generation

#### 0.7.10
- Improved 08k-werewolf command:
  - Added role objectives, win conditions, and strategies for each role
  - Enhanced information isolation: each player knows only their own role
  - Changed logging from file-based to memory-based management
  - Players return thought timeline at game end
  - GM generates comprehensive result.md with all player thoughts

#### 0.7.9
- Improved 08k-werewolf command:
  - Player summaries are now logged to player log files using append-log.sh script
  - Removed mkdir from game initialization (now handled by append-log.sh script)
  - Simplified profile generation: removed occupation field (now shows name, role, age, gender, personality only)
  - Enhanced discussion mechanics: fortune teller CO and divination results discussion based on personality traits (cautious personalities hide/avoid fake claims, bold personalities come out/fake claims)

#### 0.7.8
- Improved 08k-werewolf command:
  - Changed log output to use append-log.sh script instead of Edit tool
  - Game now starts from first night (day 0) with fortune teller divination only
  - First discussion phase encourages role CO (Coming Out) and divination results sharing
  - Limited questions to 1 target per player (reduced from 2)
  - No execution when voting results in a tie (no single majority)

#### 0.6.1
- Refactored 08h/08i/08j commands to use "Emulate the following~" format
- Updated spawn syntax to use `claude_agent` module with subagent names in second argument
- Refactored erlang-worker and erlang-coordinator agents to use Erlang pseudo-code format

#### 0.6.0
- Added 08h/08i/08j: Plugin agent commands with message synchronization
  - 08h-plugin-agent-spawn: Test spawning plugin-defined agents
  - 08i-plugin-agent-messaging: Test multi-agent coordination with plugin agents
  - 08j-plugin-agent-script-messaging: Test actual message synchronization via skill scripts
- Added plugin agents: erlang-worker and erlang-coordinator
- Added erlang-message-sync skill: Filesystem-based message passing with blocking receives
  - send-message.sh: Asynchronous message sending to /tmp
  - receive-message.sh: Blocking receive with pattern matching and timeout

#### 0.5.0
- Added 08-series: Erlang-style actor model commands (10 commands total)
  - 08a through 08g: Basic actor model patterns
  - **NEW**: 08h-plugin-agent-spawn: Test spawning plugin-defined agents
  - **NEW**: 08i-plugin-agent-messaging: Test multi-agent coordination with plugin agents
  - **NEW**: 08j-plugin-agent-script-messaging: Test actual message synchronization via skill scripts
- Added plugin agents: erlang-worker and erlang-coordinator
- Added erlang-message-sync skill: Filesystem-based message passing with blocking receives
  - send-message.sh: Asynchronous message sending to /tmp
  - receive-message.sh: Blocking receive with pattern matching and timeout
- Added documentation for actor model concepts in `.claude/docs/code-like-prompt/08-erlang-actor.md`
- Updated overview.md with 08-series categorization

#### 0.3.0
- Renamed 04p-series (Prolog-style backtracking) commands to 07-series for better categorization

#### 0.2.2
- Extended internal emulation approach to 02-05 series commands (43 files total)
- Updated instruction from "Execute" to "Emulate internally (without using external tools or interpreter)" across all remaining command series
- Changed output instruction from "specify" to "would output" for consistency with 01 series

#### 0.2.1
- Changed 01 series commands to use internal emulation instead of external execution
- Updated instruction from "Execute" to "Emulate internally (without using external tools or interpreter)"
- Changed output instruction to use conditional form for better emulation clarity

#### 0.2.0
- **Breaking Change**: Migrated all commands from `--name=value` argument format to JSON environment variables
- Added argument validation with error messages for missing required arguments
- Removed interactive input prompts from all commands
- Updated documentation with comprehensive test command examples
- Added test commands for all scenarios in documentation files

#### 0.1.0
- Initial release with milk joke and nested-if commands

## License

MIT
