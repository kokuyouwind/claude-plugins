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

## Version

0.2.0

## License

MIT
