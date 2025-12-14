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

### 04-series: Pattern Matching

#### `/code-like-prompt:04a-regex-match`

**Description**: コード風プロンプト例4a パターンマッチ:基本的な文字列パターンマッチング

Tests basic string pattern matching with regex-like patterns.

**Arguments**: `--text`

#### `/code-like-prompt:04b-structural-match`

**Description**: コード風プロンプト例4b パターンマッチ:構造的パターンマッチングとガード

Tests structural pattern matching with dict destructuring and guards.

**Arguments**: `--type`, `--value`

#### `/code-like-prompt:04c-list-destructure`

**Description**: コード風プロンプト例4c パターンマッチ:リスト/配列の分解

Tests sequence pattern matching with list destructuring.

**Arguments**: `--item1`, `--item2`, `--item3`

#### `/code-like-prompt:04d-nested-match`

**Description**: コード風プロンプト例4d パターンマッチ:ネストした構造のマッチング

Tests deep pattern matching on nested tree-like structures.

**Arguments**: `--left`, `--right-left`, `--right-right`

#### `/code-like-prompt:04e-multi-guard`

**Description**: コード風プロンプト例4e パターンマッチ:複数のガード条件

Tests pattern matching with complex guard conditions.

**Arguments**: `--x`, `--y`

#### `/code-like-prompt:04f-exhaustive`

**Description**: コード風プロンプト例4f パターンマッチ:網羅的パターンマッチング(Rust風)

Tests exhaustive pattern matching in Rust-style enum matching.

**Arguments**: `--color`, `--r`, `--g`, `--b`

### 04p-series: Prolog-style Backtracking

Commands demonstrating logic programming concepts like unification, backtracking, and constraint propagation.

#### `/code-like-prompt:04p-a-basic-facts`

**Description**: コード風プロンプト例4p-a Prologバックトラック:基本的なファクトとクエリ

Simple facts and queries with backtracking.

#### `/code-like-prompt:04p-b-multi-clause`

**Description**: コード風プロンプト例4p-b Prologバックトラック:複数節でのパス探索

Path finding with backtracking through multiple clauses.

#### `/code-like-prompt:04p-c-cut`

**Description**: コード風プロンプト例4p-c Prologバックトラック:カット演算子

Cut operator (!) preventing backtracking.

#### `/code-like-prompt:04p-d-tree-traverse`

**Description**: コード風プロンプト例4p-d Prologバックトラック:再帰的な木の走査

Recursive tree traversal with DFS order.

#### `/code-like-prompt:04p-e-findall`

**Description**: コード風プロンプト例4p-e Prologバックトラック:全解の収集

Collecting all solutions with findall.

#### `/code-like-prompt:04p-f-negation`

**Description**: コード風プロンプト例4p-f Prologバックトラック:失敗による否定

Negation as failure (\+).

#### `/code-like-prompt:04p-g-constraints`

**Description**: コード風プロンプト例4p-g Prologバックトラック:制約伝播によるグラフ彩色

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

## Version

0.2.0

### Changelog

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
