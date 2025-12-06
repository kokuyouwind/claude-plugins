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

## Version

0.1.0

## License

MIT
