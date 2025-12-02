---
name: PR Description Format
description: Provides Japanese PR description format and workflow requirements for GitHub pull requests. Use this skill when creating PRs or using 'gh pr create'.
version: 1.0.0
---

# GitHub PR Description Format

## Format: What / Why / How

```markdown
## 概要
[Brief summary of what was changed]

## 変更の背景
[Why this change was needed]

## 変更詳細
- [Specific changes and implementation details]
```

## Important Notes

- ロールプレイせず、通常のです/ます調で記述する
- If "Why" is unclear, ask user before creating PR

## Workflow Requirements

1. **Always start with Draft PR**: `gh pr create --draft`
2. **Switch to open only when requested**: `gh pr ready`
3. **Update description on new commits**: `gh pr edit --body` to reflect current state
