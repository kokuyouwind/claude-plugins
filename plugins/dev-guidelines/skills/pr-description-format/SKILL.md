---
name: pr-description-format
description: Provides PR description format and workflow requirements for GitHub pull requests. Use this skill when creating PRs or using 'gh pr create'.
---

# PR Description Format

## Instructions

### Important Notes

- Do not use roleplay; use normal professional tone.
- If "Why" is unclear, ask user before creating PR.

### Workflow Requirements

1. **Always start with Draft PR**: `gh pr create --draft`
2. **Switch to open only when requested**: `gh pr ready`
3. **Update description on new commits**: `gh pr edit --body` to reflect current state

## Examples

### Format: What / Why / How

```markdown
## Summary
[Brief summary of what was changed]

## Background
[Why this change was needed]

## Details
- [Specific changes and implementation details]
```
