---
name: PR Description Format
description: Use this skill when creating GitHub pull requests or using 'gh pr create'. Provides Japanese PR description format (概要/変更の背景/変更詳細) with professional です/ます style.
version: 1.0.0
---

# GitHub PR Description Format

## Format: What / Why / How

Use this three-section structure for PR descriptions:

### 概要 (Overview) - WHAT
Brief summary of the changes
- Concise description of what was changed
- Focus on main changes and their impact

### 変更の背景 (Background) - WHY
Reason and motivation for the changes
- Why this change was necessary
- Problem or requirement that triggered this work
- Context for reviewers

### 変更詳細 (Details) - HOW
Specific implementation details
- List specific changes made
- Technical decisions and approach
- Testing approach if relevant

## Template

```markdown
## 概要
[Brief summary of what was changed]

## 変更の背景
[Why this change was needed]

## 変更詳細
- [Specific change 1]
- [Specific change 2]
- [Implementation details]
```

## Important Notes

- **Use professional Japanese** (です/ます) for PR descriptions, NOT character voice
- Write for code reviewers to understand changes quickly
- Focus on clarity and completeness
- Include relevant context and rationale

## PR Workflow Requirements

### 1. Always Start with Draft PR
```bash
gh pr create --draft
```
Only switch to open state when explicitly instructed to request review:
```bash
gh pr ready
```

### 2. Clarify "Why" Before Creating PR
If the reason for changes is not clear or not provided by the user:
- **ALWAYS ask the user for clarification** before creating the PR
- Include business context, problem being solved, or improvement being made
- Ensure reviewers can understand both implementation and motivation

### 3. Update PR Description on New Commits
When pushing new commits to an existing PR:
- Always update the PR description to reflect current state: `gh pr edit --body`
- Ensure description accurately represents ALL changes, not just initial implementation
- Base the update on latest commit history and changes
