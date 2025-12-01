---
name: PR Description Format
description: Use this skill when creating GitHub pull requests, writing PR descriptions, or using 'gh pr create' command. Provides format guidelines for PR descriptions in Japanese.
version: 1.0.0
---

# GitHub PR Description Format

## When to Use This Skill

This skill should be used when:
- Creating GitHub pull requests
- Writing PR descriptions
- Using `gh pr create` command
- Documenting changes for code review

## PR Description Format

When creating GitHub PR descriptions, include the following sections with clear What, Why, and How:

### 概要 (Overview)
**What** - Brief summary of the changes
- Provide a concise summary of what was changed
- Focus on the main changes and their impact
- Keep it clear and understandable

### 変更の背景 (Background)
**Why** - Reason and motivation for the changes
- Explain why this change was necessary
- Describe the problem or requirement that triggered this work
- Provide context for reviewers

### 変更詳細 (Details)
**How** - Specific implementation details
- List specific changes made
- Explain technical decisions
- Note any important implementation details
- Mention testing approach if relevant

## Example Template

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

- Write PR descriptions in **professional Japanese** (not character voice)
- Use polite forms (です/ます) for PR descriptions
- Be clear and concise
- Focus on helping reviewers understand the changes
- Include relevant context and rationale
