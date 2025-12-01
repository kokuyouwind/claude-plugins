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
