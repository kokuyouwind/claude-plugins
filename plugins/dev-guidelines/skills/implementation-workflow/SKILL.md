---
name: Implementation Workflow
description: Ensures proper Git branch management and PR workflow for code changes. Use this skill when implementing new features, bug fixes, or any code modifications in a Git repository.
version: 1.0.0
---

# Code Implementation Workflow

## CRITICAL: Never Push to Default Branches

❌ **PROHIBITED**: `git push origin main/develop/master`

ALL changes must go through Pull Requests. No exceptions.

## Workflow Decision

```
Start Implementation
    ↓
Existing PR? → YES → gh pr checkout <PR#> → git pull → Implement → Push
    ↓ NO
User says "current branch"? → YES → Implement directly
    ↓ NO
git checkout main && git pull → git checkout -b feature/name → Implement
```

## For New Work

```bash
git checkout main && git pull origin main
git checkout -b feature/descriptive-name  # or: fix/, refactor/, docs/
```

## For Existing PR

```bash
gh pr checkout <PR-number>
git pull origin <branch-name>
# Make changes
git push origin <branch-name>
```

## DO / DON'T

**DO**: Pull latest before starting, use descriptive branch names, verify current branch, follow existing patterns

**DON'T**: Push to main/develop directly, create new branch for existing PR work, skip pulling latest
