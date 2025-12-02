---
name: Implementation Workflow
description: Use this skill when implementing code changes, new features, or bug fixes. Ensures proper Git branch management (update main → create feature branch OR checkout PR branch → make changes).
version: 1.0.0
---

# Code Implementation Workflow

## CRITICAL RULE: Never Push Directly to Default Branches

**ABSOLUTELY PROHIBITED:**
- ❌ `git push origin main`
- ❌ `git push origin develop`
- ❌ `git push origin master`

**ALL changes must go through Pull Requests. NO EXCEPTIONS.**

If you need to add files to default branch:
1. Create a separate PR branch
2. Submit a proper Pull Request
3. Never bypass the PR process

## Core Rule: Branch Hygiene First

**Before ANY code implementation**, follow proper branch workflow:

### For New Work
```bash
# Update default branch
git checkout main && git pull origin main

# Create feature branch with descriptive name
git checkout -b feature/descriptive-name
# or: fix/, refactor/, docs/, test/
```

**Exception**: Only skip branch creation if user explicitly says "work on current branch"

### For Existing PR Work
```bash
# Checkout PR branch
gh pr checkout <PR-number>

# Update branch
git pull origin <branch-name>

# Make changes, then push to same branch
git push origin <branch-name>
```

**Exception**: Only skip checkout if already on PR branch or user says "create new PR instead"

## Decision Tree

```
Start Implementation
    ↓
Is this for an existing PR?
    ├─ YES → Checkout PR branch → Update → Implement → Push to same branch
    └─ NO → User explicitly wants current branch?
              ├─ YES → Work on current branch
              └─ NO → Update main → Create feature branch → Implement
```

## Best Practices

### DO:
- Pull latest changes before starting
- Use descriptive branch names (`feature/user-auth`, `fix/login-error`)
- Verify current branch with `git branch --show-current`
- Follow existing code patterns and conventions
- Make focused, incremental changes

### DON'T:
- Work directly on main/master without explicit request
- Create new branch for existing PR work
- Make changes without checking current state
- Add unrequested features or refactorings
- Skip pulling latest changes

## Example Workflows

**New Feature**:
```markdown
User: "Add user registration"
→ git checkout main && git pull origin main
→ git checkout -b feature/user-registration
→ Implement feature
```

**Update Existing PR**:
```markdown
User: "Add changes to PR #123"
→ gh pr checkout 123 && git pull
→ Implement changes
→ git push origin <branch-name>
```

**Explicit Current Branch**:
```markdown
User: "Work on current branch, add feature"
→ Implement directly (no branch operations)
```

## Integration with Other Skills

- After branch setup, use **test-driven-approach** to implement tests first
- During implementation, use **debugging-process** when investigating existing code
- When creating PR, use **pr-description-format** for PR description
