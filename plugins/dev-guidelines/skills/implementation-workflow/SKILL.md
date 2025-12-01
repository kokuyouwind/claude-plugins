---
name: Code Implementation Workflow
description: Use this skill when implementing code changes, new features, or bug fixes. Provides workflow for branch management and code implementation.
version: 1.0.0
---

# Code Implementation Workflow

## When to Use This Skill

This skill should be used when:
- Implementing new features
- Making code changes or modifications
- Fixing bugs
- Refactoring code
- Making any code implementation work

## Pre-Implementation Checklist

Before starting any code implementation, follow these steps:

### 1. Branch Preparation

**Default Behavior** (unless explicitly told otherwise):

#### For New Work (Non-PR Work)
1. **Update default branch**: Pull the latest changes from the default branch
   ```bash
   git checkout main  # or master, depending on repository
   git pull origin main
   ```

2. **Create feature branch**: Create a new branch with a descriptive name based on the work
   ```bash
   git checkout -b feature/descriptive-branch-name
   ```

**Exception**: Only work on the current branch if:
- User explicitly says "work on the current branch"
- User explicitly says "work in the current environment as-is"
- User explicitly says "don't create a new branch"

#### For Existing PR Work
When working on changes for an existing PR:
1. **Checkout PR branch**: Switch to the PR's branch
   ```bash
   gh pr checkout <PR-number>
   # or
   git checkout <branch-name>
   ```

2. **Update branch** (if needed): Pull latest changes from remote
   ```bash
   git pull origin <branch-name>
   ```

3. **Make changes and push**: After making changes, push to the same branch
   ```bash
   git add .
   git commit -m "commit message"
   git push origin <branch-name>
   ```

**Exception**: Only skip checkout if:
- User explicitly says "work on the current branch"
- Current branch is already the PR branch
- User explicitly says "create a new PR instead"

### 2. Verify Current State

Before making changes:
- Check current branch: `git branch --show-current`
- Check git status: `git status`
- Verify you're on the correct branch for the work

### 3. Understand the Context

- Read relevant code files
- Understand existing patterns and conventions
- Check for related tests
- Review any related documentation

## Implementation Process

### 1. Follow Existing Patterns

- Match coding style of the codebase
- Follow naming conventions
- Use similar patterns to existing code
- Maintain consistency

### 2. Incremental Changes

- Make small, focused commits
- Test changes as you go
- Don't make unrelated changes
- Keep the scope manageable

### 3. Respect Scope Boundaries

- Only change what's necessary
- Don't add unrequested features
- Don't refactor unrelated code
- Stay focused on the task

## Branch Naming Conventions

Use descriptive branch names that indicate the type and purpose of work:

### Prefixes
- `feature/` - New features or enhancements
- `fix/` - Bug fixes
- `refactor/` - Code refactoring
- `docs/` - Documentation updates
- `test/` - Test additions or modifications

### Examples
```bash
feature/user-authentication
fix/login-validation-error
refactor/payment-processing
docs/api-documentation
test/checkout-flow
```

## Decision Tree

```
Start Implementation
    ↓
Is this work for an existing PR?
    ├─ YES → Checkout PR branch
    │         ↓
    │      Update branch (git pull)
    │         ↓
    │      Make changes
    │         ↓
    │      Push to same branch
    │
    └─ NO → Is user explicitly asking to work on current branch?
              ├─ YES → Work on current branch as-is
              │
              └─ NO → Update default branch
                       ↓
                    Create new feature branch
                       ↓
                    Make changes
```

## Best Practices

### DO:
- Pull latest changes before starting work
- Create descriptive branch names
- Work on feature branches, not default branch
- Update PR branches before making changes
- Push changes to PR branches after implementation
- Verify current branch before starting
- Follow existing codebase patterns

### DON'T:
- Work directly on default branch (main/master)
- Create branches without updating default branch first
- Make changes without checking current state
- Assume current branch is correct
- Skip pulling latest changes
- Create new branches for existing PR work
- Push to wrong branch

## Example Workflows

### Example 1: New Feature Implementation

```markdown
1. User: "Add user registration feature"

2. Claude: "わかった！ユーザー登録機能を実装するね。まず最新のmainブランチを取得して、feature/user-registrationブランチを作成するよ"

3. Actions:
   git checkout main
   git pull origin main
   git checkout -b feature/user-registration

4. Claude: "ブランチを作成したよ！それじゃあ実装を始めるね"
```

### Example 2: Working on Existing PR

```markdown
1. User: "PR #123に変更を追加して"

2. Claude: "わかった！PR #123のブランチに変更を追加するね。まずそのブランチをチェックアウトするよ"

3. Actions:
   gh pr checkout 123
   git pull

4. Claude: "ブランチに切り替えたよ！変更を実装して、同じブランチにpushするね"

5. [Make changes]

6. Actions:
   git add .
   git commit -m "message"
   git push origin <branch-name>
```

### Example 3: Explicit Current Branch Usage

```markdown
1. User: "このままのブランチで作業して、機能を追加して"

2. Claude: "わかった！現在のブランチのまま作業するね"

3. Actions:
   [Make changes directly without branch operations]
```

## Important Notes

- **Branch hygiene is important**: Always work on appropriate branches
- **Default branch protection**: Avoid direct commits to main/master
- **PR workflow**: Existing PRs should receive updates on their branches
- **Context matters**: Understanding existing patterns prevents inconsistencies
- **Explicit instructions override**: User's explicit instructions always take precedence

## Common Mistakes to Avoid

1. **Making changes without pulling latest**: Can cause merge conflicts
2. **Working on wrong branch**: Can create confusion and merge issues
3. **Creating new branch for PR work**: Should update existing PR branch instead
4. **Assuming current environment is ready**: Always verify before starting
5. **Skipping branch creation**: Should use feature branches, not default branch

## Integration with Other Skills

This skill works together with:
- **test-driven-approach**: After setting up branch, implement tests
- **investigation-process**: Before implementation, investigate existing code
- **root-cause-analysis**: When fixing bugs, analyze root cause first
- **pr-description-format**: When creating PR from feature branch
