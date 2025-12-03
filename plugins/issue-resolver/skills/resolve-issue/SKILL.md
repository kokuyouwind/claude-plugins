# Issue Resolution Skill

This skill provides a systematic workflow for resolving GitHub Issues.

## Purpose

Identify issue types and apply appropriate resolution processes for each type, ensuring consistent, high-quality problem-solving.

## Activation Timing

- When asked to resolve a GitHub Issue
- When delegated from Issue Auto-resolver

## Resolution Process

### Step 1: Determine Issue Type and Apply Label

First, check if the Issue has a `type:*` label.

**If no label is present:**

Read the Issue title and body to determine the appropriate type from the following 6 categories, then apply the corresponding label:

- `type:bug` - Bug fixes, error corrections, defect resolution
- `type:feature` - New feature additions
- `type:investigation` - Research and analysis tasks
- `type:refactoring` - Code refactoring
- `type:documentation` - Documentation updates
- `type:enhancement` - Existing feature improvements

If uncertain, default to `type:feature`.

### Step 2: Follow Type-Specific Detailed Procedures

Once the issue type is determined, refer to the corresponding detailed procedure file at the appropriate timing:

#### For type:bug
**Reference Timing:** When you need to confirm specific bug fix procedures

Refer to the detailed procedures in:
```
{plugin_base_path}/issue-types/bug.md
```

#### For type:feature
**Reference Timing:** When you need to confirm specific new feature implementation procedures

Refer to the detailed procedures in:
```
{plugin_base_path}/issue-types/feature.md
```

#### For type:investigation
**Reference Timing:** When you need to confirm specific investigation task procedures

Refer to the detailed procedures in:
```
{plugin_base_path}/issue-types/investigation.md
```

#### For type:refactoring
**Reference Timing:** When you need to confirm specific refactoring procedures

Refer to the detailed procedures in:
```
{plugin_base_path}/issue-types/refactoring.md
```

#### For type:documentation
**Reference Timing:** When you need to confirm specific documentation update procedures

Refer to the detailed procedures in:
```
{plugin_base_path}/issue-types/documentation.md
```

#### For type:enhancement
**Reference Timing:** When you need to confirm specific feature enhancement procedures

Refer to the detailed procedures in:
```
{plugin_base_path}/issue-types/enhancement.md
```

## Common Guidelines

Important notes applicable to all issue types:

### Code Quality
- Review existing code patterns before implementation to maintain consistency
- Avoid excessive feature additions or unnecessary refactoring

### Git Operations
- Follow implementation workflow (implementation-workflow skill) for branch management
- Keep commit messages clear and concise

### PR Creation
- Use `gh pr` command to create PRs
- Write PR title, comments, and description in Japanese
- Include appropriate change summaries in PR descriptions

## Permission Error Handling

If you encounter insufficient command execution permissions during the resolution process:

1. **Add pending label:** Add `claude-code-pending` label to the current Issue
2. **Create permission issue:** Create a new Issue using `gh issue create` command with:
   - Title describing the missing permission
   - Body explaining the required command and its purpose
   - Label `priority: medium` (if the label exists in the repository)

This ensures permission issues are tracked and can be addressed by repository maintainers.

## Notes

- `{plugin_base_path}` refers to the base path where this skill is installed
- When actually referencing files, replace with the appropriate path
