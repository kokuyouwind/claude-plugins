# Issue Resolution Skill

This skill provides procedures for resolving GitHub issues. It is automatically activated when triggered from issue comments.

## Overview

This skill guides you through resolving GitHub issues by:
1. Identifying the issue type and applying appropriate labels
2. Following type-specific resolution procedures
3. Applying common guidelines across all issue types

## 1. Issue Type Identification and Labeling

First, check if the issue has a `type:*` label assigned.

**If no type label exists:**
Read the issue title and body to determine the appropriate type from the following 6 categories, then apply the corresponding label:

- `type:bug` - Bug fixes, errors, defects
- `type:feature` - New feature additions
- `type:investigation` - Investigation and analysis tasks
- `type:refactoring` - Code refactoring
- `type:documentation` - Documentation updates
- `type:enhancement` - Improvements to existing features

If the type is ambiguous, default to `type:feature`.

## 2. Type-Specific Resolution Procedures

After identifying the issue type, refer to the detailed procedures in the corresponding file below:

### type:bug
**When to reference:** When you need specific procedures for bug fixing

Detailed procedures: {plugin_base_path}/issue-types/bug.md

### type:feature
**When to reference:** When you need specific procedures for adding new features

Detailed procedures: {plugin_base_path}/issue-types/feature.md

### type:investigation
**When to reference:** When you need specific procedures for investigation tasks

Detailed procedures: {plugin_base_path}/issue-types/investigation.md

### type:refactoring
**When to reference:** When you need specific procedures for refactoring

Detailed procedures: {plugin_base_path}/issue-types/refactoring.md

### type:documentation
**When to reference:** When you need specific procedures for documentation updates

Detailed procedures: {plugin_base_path}/issue-types/documentation.md

### type:enhancement
**When to reference:** When you need specific procedures for feature enhancements

Detailed procedures: {plugin_base_path}/issue-types/enhancement.md

## 3. Common Guidelines

Apply these guidelines across all issue types:

- **Consistency:** Review existing code patterns before implementation to maintain consistency
- **Focused Changes:** Avoid excessive feature additions or unnecessary refactoring beyond the issue scope
- **Commit Messages:** Write clear and concise commit messages
- **PR Descriptions:** Include appropriate explanations when creating PRs
- **Branch Management:** Follow the implementation-workflow for branch management
- **PR Creation:** Use the `gh pr` command to create pull requests
- **Language:** Write PR comments and descriptions in Japanese

## 4. Permission Error Handling

If you encounter insufficient command permissions during the resolution process:

1. **Add pending label:** Add the `claude-code-pending` label to the current issue using `gh issue edit`
2. **Create tracking issue:** Create a new issue to track the missing permission using `gh issue create` with:
   - Clear description of the missing command/permission
   - Reference to the blocked issue
