---
name: resolve-issue
description: Provides a systematic workflow for resolving GitHub Issues. Use this skill when assigned a GitHub Issue or when delegated by the Issue Auto-resolver.
---

# Resolve Issue

## Instructions

### Purpose

Achieve consistent, high-quality problem resolution by determining the GitHub Issue type and applying the appropriate resolution process for each type.

### Usage Timing

- When requested to resolve a GitHub Issue
- When delegated processing by the Issue Auto-resolver

### Resolution Process

#### Step 1: Determine Issue Type and Apply Label

First, check if the Issue has a `type:*` label.

**If no label is present:**

Read the Issue title and body, determine the appropriate type from the following 6 categories, and apply the corresponding label:

- `type:bug` - Fix bugs, errors, or defects
- `type:feature` - Add new features
- `type:investigation` - Investigation or analysis tasks
- `type:refactoring` - Code refactoring
- `type:documentation` - Update documentation
- `type:enhancement` - Improve existing features

If it is difficult to determine, apply `type:feature` by default.

#### Step 2: Follow Detailed Instructions for Each Type

Once the Issue type is determined, refer to the corresponding detailed instruction file at the following times:

**type:bug**
- **When to refer:** When you need to check specific steps for bug fixing.
- **Reference file:** `{plugin_base_path}/issue-types/bug.md`

**type:feature**
- **When to refer:** When you need to check specific steps for implementing new features.
- **Reference file:** `{plugin_base_path}/issue-types/feature.md`

**type:investigation**
- **When to refer:** When you need to check specific steps for investigation tasks.
- **Reference file:** `{plugin_base_path}/issue-types/investigation.md`

**type:refactoring**
- **When to refer:** When you need to check specific steps for refactoring.
- **Reference file:** `{plugin_base_path}/issue-types/refactoring.md`

**type:documentation**
- **When to refer:** When you need to check specific steps for updating documentation.
- **Reference file:** `{plugin_base_path}/issue-types/documentation.md`

**type:enhancement**
- **When to refer:** When you need to check specific steps for improving existing features.
- **Reference file:** `{plugin_base_path}/issue-types/enhancement.md`

### Common Guidelines

Important notes common to all Issue types:

**Code Quality**
- Check existing code patterns before implementation and maintain consistency.
- Avoid excessive feature additions or unnecessary refactoring.

**Git Operations**
- Manage branches according to the implementation workflow (implementation-workflow skill).
- Keep commit messages clear and concise.

**Creating PRs**
- Use the `gh pr` command when creating PRs.
- Write the PR title, comments, and description in Japanese (unless otherwise specified).
- Include an appropriate summary of changes in the PR description.

### Notes

- `{plugin_base_path}` refers to the base path of the plugin where this skill is installed.
- When actually referencing files, replace it with the appropriate path.
