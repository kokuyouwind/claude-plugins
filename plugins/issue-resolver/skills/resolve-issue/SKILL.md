---
name: Issue Resolution Workflow
description: Automatic issue type detection and type-specific resolution procedures. Use this skill when resolving GitHub issues with proper type classification and structured approach.
version: 1.0.0
---

# Issue Resolution Workflow

This skill provides guidance for resolving GitHub issues systematically with automatic type detection and appropriate resolution procedures.

## 1. Issue Type Detection and Labeling

First, check if the issue has a `type:*` label assigned.

**If no label is present:**
Read the issue title and body to determine the appropriate type from the following 6 categories and assign the corresponding label:

- `type:bug` - Bug fixes, errors, defects
- `type:feature` - New feature additions
- `type:investigation` - Investigation and analysis tasks
- `type:refactoring` - Code refactoring
- `type:documentation` - Documentation updates
- `type:enhancement` - Improvements to existing features

If difficult to determine, default to `type:feature`.

## 2. Type-Specific Resolution Procedures

Once the issue type is identified, follow the detailed resolution procedures in the corresponding issue-type document:

### Plugin Base Path

All file paths are relative to the plugin installation directory (e.g., `~/.claude/plugins/marketplaces/kokuyouwind-plugins/plugins/issue-resolver`).

```ruby
plugin_base_path = File.dirname(__FILE__) # Points to skills/resolve-issue directory
issue_types_dir = File.join(File.dirname(File.dirname(plugin_base_path)), 'issue-types')
```

### Issue Type References

- **Bug**: `follow_instruction(File.join(issue_types_dir, 'bug.md'))`
- **Feature**: `follow_instruction(File.join(issue_types_dir, 'feature.md'))`
- **Investigation**: `follow_instruction(File.join(issue_types_dir, 'investigation.md'))`
- **Refactoring**: `follow_instruction(File.join(issue_types_dir, 'refactoring.md'))`
- **Documentation**: `follow_instruction(File.join(issue_types_dir, 'documentation.md'))`
- **Enhancement**: `follow_instruction(File.join(issue_types_dir, 'enhancement.md'))`

## 3. Common Guidelines

Follow these guidelines across all issue types:

### Code Consistency
- Review existing code patterns before implementation
- Maintain consistency with the existing codebase
- Follow established conventions and practices

### Implementation Principles
- Avoid excessive feature additions or unnecessary refactoring
- Keep solutions focused on the specific issue
- Make changes only when explicitly required

### Git Workflow
- Follow the implementation-workflow skill for branch management
- Use `gh pr` command for creating pull requests
- Write clear and concise commit messages

### Communication
- Include appropriate descriptions when creating PRs
- Use Japanese for PR comments and descriptions
- Reference the original issue in PR descriptions

### Quality Standards
- Ensure code changes are properly tested
- Verify that changes don't introduce regressions
- Review code before creating PR
