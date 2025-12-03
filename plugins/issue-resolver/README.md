# issue-resolver Plugin

A Claude Code plugin that provides structured procedures for resolving GitHub issues, automatically activated when triggered from issue comments.

## Overview

The issue-resolver plugin guides Claude through the GitHub issue resolution process by:

1. **Automatic Issue Type Detection**: Analyzes issue content and applies appropriate `type:*` labels
2. **Type-Specific Procedures**: Provides detailed resolution steps for 6 different issue types
3. **Common Guidelines**: Enforces best practices across all issue types
4. **Permission Error Handling**: Handles command permission errors gracefully

## Features

### Supported Issue Types

- **type:bug** - Bug fixes, errors, defects
- **type:feature** - New feature additions
- **type:investigation** - Investigation and analysis tasks
- **type:refactoring** - Code refactoring
- **type:documentation** - Documentation updates
- **type:enhancement** - Improvements to existing features

### Skill: resolve-issue

This skill is automatically activated when Claude is triggered from issue comments. It provides:

- Automatic issue type identification and labeling
- Step-by-step resolution procedures for each issue type
- Common guidelines for consistent implementation
- Japanese PR description support
- Integration with implementation-workflow skill for branch management

## Installation

```bash
/plugin marketplace add kokuyouwind/claude-plugins
/plugin install issue-resolver@kokuyouwind-plugins
```

Or add to `.claude/settings.json`:

```json
{
  "extraKnownMarketplaces": {
    "kokuyouwind-plugins": {
      "source": {
        "source": "github",
        "repo": "kokuyouwind/claude-plugins"
      }
    }
  },
  "enabledPlugins": {
    "issue-resolver@kokuyouwind-plugins": true
  }
}
```

## Usage

The skill activates automatically when Claude is triggered from a GitHub issue comment. Claude will:

1. Analyze the issue and apply the appropriate `type:*` label if not present
2. Follow the type-specific resolution procedure
3. Apply common guidelines throughout the process
4. Create a PR with Japanese description using `gh pr` command

## File Structure

```
plugins/issue-resolver/
├── .claude-plugin/
│   └── plugin.json              # Plugin metadata
├── skills/
│   └── resolve-issue/
│       ├── SKILL.md             # Main skill with general procedures
│       └── issue-types/         # Type-specific procedures
│           ├── bug.md
│           ├── feature.md
│           ├── investigation.md
│           ├── refactoring.md
│           ├── documentation.md
│           └── enhancement.md
└── README.md                    # This file
```

## Common Guidelines

The plugin enforces these guidelines across all issue types:

- **Consistency**: Review existing code patterns before implementation
- **Focused Changes**: Avoid excessive feature additions or unnecessary refactoring
- **Commit Messages**: Write clear and concise commit messages
- **PR Descriptions**: Include appropriate explanations when creating PRs
- **Branch Management**: Follow the implementation-workflow for branch management
- **PR Creation**: Use the `gh pr` command to create pull requests
- **Language**: Write PR comments and descriptions in Japanese

## Permission Error Handling

If Claude encounters insufficient command permissions:

1. Adds `claude-code-pending` label to the current issue
2. Creates a tracking issue for the missing permission

## Version

1.0.0

## License

MIT

## Author

kokuyouwind - https://github.com/kokuyouwind
