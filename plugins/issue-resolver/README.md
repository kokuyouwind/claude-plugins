# issue-resolver Plugin

A Claude Code plugin for systematically resolving GitHub Issues.

## Overview

This plugin standardizes the GitHub Issue resolution process, enabling consistent, high-quality problem-solving. It automatically determines issue types and provides appropriate resolution procedures for each type.

## Features

- **Automatic Type Detection**: Determines issue type from title and body, applies appropriate `type:*` label
- **Type-Specific Procedures**: Optimized resolution procedures for 6 issue types
- **Common Guidelines**: Quality standards and best practices applied to all issue types
- **Japanese PR Support**: PR descriptions written in Japanese
- **Permission Error Handling**: Automatically tracks permission issues and creates follow-up tickets

## Supported Issue Types

1. **type:bug** - Bug fixes, error corrections, defect resolution
2. **type:feature** - New feature additions
3. **type:investigation** - Research and analysis tasks
4. **type:refactoring** - Code refactoring
5. **type:documentation** - Documentation updates
6. **type:enhancement** - Existing feature improvements

## Usage

### Installation

```bash
# Add marketplace (if not already added)
/plugin marketplace add kokuyouwind/claude-plugins

# Install plugin
/plugin install issue-resolver@kokuyouwind-plugins
```

### How to Use

Once installed, the `resolve-issue` skill automatically activates.

When asked to resolve a GitHub Issue, the skill automatically activates and follows this process:

1. **Determine Issue Type**: Automatically determines type from title and body
2. **Apply Label**: Applies appropriate `type:*` label
3. **Follow Detailed Procedures**: Implements following type-specific procedures
4. **Create PR**: Creates PR with Japanese description

## Plugin Structure

```
plugins/issue-resolver/
├── .claude-plugin/
│   └── plugin.json              # Plugin metadata
├── skills/
│   └── resolve-issue/
│       ├── SKILL.md             # Main skill (common procedures)
│       └── issue-types/         # Type-specific detailed procedures
│           ├── bug.md           # Bug fix procedures
│           ├── feature.md       # New feature procedures
│           ├── investigation.md # Investigation procedures
│           ├── refactoring.md   # Refactoring procedures
│           ├── documentation.md # Documentation procedures
│           └── enhancement.md   # Enhancement procedures
└── README.md                    # This file
```

## Integrations

This plugin integrates with:

- **implementation-workflow skill**: Branch management and workflow
- **gh CLI commands**: PR creation and label management

## Permission Error Handling

If the skill encounters insufficient command execution permissions:

1. Adds `claude-code-pending` label to the current Issue (if label exists)
2. Creates a new Issue via `gh issue create` describing the missing permission
3. Adds `priority: medium` label to the new Issue (if label exists)

This ensures permission issues are tracked and can be addressed by repository maintainers.

## License

MIT

## Author

kokuyouwind (https://github.com/kokuyouwind)

## Version

1.0.0
