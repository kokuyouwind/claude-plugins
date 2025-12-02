# Issue Resolver Plugin

Issue resolution workflow with automatic type detection and type-specific resolution procedures for GitHub issues.

## Overview

This plugin provides a skill that guides the systematic resolution of GitHub issues. It automatically detects issue types, assigns appropriate labels, and follows structured resolution procedures tailored to each issue type.

## Features

- **Automatic Issue Type Detection**: Analyzes issue title and body to determine the appropriate type
- **Type-Specific Procedures**: Provides detailed resolution workflows for each issue type
- **Structured Approach**: Ensures consistent and thorough issue resolution
- **Japanese Support**: PR descriptions and comments in Japanese

## Skills

### Issue Resolution Workflow

**When activated:** When resolving GitHub issues, analyzing issue types, or following issue resolution procedures.

**What it does:**
- Detects and assigns appropriate type labels (bug, feature, investigation, refactoring, documentation, enhancement)
- Follows type-specific resolution procedures
- Enforces best practices for code consistency and quality
- Guides proper Git workflow and PR creation

## Issue Types

The plugin supports six issue types, each with dedicated resolution procedures:

1. **type:bug** - Bug fixes, errors, and defects
2. **type:feature** - New feature additions
3. **type:investigation** - Investigation and analysis tasks
4. **type:refactoring** - Code refactoring
5. **type:documentation** - Documentation updates
6. **type:enhancement** - Improvements to existing features

## Common Guidelines

All issue resolutions follow these principles:

- **Code Consistency**: Review and follow existing code patterns
- **Focused Changes**: Make only necessary changes to address the issue
- **Proper Workflow**: Use implementation-workflow skill for branch management
- **Quality Standards**: Ensure proper testing and review
- **Japanese Communication**: Use Japanese for PR descriptions and comments

## Installation

```bash
/plugin marketplace add kokuyouwind/claude-plugins
/plugin install issue-resolver@kokuyouwind-plugins
```

## Usage

The skill is automatically activated when working on GitHub issue resolution. It will:

1. Check for existing type labels
2. Assign appropriate type label if missing
3. Follow the corresponding issue-type procedure
4. Guide through implementation and PR creation

You don't need to invoke the skill manually - it provides guidance automatically when relevant to issue resolution tasks.

## File Structure

```
issue-resolver/
├── .claude-plugin/
│   └── plugin.json          # Plugin metadata
├── skills/
│   └── resolve-issue/
│       └── SKILL.md         # Main skill with general procedures
├── issue-types/             # Type-specific procedures
│   ├── bug.md              # Bug resolution procedure
│   ├── feature.md          # Feature addition procedure
│   ├── investigation.md    # Investigation task procedure
│   ├── refactoring.md      # Refactoring procedure
│   ├── documentation.md    # Documentation update procedure
│   └── enhancement.md      # Enhancement procedure
└── README.md               # This file
```

## Version

1.0.0

## Author

kokuyouwind (https://github.com/kokuyouwind)

## License

MIT
