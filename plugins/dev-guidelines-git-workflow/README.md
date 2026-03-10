# Git Repository Workflow Plugin

Git repository workflow guidelines for Claude Code.

## Overview

Provides Git repository workflow guidelines including branch management, testing practices (TDD), Docker environment handling, and PR workflow.

## Skills

### Git Repository Workflow

**When activated:** When implementing features, fixing bugs, or making any code changes in a Git repository.

**What it does:**
- Provides Git branch management workflow guidelines
- Determines whether to create new branch or work on existing PR branch
- Ensures latest changes are pulled before starting work
- Provides TDD methodology (Red-Green-Refactor cycle)
- Checks test necessity before adding tests to projects without existing tests
- Handles Docker environment command execution (docker exec when container is running)
- References [web-app-testing.md](skills/git-repository-workflow/web-app-testing.md) for E2E testing with Playwright MCP

## Installation

```bash
/plugin install dev-guidelines-git-workflow@kokuyouwind-plugins
```

## Author

kokuyouwind
