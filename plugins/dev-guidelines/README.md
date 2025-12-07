# Development Guidelines Plugin

Comprehensive development guidelines for Claude Code, including best practices for code implementation, debugging, design decisions, and GitHub workflows.

## Overview

This plugin provides four skills that guide development practices:

1. **Implementation Workflow** - Git branch management, testing practices (TDD), Docker environment handling, and code implementation workflow
2. **Debugging Process** - Methodology for code investigation, bug analysis, and root cause analysis
3. **Design Alternatives** - Approach for evaluating design decisions and architecture choices
4. **PR Description Format** - Format guidelines for GitHub PR descriptions in Japanese

## Skills

### Implementation Workflow

**When activated:** When implementing new features, making code changes, fixing bugs, or starting any code implementation work in a Git repository.

**What it does:**
- Guides Git branch management workflow
- Determines whether to create new branch or work on existing PR branch
- Ensures latest changes are pulled before starting work
- Provides TDD methodology (Red-Green-Refactor cycle)
- Checks test necessity before adding tests to projects without existing tests
- Handles Docker environment command execution (docker exec when container is running)
- References [web-app-testing.md](skills/implementation-workflow/web-app-testing.md) for E2E testing with Playwright MCP

### Debugging Process

**When activated:** When investigating code, analyzing bugs, debugging issues, performing root cause analysis, or handling errors.

**What it does:** Provides methodology for transparent real-time investigation with file:line references and enforces "investigate WHY before fixing" philosophy using 5 Whys technique.

### Design Alternatives

**When activated:** When making design decisions, choosing architecture patterns, selecting technologies, or evaluating implementation approaches.

**What it does:** Guides the process of proposing multiple alternatives (2-3 minimum) with clear advantages, disadvantages, reasoning, and future scenarios.

### PR Description Format

**When activated:** When creating GitHub pull requests or writing PR descriptions.

**What it does:** Provides format guidelines for PR descriptions in Japanese (概要/変更の背景/変更詳細) with clear What, Why, and How structure using Japanese plain form (da/dearu style). Checks for repository PR templates and uses them when available.

## Installation

```bash
/plugin install dev-guidelines@kokuyouwind-plugins
```

## Usage

Skills are automatically activated based on the context of your work. You don't need to invoke them manually - they provide guidance when relevant to your current task.

## Author

kokuyouwind
