# Development Guidelines Plugin

Comprehensive development guidelines for Claude Code, including best practices for code implementation, debugging, design decisions, testing methodology, GitHub workflows, and web research.

## Overview

This plugin provides six skills that guide development practices:

1. **Implementation Workflow** - Git branch management and code implementation workflow
2. **Debugging Process** - Methodology for code investigation, bug analysis, and root cause analysis
3. **Design Alternatives** - Approach for evaluating design decisions and architecture choices
4. **Test-Driven Approach** - TDD methodology and testing best practices
5. **PR Description Format** - Format guidelines for GitHub PR descriptions in Japanese
6. **Web Tools** - Guidelines for web information retrieval and documentation

## Skills

### Implementation Workflow

**When activated:** When implementing new features, making code changes, fixing bugs, or starting any code implementation work in a Git repository.

**What it does:** Guides Git branch management workflow, determines whether to create new branch or work on existing PR branch, and ensures latest changes are pulled before starting work.

### Debugging Process

**When activated:** When investigating code, analyzing bugs, debugging issues, performing root cause analysis, or handling errors.

**What it does:** Provides methodology for transparent real-time investigation with file:line references and enforces "investigate WHY before fixing" philosophy using 5 Whys technique.

### Design Alternatives

**When activated:** When making design decisions, choosing architecture patterns, selecting technologies, or evaluating implementation approaches.

**What it does:** Guides the process of proposing multiple alternatives (2-3 minimum) with clear advantages, disadvantages, reasoning, and future scenarios.

### Test-Driven Approach

**When activated:** When implementing tests, practicing TDD, creating test cases, or planning test strategy.

**What it does:** References specific methodologies (t_wada's TDD, Kent Beck's TDD) and provides test skeleton/structure first before implementation following Red-Green-Refactor cycle.

### PR Description Format

**When activated:** When creating GitHub pull requests or writing PR descriptions.

**What it does:** Provides format guidelines for PR descriptions in Japanese (概要/変更の背景/変更詳細) with clear What, Why, and How structure using professional Japanese (です/ます).

### Web Tools

**When activated:** When performing web information retrieval, web scraping, browser automation, or documentation tasks.

**What it does:** Provides guidelines for using Playwright MCP for web scraping/browser automation and Obsidian MCP for documentation and note-taking.

## Installation

```bash
/plugin install dev-guidelines@claude-plugins-private
```

## Usage

Skills are automatically activated based on the context of your work. You don't need to invoke them manually - they provide guidance when relevant to your current task.

## Author

kokuyouwind
