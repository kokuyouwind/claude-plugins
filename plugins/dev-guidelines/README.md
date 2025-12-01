# Development Guidelines Plugin

Comprehensive development guidelines for Claude Code, including best practices for code investigation, design decisions, error analysis, testing methodology, GitHub workflows, and web research.

## Overview

This plugin provides six skills that guide development practices:

1. **Investigation Process** - Methodology for code investigation and bug analysis
2. **Design Alternatives** - Approach for evaluating design decisions
3. **Root Cause Analysis** - Guidelines for finding and fixing root causes
4. **PR Description Format** - Format guidelines for GitHub PR descriptions
5. **Test-Driven Approach** - TDD methodology and testing best practices
6. **Web Research Tools** - Guidelines for web information retrieval

## Skills

### Investigation Process

**When activated:** When investigating code, analyzing bugs, understanding existing implementations, exploring codebases, or debugging issues.

**What it does:** Provides methodology for transparent, real-time code investigation that enables collaboration and catches mistakes early.

### Design Alternatives

**When activated:** When making design decisions, choosing architecture patterns, selecting technologies, or evaluating implementation approaches.

**What it does:** Guides the process of proposing multiple alternatives with clear advantages, disadvantages, and future considerations.

### Root Cause Analysis

**When activated:** When handling errors, debugging unexpected behavior, investigating failures, fixing bugs, or analyzing system issues.

**What it does:** Enforces "investigate WHY before fixing" philosophy to prevent recurring issues and reduce technical debt.

### PR Description Format

**When activated:** When creating GitHub pull requests or writing PR descriptions.

**What it does:** Provides format guidelines for PR descriptions in Japanese with clear What, Why, and How sections.

### Test-Driven Approach

**When activated:** When implementing tests, practicing TDD, creating test cases, or planning test strategy.

**What it does:** References specific methodologies (t_wada's TDD, Kent Beck's TDD) and provides skeleton-first approach to test implementation.

### Web Research Tools

**When activated:** When performing web information retrieval, web scraping, browser automation, or documentation tasks.

**What it does:** Provides guidelines for using Playwright MCP and Obsidian tools effectively.

## Installation

```bash
/plugin install dev-guidelines@claude-plugins-private
```

## Usage

Skills are automatically activated based on the context of your work. You don't need to invoke them manually - they provide guidance when relevant to your current task.

## Author

kokuyouwind
