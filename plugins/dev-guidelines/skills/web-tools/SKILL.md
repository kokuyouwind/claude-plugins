---
name: web-tools
description: Specifies tool selection rules for web operations and documentation. Use this skill when performing web scraping, browser automation, GitHub information retrieval, or documentation tasks.
---

# Web Research Tools

## Instructions

### Tool Selection Rules

| Task | Tool | Prohibited |
|------|------|------------|
| Web scraping / Browser automation | Playwright MCP | curl, custom solutions |
| GitHub info (PR, Issue, Actions) | `gh` command | WebFetch, WebSearch |
| App behavior verification | Playwright MCP (`mcp__playwright__browser_*`) | curl, WebFetch |
| Documentation / Notes | Obsidian MCP | - |

### Workflow

1. Gather with Playwright → 2. Organize with Obsidian

## Examples

### GitHub Operations

```bash
gh pr view/list    # PR information
gh issue view/list # Issue information
gh repo view       # Repository information
gh run list/view   # GitHub Actions
```

### Playwright Operations

- Navigation: `mcp__playwright__browser_navigate`
- Interaction: `mcp__playwright__browser_click`, `mcp__playwright__browser_type`
- Screenshots: `mcp__playwright__browser_take_screenshot`
