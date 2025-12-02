---
name: Web Research Tools
description: Use this skill when performing web scraping, browser automation, or documentation tasks. Use Playwright MCP for web information retrieval and Obsidian MCP for note-taking.
version: 1.0.0
---

# Web Research and Documentation Tools

## Web Information Retrieval: Playwright MCP

For web scraping and browser automation:
- **Tool**: https://github.com/microsoft/playwright-mcp
- **Use for**: Navigating pages, interacting with elements, extracting data from dynamic websites, taking screenshots, handling authentication flows

## Documentation and Notes: Obsidian MCP

For knowledge management and documentation:
- **Tool**: https://github.com/jacksteamdev/obsidian-mcp-tools
- **Pre-configured** in the environment
- **Use for**: Organizing research findings, creating interconnected notes, building knowledge bases, markdown-based documentation

## Best Practices

### Choose the Right Tool
- **Playwright MCP** for web data retrieval
- **Obsidian** for documentation and notes
- **Combine both** for comprehensive research workflows (gather with Playwright → organize with Obsidian)

### Efficient Workflows
1. Use Playwright to gather information from web sources
2. Use Obsidian to organize and document findings
3. Create reproducible research processes

## Important Notes

- Always prefer Playwright MCP over custom web scraping solutions
- Leverage pre-configured Obsidian tools when available
- Consider data privacy and website terms of service
- Document your research process for reproducibility

## Required Tool Usage Rules

### GitHub Information Retrieval
**Always use gh command when retrieving information from GitHub:**
- Pull Request information: `gh pr view`, `gh pr list`
- Issue information: `gh issue view`, `gh issue list`
- Repository information: `gh repo view`
- GitHub Actions information: `gh run list`, `gh run view`
- **Prohibited:** Do not use WebFetch or WebSearch for GitHub information

### Application Behavior Verification
**Always use Playwright for web application behavior verification:**
- UI interaction testing: Use `mcp__playwright__browser_*` tool suite
- Screen screenshots: `mcp__playwright__browser_take_screenshot`
- Element operations: `mcp__playwright__browser_click`, `mcp__playwright__browser_type`
- Page navigation verification: `mcp__playwright__browser_navigate`
- **Prohibited:** Do not use curl or WebFetch for application behavior verification
