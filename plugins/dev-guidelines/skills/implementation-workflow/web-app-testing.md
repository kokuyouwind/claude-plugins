# Web Application Testing

## Playwright MCP for E2E Testing

Use Playwright MCP for browser automation and E2E testing. Do NOT use curl or custom solutions.

### Available Operations

| Operation | Tool |
|-----------|------|
| Navigate | `mcp__playwright__browser_navigate` |
| Click | `mcp__playwright__browser_click` |
| Type | `mcp__playwright__browser_type` |
| Screenshot | `mcp__playwright__browser_take_screenshot` |

### When to Use

- E2E test scenarios
- UI behavior verification
- Visual regression testing
- Form submission testing
- User flow validation

### Example Flow

```
1. Navigate to page
2. Interact with elements (click, type)
3. Take screenshot for verification
4. Assert expected state
```

### DO / DON'T

**DO**: Use Playwright MCP for all browser interactions, take screenshots for debugging

**DON'T**: Use curl for page fetching, implement custom browser automation
