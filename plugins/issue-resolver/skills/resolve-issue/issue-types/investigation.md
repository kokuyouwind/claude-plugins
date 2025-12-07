# type:investigation - Investigation Task

## Process

### 1. Clarify Investigation Scope

- Review investigation purpose in the Issue
- Clarify what to investigate and what information is needed
- Identify scope (files, directories, system components, etc.)

### 2. Investigate

Use **`dev-guidelines:debugging-process`** skill for systematic investigation:
- Read related source code
- Understand background from commit/PR history
- Check existing documentation and comments
- Reference external resources (library docs, etc.) as needed

### 3. Organize Findings

- Structure discovered facts
- Highlight important findings
- Use diagrams/tables for visualization as needed
- Summarize conclusions and recommendations

### 4. Post Results to Issue

Post investigation results as Issue comment with clear, readable format:

```
## 調査概要
[What was investigated]

## 発見事項
[Key findings]

## 詳細
[Specific investigation results]

## 結論
[Conclusions and recommendations]
```

- Include file path and line numbers for code references
- Keep it concise; detail can go in separate documents if needed

## Guidelines

- Investigation tasks typically don't require code changes or PRs
- Propose creating new Issues if findings warrant follow-up work
- Clearly distinguish facts from speculation
- Report minor issues found during investigation as separate Issues
