# type:feature - New Feature

## Process

### 1. Understand Requirements

- Review feature requirements in the Issue
- Clarify expected behavior and I/O
- Ask Issue author if anything is unclear

### 2. Plan Implementation

Use **`dev-guidelines:design-alternatives`** skill when multiple approaches exist:
- Check existing codebase architecture
- Evaluate pros/cons of each approach
- Present options to Issue author if needed
- Leverage existing patterns and libraries

### 3. Implement

Follow **`dev-guidelines:implementation-workflow`** skill:
- Follow coding conventions
- Reuse existing components/utilities
- Implement proper error handling
- Add tests as needed

### 4. Create PR

Use **`dev-guidelines:pr-description-format`** skill. Include:
- Feature overview
- Implementation approach
- Usage/configuration (if applicable)
- Test method (if applicable)

Commit message format: `feat: <brief description>`

## Guidelines

- Avoid over-complexity; keep it simple
- Don't over-abstract for hypothetical future needs
- Watch for scope creep
- Minimize new dependencies
