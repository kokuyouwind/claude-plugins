# type:refactoring - Code Refactoring

## Process

### 1. Understand Target Code

- Read and understand the refactoring target
- Identify problems and improvement opportunities
- Verify existing tests can guarantee behavior after refactoring

### 2. Plan Approach

Use **`dev-guidelines:design-alternatives`** skill when multiple approaches exist:
- Clarify refactoring goal (readability, maintainability, performance, etc.)
- Evaluate impact of each approach
- Align with existing codebase patterns/style
- Plan incremental steps for large refactoring

### 3. Implement

Follow **`dev-guidelines:git-repository-workflow`** skill:
- Don't change external behavior (API, interfaces)
- Verify all existing tests pass
- Add/improve tests as needed
- Split commits into logical units for easier review

### 4. Create PR

Use **`dev-guidelines:pr-description-format`** skill. Include:
- Refactoring purpose
- Change summary
- Confirmation of no functional changes
- Before/After comparison (if applicable)

Commit message format: `refactor: <brief description>`

## Guidelines

- Separate refactoring from feature additions (don't do both at once)
- Split large refactoring into smaller incremental steps
- Prioritize preserving existing behavior
- Report bugs found during refactoring as separate Issues
- Take benchmarks for performance-impacting changes
