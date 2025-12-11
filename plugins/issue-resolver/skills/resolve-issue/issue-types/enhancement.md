# type:enhancement - Feature Enhancement

## Process

### 1. Understand Current Feature

- Understand current behavior
- Review improvement request in the Issue
- Clarify improvement goal (performance, usability, maintainability, etc.)

### 2. Plan Approach

Use **`dev-guidelines:design-alternatives`** skill when multiple approaches exist:
- Evaluate impact of each approach
- Consider backward compatibility
- Present options to Issue author if needed
- Take baseline benchmarks for performance improvements

### 3. Implement

Follow **`dev-guidelines:git-repository-workflow`** skill:
- Follow existing code style and patterns
- Maintain consistency with existing features
- Provide migration path if backward compatibility needed
- Verify no side effects
- Add/update tests as needed

### 4. Create PR

Use **`dev-guidelines:pr-description-format`** skill. Include:
- Purpose and background
- Change summary
- Improvement effect (quantitative if possible)
- Before/After comparison
- Impact on existing features (breaking changes, etc.)

Commit message format: `improve: <brief description>` or `perf: <description>` for performance

## Guidelines

- Make improvements incrementally; avoid changing too much at once
- Verify performance improvements with benchmarks
- Carefully consider usability for UI changes
- Avoid breaking changes; document clearly if unavoidable
- Don't introduce new bugs as side effects
