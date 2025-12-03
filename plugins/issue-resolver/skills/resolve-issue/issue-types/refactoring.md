# type:refactoring - Refactoring Procedures

## Overview

Detailed procedures for improving code structure and quality while maintaining existing functionality.

## Implementation Steps

### 1. Review code to be refactored

- Read and understand the target code
- Identify current problems and improvement opportunities
- Review existing tests to ensure same behavior after refactoring

### 2. Plan improvement approach

- Clarify refactoring purpose (readability, maintainability, performance, etc.)
- Consider impact of different approaches if multiple exist
- Align with existing codebase patterns and style
- For large refactorings, plan to proceed incrementally

### 3. Clean up code while maintaining existing functionality

- Don't change externally visible behavior (API, interfaces)
- Verify all existing tests pass
- Add or improve tests as needed
- Separate commits into logical units for easier code review

### 4. Create refactoring PR

- Use commit message format: `refactor: <concise target description>`
- Include in PR description:
  - Refactoring purpose
  - Overview of changes
  - How to verify no functional changes
  - Before/After comparison (if applicable)

## Notes

- Separate refactoring from feature additions (don't do simultaneously)
- Break large refactorings into small units and implement incrementally
- Prioritize not changing existing behavior
- Report bugs found during refactoring as separate Issues
- Benchmark performance-impacting changes
