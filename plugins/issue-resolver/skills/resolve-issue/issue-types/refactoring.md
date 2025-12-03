# Refactoring Procedure

Follow these steps to perform refactoring:

## 1. Identify Refactoring Target

- Read the issue to understand what needs to be refactored
- Review the current code structure
- Identify areas for improvement

## 2. Plan Refactoring

- Define refactoring objectives
- Ensure the refactoring maintains existing functionality
- Consider the scope and impact of changes
- Keep changes focused and minimal

## 3. Implement Refactoring

- Refactor code while preserving behavior
- Improve code structure, readability, or maintainability
- Follow existing code patterns and conventions
- Avoid adding new features during refactoring
- Make incremental changes when possible

## 4. Create Pull Request

- Use `gh pr create` to create the PR
- Write PR description in Japanese
- Include:
  - Refactoring objectives and rationale
  - Summary of changes made
  - Confirmation that functionality is preserved
- Reference the issue in the PR description with "Closes #<issue-number>"
