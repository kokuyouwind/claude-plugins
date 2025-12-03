# type:feature - New Feature Addition Procedures

## Overview

Detailed procedures for adding new features.

## Implementation Steps

### 1. Understand the feature requirements

- Review feature requirements described in the Issue
- Clarify expected behavior and input/output
- Ask the Issue creator if anything is unclear

### 2. Plan implementation approach

- Review existing codebase architecture
- Consider pros/cons of different implementation methods if multiple exist
- Present implementation approach options to Issue creator if needed
- Check if existing patterns or libraries can be leveraged

### 3. Implement following existing codebase patterns

- Follow coding conventions
- Reuse existing components and utilities
- Implement appropriate error handling
- Add tests as necessary

### 4. Create implementation PR

- Use commit message format: `feat: <concise feature description>`
- Include in PR description:
  - Feature overview
  - Implementation approach explanation
  - Usage or configuration instructions (if applicable)
  - Test method (if applicable)

## Notes

- Avoid overly complex implementations; keep it simple
- Don't over-abstract for hypothetical future extensions
- Watch for scope creep (feature bloat)
- Minimize dependency additions
