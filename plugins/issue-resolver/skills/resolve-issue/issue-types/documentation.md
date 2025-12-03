# type:documentation - Documentation Update Procedures

## Overview

Detailed procedures for updating documentation.

## Implementation Steps

### 1. Identify documentation to update

- Determine which documentation needs updating
- Review current documentation content and identify issues
- Understand target audience (users, developers, operators, etc.)

### 2. Gather accurate information

- Check source code for accurate information
- Verify actual behavior (when possible)
- Collect background information from related Issues and PRs
- Verify consistency with existing documentation

### 3. Update documentation

- Use clear, concise expressions
- Write at appropriate detail level for target audience
- Include code examples or screenshots as appropriate
- Use markdown formatting properly
- Check for broken links

### 4. Verify readability and accuracy

- Check for typos and grammatical errors
- Verify technical accuracy
- Ensure natural flow of text
- Add links to other documentation as needed

### 5. Create documentation update PR

- Use commit message format: `docs: <concise update description>`
- Include in PR description:
  - Update purpose
  - Main changes
  - Before/After comparison (if applicable)

## Notes

- Always keep documentation synchronized with latest implementation
- Explain technical terms as needed
- For internationalization, update both English and Japanese
- Pay special attention to critical documentation like README and tutorials
- If you find code issues while updating documentation, report as separate Issue
