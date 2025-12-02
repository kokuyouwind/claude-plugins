# Feature Addition Procedure

## Overview

This document provides the procedure for adding new features to the codebase.

## Resolution Steps

### 1. Understand Requirements
- Read the feature request carefully
- Identify the core functionality required
- Clarify any ambiguous requirements
- Understand the use case and user needs

### 2. Plan Implementation
- Review existing codebase patterns and architecture
- Consider multiple implementation approaches if applicable
- Choose the approach that best fits the existing codebase
- Identify which files and components will be affected

### 3. Design the Feature
- Plan the feature structure following existing patterns
- Consider integration points with existing code
- Think about edge cases and error handling
- Keep the design simple and focused

### 4. Implement the Feature
- Follow the planned implementation approach
- Write code that matches existing style and patterns
- Implement only the requested functionality
- Avoid adding extra features or abstractions

### 5. Create Pull Request
- Use `gh pr create` command to create the PR
- Title the PR clearly (e.g., "Feature: [brief description]")
- Include in PR description (in Japanese):
  - 概要: What feature was added
  - 変更の背景: Why this feature is needed
  - 変更詳細: How the feature works
- Reference the original issue number

## Important Notes

- Stick to the requested feature scope
- Follow existing architectural patterns
- Don't over-engineer or add unnecessary complexity
- Ensure the feature integrates well with existing code
- Consider backward compatibility if applicable
