# code-like-prompt Plugin - Overview

## Purpose

This plugin serves as an experimental testbed to verify whether Claude correctly interprets prompts written in code-like syntax using various programming language features.

## Goals

1. **Verify interpretation accuracy**: Test if Claude can correctly interpret and execute code-like prompts
2. **Explore language paradigms**: Experiment with different programming paradigms (imperative, functional, logic programming, concurrent)
3. **Document behaviors**: Record how Claude handles edge cases, ambiguities, and complex control flows
4. **Educational value**: Provide interesting examples that demonstrate programming concepts

## Design Principles

### Prompt Structure

Each command follows a consistent structure:
1. **Minimal instruction prefix**: A brief English instruction explaining what Claude should do
2. **Code-like prompt**: The main prompt written in code-like syntax
3. **Output constraints**: Clear specification of expected output format

Example from 01-shopping-request:
```
Execute the following code. Output only what puts() commands specify. Do not show any explanations, code, variables, or other messages.

```ruby
if Milk.stock.nil?
  puts("Input: Milk")
  Milk.stock = gets
end
...
```

### Command Naming Convention

Commands are numbered for organization:
- `01-*`: Basic conditionals (if/else)
- `02-*`: Nested conditionals
- `03-*`: Loop constructs
- `04-*`: Pattern matching (multiple experiments)
- `05-*`: Concurrent/parallel constructs

### Testing Philosophy

- Each concept may have multiple variant commands to test different interpretations
- Arguments allow testing with different inputs without modifying the prompt
- Results should be documented to track Claude's behavior across versions

## Planned Experiments

| ID | Name | Concept | Status |
|----|------|---------|--------|
| 01 | shopping-request/misunderstanding | Basic if/else | Done |
| 02a | dangling-else-outer | Else at outer indentation level | Planned |
| 02b | dangling-else-inner | Else at inner indentation level | Planned |
| 02c | deep-nesting | Complex 5-level nesting with mixed patterns | Planned |
| 03a-j | loop variants | For/while/each/break/continue/nested/accumulator | Planned |
| 04a-f | pattern-match variants | Regex, structural, list, nested, guards | Planned |
| 04p-a-g | prolog-backtrack variants | Facts, multi-clause, cut, tree, findall, negation, constraints | Planned |
| 05a-k | golang-goroutine variants | Goroutines, channels, select, workers, pipeline, subagents | Planned |

## Documentation Structure

```
.claude/docs/code-like-prompt/
├── overview.md             # This file
├── 02-nested-if.md         # Nested if specification (dangling else, deep nesting)
├── 03-loop.md              # Loop specification (10 variants)
├── 04-pattern-match.md     # Pattern matching specification (6 variants)
├── 04-prolog-backtrack.md  # Prolog backtracking specification (7 variants)
└── 05-golang-goroutine.md  # Goroutine specification (11 variants)
```

## Implementation Guidelines

1. Keep prompts self-contained and focused on a single concept
2. Use familiar syntax from popular languages (Ruby, Python, Go, Prolog)
3. Include both "happy path" and edge case testing scenarios
4. Document expected vs actual behavior for each experiment
5. Consider adding variant commands that test different interpretations

## Testing Guidelines

### Command Testing Method

**CRITICAL**: When testing code-like-prompt commands, always use `claude` command's non-interactive mode instead of SlashCommand tool.

**Why**: Using SlashCommand tool directly in a session causes command expansion and returns control to the user, preventing continuous automated testing.

**Correct method**:
```bash
claude -p "/code-like-prompt:02a-dangling-else-outer --condition-a=true --condition-b=true"
```

This allows continuous automated testing of multiple patterns without interruption.

### Testing Workflow

1. **Update plugin**: Ensure the plugin is updated and session is restarted
2. **Run tests systematically**: Use `claude -p` to test all variants with different argument combinations
3. **Verify output**: Compare actual output against expected behavior in specification docs
4. **Document results**: Record any deviations or unexpected behaviors

### Test Purpose and Interpretation

**IMPORTANT**: The purpose of code-like-prompt tests is to verify whether Claude can correctly interpret code-like prompts, not to ensure all tests pass.

**Key Points**:
- These tests evaluate Claude's interpretation capabilities
- If command specifications and implementations are correct but Claude interprets them incorrectly, this is an **expected and acceptable test outcome**
- Failed test cases are valuable data points that reveal Claude's current limitations in understanding certain code patterns
- Examples of challenging patterns: indentation-based scope determination, dangling else problem, complex nested structures
- All test results (both passes and failures) should be documented to track Claude's interpretation behavior across versions
