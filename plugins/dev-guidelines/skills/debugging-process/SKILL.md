---
name: Debugging Process
description: Use this skill when investigating code, analyzing bugs, debugging issues, tracing errors, performing root cause analysis, or understanding unexpected behavior. Report findings in real-time with file:line references and investigate WHY before applying fixes.
version: 1.0.0
---

# Debugging and Root Cause Analysis

## Core Principles

### 1. Real-Time Investigation Reporting

**Communicate as you investigate**:
- Share findings immediately with `file_path:line_number` references
- Explain your reasoning and thought process
- Let the user verify your understanding
- Report "I'm checking [X] at [location]" → "Found [Y], suggests [Z]"

### 2. Root Cause Before Fixes

**Never apply quick fixes without understanding**:
- Ask "Why?" repeatedly (5 Whys technique) to find root causes
- Distinguish between where errors manifest vs. where they originate
- Fix the source, not symptoms
- Understand the mechanism before implementing countermeasures

## Investigation Pattern

```markdown
1. Starting point: "Investigating [X] by checking [Y]"
2. Evidence: "Found [this] at [location:line]. Suggests [interpretation]"
3. Following leads: "Based on this, checking [next location]"
4. Root cause: "After tracing back: [root cause explanation]"
5. Solution: "Fixing [root cause] by [approach]"
```

## 5 Whys Example

```
Problem: Application crashes on submit button click

Why? → Exception in validation function
Why? → Validation receives null value
Why? → Form data not initialized
Why? → Component mounts before API response
Why? → Race condition in async initialization

Root Cause: Race condition → Fix async initialization order
```

## Anti-Patterns to Avoid

❌ **Symptom-based fixes**: Adding try-catch without understanding why it throws
❌ **Null-checks everywhere**: Instead of understanding why values are null
❌ **Guessing solutions**: Adding setTimeout hoping it helps
❌ **Hidden investigation**: Presenting only conclusions without evidence

## Best Practices

### DO:
- Use `file:line` format for all code references
- Share evidence progressively as you discover it
- Trace errors back to their origin
- Test your understanding by explaining the causal chain
- Document why the fix addresses the root cause

### DON'T:
- Rush to implement fixes
- Hide your investigation process
- Fix symptoms without understanding causes
- Make assumptions without verification
- Skip explaining the "why"

## Integration with Other Skills

- Works with **implementation-workflow** when fixing bugs (checkout branch, then investigate)
- Complements **test-driven-approach** (understand bug → write failing test → fix → verify)
