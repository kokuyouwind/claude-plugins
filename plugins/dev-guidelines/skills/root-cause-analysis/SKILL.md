---
name: Root Cause Analysis
description: Use this skill when handling errors, debugging unexpected behavior, investigating failures, fixing bugs, or analyzing system issues. Provides methodology for finding and fixing root causes.
version: 1.0.0
---

# Error Handling and Root Cause Analysis

## When to Use This Skill

This skill should be used when:
- Handling errors or exceptions
- Debugging unexpected behavior
- Investigating system failures
- Fixing bugs
- Analyzing why something went wrong
- Troubleshooting issues

## Error Handling Philosophy

### Investigate WHY Before Fixing

When unexpected behavior occurs:

1. **Don't rush to fix symptoms**
   - Resist the urge to apply quick patches
   - Surface-level solutions often don't solve the real problem
   - Band-aids can make problems worse later

2. **Investigate the root cause first**
   - Understand WHY the error is happening
   - Trace back to the original source
   - Identify the underlying issue

3. **Then implement proper countermeasures**
   - Fix the root cause, not symptoms
   - Prevent the issue from recurring
   - Improve system understanding

### Benefits of Root Cause Analysis

- **Prevents recurring issues**: Fix problems permanently
- **Improves system understanding**: Learn how things really work
- **Reduces technical debt**: Avoid accumulating workarounds
- **Builds better solutions**: Address fundamental issues
- **Saves time long-term**: Fewer repeat issues to debug

## Root Cause Analysis Process

### 1. Observe and Document

When an error occurs:
- **Record the symptoms**: What exactly is going wrong?
- **Collect evidence**: Error messages, logs, stack traces
- **Note the context**: When does it happen? What triggers it?
- **Document steps to reproduce**: How can we make it happen again?

### 2. Investigate the Chain of Causation

Ask "Why?" repeatedly (5 Whys technique):

```markdown
Problem: Application crashes when user clicks submit

Why? → Exception thrown in validation function
Why? → Validation function receives null value
Why? → Form data not properly initialized
Why? → Component mounted before API response
Why? → Race condition in async initialization

Root Cause: Race condition in async initialization
```

### 3. Trace Back to the Source

- Follow the error backward through the call stack
- Identify where the problem originates
- Distinguish between:
  - **Where the error manifests** (symptom location)
  - **Where the error originates** (root cause)

### 4. Understand the Mechanism

Before fixing, understand:
- **How** does the root cause lead to the observed error?
- **Why** does the current code behave this way?
- **What** assumptions or conditions are violated?

### 5. Design Proper Countermeasures

Based on understanding:
- **Fix the root cause**, not just symptoms
- **Prevent recurrence** through proper design
- **Consider edge cases** that might trigger similar issues
- **Add safeguards** where appropriate

## Common Anti-Patterns to Avoid

### ❌ Symptom-Based Fixes

```typescript
// Bad: Hiding the symptom
try {
  processData(data);
} catch (e) {
  // Just swallow the error
}

// Good: Understanding and fixing the cause
// First investigate: Why is processData throwing?
// Then fix the actual issue in data preparation
```

### ❌ Quick Patches Without Understanding

```typescript
// Bad: Adding null checks everywhere
if (value !== null && value !== undefined) {
  // Process value
}

// Good: Understanding why value is null
// Fix the source of null values
// Or document if null is valid and handle intentionally
```

### ❌ Assumption-Based Solutions

```typescript
// Bad: Guessing the fix
setTimeout(() => {
  // Maybe waiting will help?
  processData();
}, 1000);

// Good: Understanding the timing issue
// Properly handle async dependencies
// Use explicit synchronization
```

## Investigation Questions

When analyzing errors, ask:

1. **What changed recently?**
   - New code, configuration, dependencies?
   - Environment changes?

2. **What assumptions exist?**
   - What does the code assume about inputs?
   - Are those assumptions always valid?

3. **What are the preconditions?**
   - What must be true for the code to work?
   - Are preconditions guaranteed?

4. **What are the side effects?**
   - Does this code affect other parts of the system?
   - Could the error originate elsewhere?

5. **What does the data look like?**
   - What values are actually present?
   - Do they match expectations?

## Practical Example

### Scenario: User login fails intermittently

#### Surface-level approach (❌):
```typescript
// Just retry on failure
async function login(credentials) {
  try {
    return await api.login(credentials);
  } catch (error) {
    // Try again
    return await api.login(credentials);
  }
}
```

#### Root cause analysis approach (✅):

```markdown
1. Observe: Login fails ~10% of the time with "Invalid token"

2. Investigate:
   - Check logs: Token sometimes arrives before user data
   - Why? Parallel API calls with different latencies
   - Why parallel? Code makes two simultaneous requests
   - Why? Attempting to optimize performance

3. Root cause: Race condition between auth and user data APIs

4. Proper fix:
   - Serialize the API calls
   - Or handle partial data states properly
   - Add tests for race conditions
```

## Best Practices

### DO:
- Investigate before implementing fixes
- Understand the chain of causation
- Fix root causes, not symptoms
- Document your findings
- Add tests for the root cause
- Learn from each error

### DON'T:
- Rush to apply quick fixes
- Hide errors with try-catch
- Guess at solutions
- Fix symptoms without understanding
- Assume you know the cause without investigation
- Move on without learning

## Important Notes

- **Time invested in understanding pays off**
  - Proper fixes take less time than repeated symptom fixes
  - Understanding prevents future similar issues

- **System understanding compounds**
  - Each investigation teaches you about the system
  - Better understanding leads to better code

- **Documentation helps**
  - Write down what you learned
  - Help others avoid the same issues
  - Create institutional knowledge

- **Root causes can be surprising**
  - Don't assume you know without investigating
  - The obvious cause is often not the root cause
  - Stay curious and thorough
