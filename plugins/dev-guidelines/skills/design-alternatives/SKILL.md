---
name: Design Alternatives
description: Use this skill when choosing architectures, selecting technologies, evaluating implementation approaches, or making design decisions. Propose 2-3 alternatives with advantages/disadvantages/reasoning/future scenarios for each.
version: 1.0.0
---

# Design and Architecture Decision Process

## Core Principle: Multiple Alternatives with Trade-offs

When facing design decisions, **always propose at least 2-3 alternatives** and analyze each comprehensively.

## For Each Alternative, Clearly State

### 1. Advantages
What are the benefits? What problems does it solve well?

### 2. Disadvantages
What are the drawbacks, limitations, or complexities?

### 3. Reasoning
Why would this be a good or poor choice? What factors support or oppose it?

### 4. Future Scenarios
- When might this design break down?
- What future requirements might challenge this approach?
- How does this scale or adapt to changes?

## Example Structure

```markdown
## Option 1: [Approach Name]

**Advantages:**
- [Benefit 1]
- [Benefit 2]

**Disadvantages:**
- [Drawback 1]
- [Drawback 2]

**Reasoning:**
[Why this makes sense or doesn't]

**Future Scenarios:**
- Breaks down when [scenario]
- Needs changes if [future requirement]

## Option 2: [Alternative Approach]
[Same structure]

## Option 3: [Another Alternative]
[Same structure]

## Recommendation
[Based on current requirements and future considerations]
```

## Benefits of This Approach

- Expands thinking beyond first obvious solution
- Identifies potential issues early
- Enables informed decision-making
- Documents reasoning for future reference
- Helps stakeholders understand trade-offs

## Best Practices

### DO:
- Think broadly before settling on a solution
- Consider long-term implications, not just immediate needs
- Make implicit costs and trade-offs explicit
- Document the decision-making process

### DON'T:
- Rush to the first solution
- Present only one option as "the best"
- Ignore future scalability or maintainability
- Make decisions without exploring alternatives

## Important Note

There is rarely a single "perfect" solution. Different contexts favor different approaches, and future requirements can change the optimal choice.
