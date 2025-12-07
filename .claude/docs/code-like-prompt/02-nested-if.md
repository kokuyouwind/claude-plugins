# 02-nested-if Specification

## Concept

Test Claude's ability to correctly interpret nested conditional statements, focusing on:
1. Indentation-based scope determination (dangling else problem)
2. Complex deep nesting with:
   - if-else at various levels
   - else branches containing nested if statements
   - Dangling else patterns mixed in

## Test Scenarios

### 02a: Dangling Else - Else belongs to outer if

Test Python-style indentation where else is at outer level.

```python
if condition_a:
    if condition_b:
        print("foo")
else:
    print("bar")
```

Expected behavior:
- `A=T, B=T` → "foo"
- `A=T, B=F` → (no output)
- `A=F, B=*` → "bar"

### 02b: Dangling Else - Else belongs to inner if

Test Python-style indentation where else is at inner level.

```python
if condition_a:
    if condition_b:
        print("foo")
    else:
        print("bar")
```

Expected behavior:
- `A=T, B=T` → "foo"
- `A=T, B=F` → "bar"
- `A=F, B=*` → (no output)

### 02c: Complex Deep Nesting (5 levels)

Test comprehension of complex nested structures including:
- Else branches with nested if inside
- Dangling else patterns
- Mixed nesting depths

```python
if level1:
    if level2:
        if level3:
            print("foo")
        else:
            if level4:
                print("bar")
            else:
                print("baz")
    else:
        if level3:
            if level4:
                print("qux")
        else:
            print("quux")
else:
    if level2:
        print("corge")
    else:
        if level3:
            print("grault")
        else:
            print("garply")
```

Structure breakdown:
- Level 1 true branch: nested if-else with else containing another if-else
- Level 1 true → Level 2 false: Dangling else pattern (level3 if has no else)
- Level 1 false branch: else containing if-else, with further nesting in false branch

## Command Design

### Arguments

- `--condition-a`, `--condition-b`: Boolean values for dangling else tests
- `--level1` through `--level4`: Boolean values for deep nesting test

### Variants

Each test scenario has three syntax style variants:

**Indentation-based (Python-style)**:
- Uses indentation to define scope
- No explicit block delimiters

**Block-style (C/Java/JavaScript-style)**:
- Uses braces `{}` to define scope
- Explicit block boundaries

**Keyword-style (Ruby-style)**:
- Uses `if`/`end` keywords to define scope
- Explicit keyword boundaries

| Command | Focus | Syntax Style |
|---------|-------|--------------|
| `02a-dangling-else-outer-indent` | Else at outer indentation level | Indentation |
| `02a-dangling-else-outer-block` | Else at outer indentation level | Block braces |
| `02a-dangling-else-outer-keyword` | Else at outer indentation level | Keywords |
| `02b-dangling-else-inner-indent` | Else at inner indentation level | Indentation |
| `02b-dangling-else-inner-block` | Else at inner indentation level | Block braces |
| `02b-dangling-else-inner-keyword` | Else at inner indentation level | Keywords |
| `02c-deep-nesting-indent` | Complex 5-level nesting with mixed patterns | Indentation |
| `02c-deep-nesting-block` | Complex 5-level nesting with mixed patterns | Block braces |
| `02c-deep-nesting-keyword` | Complex 5-level nesting with mixed patterns | Keywords |

## Expected Behaviors

1. **Indentation recognition**: Claude should correctly interpret which if the else belongs to based on indentation
2. **Else with nested if**: Should correctly handle if statements inside else branches
3. **Mixed patterns**: Should correctly track context through various nesting patterns

## Test Cases

### Dangling Else Test Matrix

**02a (outer else)**:
| A | B | Output |
|---|---|--------|
| T | T | foo |
| T | F | (none) |
| F | T | bar |
| F | F | bar |

**02b (inner else)**:
| A | B | Output |
|---|---|--------|
| T | T | foo |
| T | F | bar |
| F | T | (none) |
| F | F | (none) |

### Complex Deep Nesting Test Matrix

| L1 | L2 | L3 | L4 | Output |
|----|----|----|-------|--------|
| T  | T  | T  | -  | foo |
| T  | T  | F  | T  | bar |
| T  | T  | F  | F  | baz |
| T  | F  | T  | T  | qux |
| T  | F  | T  | F  | (none) - Dangling else |
| T  | F  | F  | -  | quux |
| F  | T  | -  | -  | corge |
| F  | F  | T  | -  | grault |
| F  | F  | F  | -  | garply |

## Implementation Priority

1. `02c-deep-nesting` - Most interesting test with mixed patterns
2. `02a-dangling-else-outer` - Tests indentation-based else binding
3. `02b-dangling-else-inner` - Contrast to 02a

## Test Results

### 2025-12-07 (Claude Sonnet 4.5) - Indentation-based variants

The following test results are for the indentation-based syntax variants. Block-style and keyword-style variants have not yet been tested.

#### 02a-dangling-else-outer-indent
Tests whether Claude correctly interprets else at outer indentation level using indentation-based syntax.

| A | B | Expected | Actual | Pass |
|---|---|----------|--------|------|
| T | T | foo | foo | ✓ |
| T | F | (none) | bar | ✗ |
| F | T | bar | bar | ✓ |
| F | F | bar | bar | ✓ |

**Pass Rate: 3/4 (75%)**

**Analysis**: Claude incorrectly binds else to the inner if when A=T, B=F. Expected no output, but produced "bar", indicating Claude interprets the else as belonging to `if condition_b` instead of `if condition_a`.

#### 02b-dangling-else-inner-indent
Tests whether Claude correctly interprets else at inner indentation level using indentation-based syntax.

| A | B | Expected | Actual | Pass |
|---|---|----------|--------|------|
| T | T | foo | foo | ✓ |
| T | F | bar | bar | ✓ |
| F | T | (none) | (none) | ✓ |
| F | F | (none) | (none) | ✓ |

**Pass Rate: 4/4 (100%)**

**Analysis**: Claude correctly interprets all cases where else belongs to the inner if. This pattern aligns with Claude's natural interpretation tendency.

#### 02c-deep-nesting-indent
Tests complex 5-level nesting with mixed patterns using indentation-based syntax.

| L1 | L2 | L3 | L4 | Expected | Actual | Pass |
|----|----|----|-------|----------|--------|------|
| T  | T  | T  | -  | foo | foo | ✓ |
| T  | T  | F  | T  | bar | bar | ✓ |
| T  | T  | F  | F  | baz | bar | ✗ |
| T  | F  | T  | T  | qux | qux | ✓ |
| T  | F  | T  | F  | (none) | qux | ✗ |
| T  | F  | F  | -  | quux | quux | ✓ |
| F  | T  | -  | -  | corge | corge | ✓ |
| F  | F  | T  | -  | grault | grault | ✓ |
| F  | F  | F  | -  | garply | garply | ✓ |

**Pass Rate: 7/9 (78%)**

**Analysis**:
- Failed at L1=T,L2=T,L3=F,L4=F: Expected "baz", got "bar". Claude appears to skip the L4=F branch.
- Failed at L1=T,L2=F,L3=T,L4=F: Expected no output (dangling else), got "qux". Claude incorrectly executes the L4=T branch.
- Both failures involve interpreting nested structures with dangling else patterns.

### Summary

**Overall Pass Rate: 14/17 (82%)**

**Key Findings**:
1. Claude handles simple else-at-inner-level patterns perfectly (100%)
2. Claude struggles with else-at-outer-level patterns (75% pass rate)
3. Complex deep nesting reveals additional interpretation challenges (78% pass rate)
4. Main limitation: Indentation-based scope determination, especially with dangling else patterns

**Conclusion**: Claude's interpretation bias tends toward binding else to the nearest (inner) if statement, which causes failures when else should belong to outer scopes.

## Testing Different Syntax Styles

The addition of block-style and keyword-style variants allows for comparative testing to determine whether Claude's difficulties are specific to indentation-based syntax or reflect fundamental scope interpretation challenges.

### Testing Methodology

For each syntax style (indent, block, keyword), run the same test matrices:
- 02a variants: 4 test cases (A×B combinations)
- 02b variants: 4 test cases (A×B combinations)
- 02c variants: 9 test cases (L1-L4 combinations)

Use non-interactive testing mode:
```bash
claude -p "/code-like-prompt:02a-dangling-else-outer-block --condition-a=true --condition-b=true"
```

### Expected Research Outcomes

1. **If block-style and keyword-style both show 100% pass rates**: The issue is specific to indentation parsing, not scope understanding
2. **If all three styles show similar failure patterns**: The issue is fundamental scope interpretation, not syntax-specific
3. **If keyword-style performs better than block-style**: Claude may have better understanding of explicit keyword boundaries
4. **If block-style performs better than keyword-style**: Brace-based delimiters may be clearer for scope determination

### Next Steps

After testing all variants:
1. Document pass rates for each syntax style
2. Compare failure patterns across styles
3. Update this specification with comparative analysis
4. Use findings to guide future code-like prompt design decisions
