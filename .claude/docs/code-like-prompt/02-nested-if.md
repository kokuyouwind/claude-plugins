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

| Command | Focus |
|---------|-------|
| `02a-dangling-else-outer` | Else at outer indentation level |
| `02b-dangling-else-inner` | Else at inner indentation level |
| `02c-deep-nesting` | Complex 5-level nesting with mixed patterns |

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
