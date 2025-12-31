# 06-function Specification

## Concept

Test Claude's ability to correctly interpret function definition, calling, and return value evaluation in code-like prompts, focusing on:
1. Argument passing and parameter usage
2. Return value evaluation and assignment
3. Refactoring complex nested conditionals into functions
4. Function composition and call chains

## Test Scenarios

### 06a: Function with Arguments

Test function parameter passing and conditional logic based on arguments.

```python
def greet(name, language):
    if language == "ja":
        print(f"こんにちは、{name}さん")
    elif language == "en":
        print(f"Hello, {name}")
    else:
        print(f"Hi, {name}")

greet($name, $language)
```

Expected outputs:
- `name="Miku", language="ja"` → "こんにちは、Mikuさん"
- `name="Alice", language="en"` → "Hello, Alice"
- `name="Bob", language="fr"` → "Hi, Bob"

### 06b: Function Return Value

Test function return values, variable assignment, and using returned values in conditional expressions.

```python
def calculate_score(base, bonus):
    if bonus:
        return base * 2
    else:
        return base

score = calculate_score($base, $bonus)

if score >= 100:
    print("Excellent")
elif score >= 50:
    print("Good")
else:
    print("Try again")
```

Expected outputs:
- `base=60, bonus=true` → score=120 → "Excellent"
- `base=60, bonus=false` → score=60 → "Good"
- `base=30, bonus=false` → score=30 → "Try again"

### 06c: Refactoring Complex Conditionals

Test whether complex nested conditionals (like 02c-deep-nesting) produce the same output when refactored into functions.

This validates:
- Function call chains
- Consistent behavior between nested if-else and function decomposition
- Return value propagation through multiple function calls

```python
def process_level3_false(level4):
    if level4:
        return "bar"
    else:
        return "baz"

def process_level2_true(level3, level4):
    if level3:
        return "foo"
    else:
        return process_level3_false(level4)

def process_level2_false(level3, level4):
    if level3:
        if level4:
            return "qux"
        else:
            return None
    else:
        return "quux"

def process_level1_true(level2, level3, level4):
    if level2:
        return process_level2_true(level3, level4)
    else:
        return process_level2_false(level3, level4)

def process_level1_false(level2, level3):
    if level2:
        return "corge"
    else:
        if level3:
            return "grault"
        else:
            return "garply"

# Main logic
if $level1:
    result = process_level1_true($level2, $level3, $level4)
else:
    result = process_level1_false($level2, $level3)

if result is not None:
    print(result)
```

Expected: Same outputs as 02c-deep-nesting for identical input arguments

## Command Design

### Arguments

JSON environment arguments (vary by command):

**06a-function-args**:
- `name`: String - Person's name (e.g., "Miku")
- `language`: String - Language code ("ja", "en", or other)

**06b-function-return**:
- `base`: Number - Base score (e.g., 60)
- `bonus`: Boolean - Whether bonus is applied

**06c-function-refactor**:
- `level1`: Boolean - Top-level condition
- `level2`: Boolean - Second-level condition
- `level3`: Boolean - Third-level condition
- `level4`: Boolean - Fourth-level condition (optional for some branches)

### Variants

| Command | Focus |
|---------|-------|
| `06a-function-args` | Argument passing and parameter usage |
| `06b-function-return` | Return value evaluation |
| `06c-function-refactor` | Complex conditional refactoring |

## Expected Behaviors

1. **Function definition**: Claude should understand function definition syntax (def, return)
2. **Argument passing**: Arguments should be correctly passed and accessible in function scope
3. **Return values**: Functions should return values that can be assigned to variables
4. **Conditional on return**: Returned values should work in if/elif/else expressions
5. **Function composition**: Functions should be able to call other functions
6. **None handling**: None/null return values should be handled correctly

## Test Cases

### 06a: Function Arguments

| name | language | Expected Output |
|------|----------|----------------|
| "Miku" | "ja" | こんにちは、Mikuさん |
| "Alice" | "en" | Hello, Alice |
| "Bob" | "fr" | Hi, Bob |
| "Charlie" | "es" | Hi, Charlie |

### 06b: Function Return Value

| base | bonus | score | Expected Output |
|------|-------|-------|----------------|
| 60 | true | 120 | Excellent |
| 60 | false | 60 | Good |
| 30 | false | 30 | Try again |
| 50 | false | 50 | Good |
| 50 | true | 100 | Excellent |
| 40 | false | 40 | Try again |

### 06c: Function Refactor

Should match all 02c-deep-nesting test cases:

| level1 | level2 | level3 | level4 | Expected |
|--------|--------|--------|--------|----------|
| true | true | true | - | foo |
| true | true | false | true | bar |
| true | true | false | false | baz |
| true | false | true | true | qux |
| true | false | true | false | (none) |
| true | false | false | - | quux |
| false | true | - | - | corge |
| false | false | true | - | grault |
| false | false | false | - | garply |

## Implementation Priority

1. `06b-function-return` - Core functionality: return value evaluation
2. `06a-function-args` - Argument passing and string interpolation
3. `06c-function-refactor` - Complex test case validating consistency

## Test Commands

### 06a-function-args
```bash
claude -p '/code-like-prompt:06a-function-args {"name": "Miku", "language": "ja"}'
claude -p '/code-like-prompt:06a-function-args {"name": "Alice", "language": "en"}'
claude -p '/code-like-prompt:06a-function-args {"name": "Bob", "language": "fr"}'
```

### 06b-function-return
```bash
claude -p '/code-like-prompt:06b-function-return {"base": 60, "bonus": true}'
claude -p '/code-like-prompt:06b-function-return {"base": 60, "bonus": false}'
claude -p '/code-like-prompt:06b-function-return {"base": 30, "bonus": false}'
```

### 06c-function-refactor
```bash
claude -p '/code-like-prompt:06c-function-refactor {"level1": true, "level2": true, "level3": true}'
claude -p '/code-like-prompt:06c-function-refactor {"level1": true, "level2": true, "level3": false, "level4": true}'
claude -p '/code-like-prompt:06c-function-refactor {"level1": false, "level2": false, "level3": false}'
```

## Test Results

(To be updated after testing)
