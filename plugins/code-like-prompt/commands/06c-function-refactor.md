---
description: コード風プロンプト例6c 複雑なネスト構造の関数化
argument-hint: '{"level1": boolean, "level2": boolean, "level3": boolean, "level4": boolean}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what print() commands would output. Do not show any explanations, code, variables, or other messages.

```python
# Validate required arguments
if level1 is None:
    raise ValueError("Required argument 'level1' is missing")
if level2 is None:
    raise ValueError("Required argument 'level2' is missing")
if level3 is None:
    raise ValueError("Required argument 'level3' is missing")
# Note: level4 is optional for some branches

# Function definitions
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
if level1:
    result = process_level1_true(level2, level3, level4)
else:
    result = process_level1_false(level2, level3)

if result is not None:
    print(result)
```
