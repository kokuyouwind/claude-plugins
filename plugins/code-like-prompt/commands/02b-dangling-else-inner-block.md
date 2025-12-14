---
description: コード風プロンプト例2b ネストされたif:elseが内側に属する (ブロックスタイル)
argument-hint: '{"condition_a": boolean, "condition_b": boolean}'
---

Execute the following code with environment: $ARGUMENTS

Output only what printf() commands specify. Do not show any explanations, code, variables, or other messages.

```c
// Validate required arguments
if (condition_a == NULL) {
    fprintf(stderr, "Required argument 'condition_a' is missing");
    exit(1);
}
if (condition_b == NULL) {
    fprintf(stderr, "Required argument 'condition_b' is missing");
    exit(1);
}

// Logic
if (condition_a) {
    if (condition_b) {
        printf("foo");
    } else {
        printf("bar");
    }
}
```
