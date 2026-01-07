---
description: コード風プロンプト例2c ネストされたif:複雑な5階層ネスト (ブロックスタイル)
argument-hint: '{"level1": boolean, "level2": boolean, "level3": boolean, "level4": boolean}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what printf() commands would output. Do not show any explanations, code, variables, or other messages. If there is nothing to output, output () instead.

```c
// Validate required arguments
if (level1 == NULL) {
    fprintf(stderr, "Required argument 'level1' is missing");
    exit(1);
}
if (level2 == NULL) {
    fprintf(stderr, "Required argument 'level2' is missing");
    exit(1);
}
if (level3 == NULL) {
    fprintf(stderr, "Required argument 'level3' is missing");
    exit(1);
}
if (level4 == NULL) {
    fprintf(stderr, "Required argument 'level4' is missing");
    exit(1);
}

// Logic
if (level1) {
    if (level2) {
        if (level3) {
            printf("foo");
        } else {
            if (level4) {
                printf("bar");
            } else {
                printf("baz");
            }
        }
    } else {
        if (level3) {
            if (level4) {
                printf("qux");
            }
        } else {
            printf("quux");
        }
    }
} else {
    if (level2) {
        printf("corge");
    } else {
        if (level3) {
            printf("grault");
        } else {
            printf("garply");
        }
    }
}
```
