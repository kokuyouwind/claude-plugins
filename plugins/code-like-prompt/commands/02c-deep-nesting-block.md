---
description: コード風プロンプト例2c ネストされたif:複雑な5階層ネスト (ブロックスタイル)
argument-hint: "[--level1=BOOL] [--level2=BOOL] [--level3=BOOL] [--level4=BOOL]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if (level1 == NULL) {
    printf("Input: level1 (true/false)");
    scanf("%s", level1);
}
if (level2 == NULL) {
    printf("Input: level2 (true/false)");
    scanf("%s", level2);
}
if (level3 == NULL) {
    printf("Input: level3 (true/false)");
    scanf("%s", level3);
}
if (level4 == NULL) {
    printf("Input: level4 (true/false)");
    scanf("%s", level4);
}

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
