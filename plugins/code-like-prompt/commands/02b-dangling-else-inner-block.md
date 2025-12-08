---
description: コード風プロンプト例2b ネストされたif:elseが内側に属する (ブロックスタイル)
argument-hint: "[--condition-a=BOOL] [--condition-b=BOOL]"
---

Execute the following code. Output only what print() commands specify. Do not show any explanations, code, variables, or other messages.

```
if (condition_a == NULL) {
    printf("Input: condition_a (true/false)");
    scanf("%s", condition_a);
}
if (condition_b == NULL) {
    printf("Input: condition_b (true/false)");
    scanf("%s", condition_b);
}

if (condition_a) {
    if (condition_b) {
        printf("foo");
    } else {
        printf("bar");
    }
}
```
