---
description: コード風プロンプト例5c Goのgoroutine:バッファ付きチャネル
argument-hint: ""
---

Execute the following Go-style code. Output only what fmt.Println() commands specify. Do not show any explanations, code, variables, or other messages.

```go
package main

import "fmt"

func main() {
    ch := make(chan string, 2)

    ch <- "foo"
    ch <- "bar"
    // ch <- "baz"  // Would block if uncommented

    fmt.Println(<-ch)
    fmt.Println(<-ch)
    fmt.Println("qux")
}
```
