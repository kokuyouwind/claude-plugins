---
description: コード風プロンプト例5c Goのgoroutine:バッファ付きチャネル
argument-hint: ""
---

Emulate the following Go-style code internally (without using external tools or interpreter). Output only what fmt.Println() commands would output. Do not show any explanations, code, variables, or other messages.

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
