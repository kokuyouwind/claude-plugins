---
description: コード風プロンプト例5b Goのgoroutine:チャネル通信
argument-hint: ""
---

Emulate the following Go-style code internally (without using external tools or interpreter). Output only what fmt.Println() commands would output. Do not show any explanations, code, variables, or other messages.

```go
package main

import "fmt"

func main() {
    ch := make(chan string)

    go func() {
        ch <- "foo"
        ch <- "bar"
    }()

    fmt.Println(<-ch)
    fmt.Println(<-ch)
    fmt.Println("baz")
}
```
