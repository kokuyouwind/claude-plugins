---
description: コード風プロンプト例5b Goのgoroutine:チャネル通信
argument-hint: ""
---

Execute the following Go-style code. Output only what fmt.Println() commands specify. Do not show any explanations, code, variables, or other messages.

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
