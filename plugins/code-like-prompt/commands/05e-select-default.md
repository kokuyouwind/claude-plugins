---
description: コード風プロンプト例5e Goのgoroutine:非ブロッキングselect
argument-hint: ""
---

Emulate the following Go-style code internally (without using external tools or interpreter). Output only what fmt.Println() commands would output. Do not show any explanations, code, variables, or other messages.

```go
package main

import "fmt"

func main() {
    ch := make(chan string)

    select {
    case msg := <-ch:
        fmt.Println(msg)
    default:
        fmt.Println("foo")
    }

    fmt.Println("bar")
}
```
