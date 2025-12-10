---
description: コード風プロンプト例5e Goのgoroutine:非ブロッキングselect
argument-hint: ""
---

Execute the following Go-style code. Output only what fmt.Println() commands specify. Do not show any explanations, code, variables, or other messages.

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
