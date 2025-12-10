---
description: コード風プロンプト例5d Goのgoroutine:selectステートメント
argument-hint: ""
---

Execute the following Go-style code. Output only what fmt.Println() commands specify. Do not show any explanations, code, variables, or other messages.

```go
package main

import "fmt"

func main() {
    ch1 := make(chan string, 1)
    ch2 := make(chan string, 1)

    ch1 <- "foo"
    ch2 <- "bar"

    for i := 0; i < 2; i++ {
        select {
        case msg := <-ch1:
            fmt.Println(msg)
        case msg := <-ch2:
            fmt.Println(msg)
        }
    }
    fmt.Println("baz")
}
```
