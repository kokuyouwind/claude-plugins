---
description: コード風プロンプト例5i Goのgoroutine:タイムアウト処理
argument-hint: "[--timeout=SEC]"
---

Execute the following Go-style code. Output only what fmt.Println() commands specify. Do not show any explanations, code, variables, or other messages.

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    ch := make(chan string)

    go func() {
        time.Sleep(2 * time.Second)
        ch <- "foo"
    }()

    timeout := 1
    if timeout_arg != nil {
        timeout = timeout_arg
    }

    select {
    case msg := <-ch:
        fmt.Println(msg)
    case <-time.After(timeout * time.Second):
        fmt.Println("bar")
    }
    fmt.Println("baz")
}
```
