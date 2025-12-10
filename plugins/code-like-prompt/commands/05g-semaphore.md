---
description: コード風プロンプト例5g Goのgoroutine:セマフォパターン
argument-hint: "[--tasks=NUM] [--max-concurrent=NUM]"
---

Execute the following Go-style code. Output only what fmt.Printf() and fmt.Println() commands specify. Do not show any explanations, code, variables, or other messages.

```go
package main

import (
    "fmt"
    "sync"
)

func main() {
    max_concurrent := 2
    if max_concurrent_arg != nil {
        max_concurrent = max_concurrent_arg
    }

    tasks := 4
    if tasks_arg != nil {
        tasks = tasks_arg
    }

    sem := make(chan struct{}, max_concurrent)  // Max concurrent
    var wg sync.WaitGroup

    for i := 0; i < tasks; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            sem <- struct{}{}
            fmt.Printf("foo%d\n", id)
            <-sem
        }(i)
    }

    wg.Wait()
    fmt.Println("bar")
}
```
