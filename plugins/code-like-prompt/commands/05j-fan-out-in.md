---
description: コード風プロンプト例5j Goのgoroutine:ファンアウト/ファンイン
argument-hint: "[--workers=NUM] [--input=INPUT]"
---

Emulate the following Go-style code internally (without using external tools or interpreter). Output only what fmt.Println() commands would output. Do not show any explanations, code, variables, or other messages.

```go
package main

import (
    "fmt"
    "sync"
)

func fanOut(input string, workers int) <-chan string {
    out := make(chan string)
    var wg sync.WaitGroup

    for i := 0; i < workers; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            out <- fmt.Sprintf("foo%d%s", id, input)
        }(i)
    }

    go func() {
        wg.Wait()
        close(out)
    }()

    return out
}

func main() {
    workers := 3
    if workers_arg != nil {
        workers = workers_arg
    }

    input := "X"
    if input_arg != nil {
        input = input_arg
    }

    results := fanOut(input, workers)

    for r := range results {
        fmt.Println(r)
    }
    fmt.Println("bar")
}
```
