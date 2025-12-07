---
description: コード風プロンプト例5a Goのgoroutine:基本的な並列実行
argument-hint: ""
---

Execute the following Go-style code. Output only what fmt.Println() commands specify. Do not show any explanations, code, variables, or other messages.

```go
package main

import (
    "fmt"
    "sync"
)

func main() {
    var wg sync.WaitGroup

    wg.Add(2)
    go func() {
        defer wg.Done()
        fmt.Println("foo")
    }()
    go func() {
        defer wg.Done()
        fmt.Println("bar")
    }()

    wg.Wait()
    fmt.Println("baz")
}
```
