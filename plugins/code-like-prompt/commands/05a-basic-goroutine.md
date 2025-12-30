---
description: コード風プロンプト例5a Goのgoroutine:基本的な並列実行
argument-hint: ""
---

Emulate the following Go-style code internally (without using external tools or interpreter). Output only what fmt.Println() commands would output. Do not show any explanations, code, variables, or other messages.

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
