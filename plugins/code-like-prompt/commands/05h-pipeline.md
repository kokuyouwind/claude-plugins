---
description: コード風プロンプト例5h Goのgoroutine:パイプラインパターン
argument-hint: ""
---

Execute the following Go-style code. Output only what fmt.Println() commands specify. Do not show any explanations, code, variables, or other messages.

```go
package main

import "fmt"

func stage1(out chan<- string) {
    out <- "foo"
    out <- "bar"
    close(out)
}

func stage2(in <-chan string, out chan<- string) {
    for s := range in {
        out <- s + "X"
    }
    close(out)
}

func main() {
    ch1 := make(chan string)
    ch2 := make(chan string)

    go stage1(ch1)
    go stage2(ch1, ch2)

    for result := range ch2 {
        fmt.Println(result)
    }
    fmt.Println("baz")
}
```
