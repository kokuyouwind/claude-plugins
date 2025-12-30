---
description: コード風プロンプト例5k Goのgoroutine:サブエージェント風並列処理
argument-hint: "[--topics=TOPICS]"
---

Emulate the following Go-style code internally (without using external tools or interpreter). Output only what fmt.Println() commands would output. Do not show any explanations, code, variables, or other messages.

```go
package main

import (
    "fmt"
    "sync"
)

func research(topic string) string {
    // Simulate research task
    return "result_" + topic
}

func main() {
    var wg sync.WaitGroup
    results := make(chan string, 3)

    topics := []string{"foo", "bar", "baz"}
    if topics_arg != nil {
        topics = strings.Split(topics_arg, ",")
    }

    for _, t := range topics {
        wg.Add(1)
        go func(topic string) {
            defer wg.Done()
            results <- research(topic)
        }(t)
    }

    go func() {
        wg.Wait()
        close(results)
    }()

    for r := range results {
        fmt.Println(r)
    }
    fmt.Println("qux")
}
```
