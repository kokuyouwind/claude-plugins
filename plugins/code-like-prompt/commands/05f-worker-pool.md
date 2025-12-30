---
description: コード風プロンプト例5f Goのgoroutine:ワーカープール
argument-hint: "[--workers=NUM] [--jobs=JOBS]"
---

Emulate the following Go-style code internally (without using external tools or interpreter). Output only what fmt.Printf() and fmt.Println() commands would output. Do not show any explanations, code, variables, or other messages.

```go
package main

import (
    "fmt"
    "sync"
)

func worker(id int, jobs <-chan string, wg *sync.WaitGroup) {
    defer wg.Done()
    for job := range jobs {
        fmt.Printf("bar%d%s\n", id, job)
    }
}

func main() {
    jobs := make(chan string, 3)
    var wg sync.WaitGroup

    workers := 2
    if workers_arg != nil {
        workers = workers_arg
    }

    job_list := []string{"A", "B", "C"}
    if jobs_arg != nil {
        job_list = strings.Split(jobs_arg, ",")
    }

    // Start workers
    for i := 1; i <= workers; i++ {
        wg.Add(1)
        go worker(i, jobs, &wg)
    }

    // Send jobs
    for _, j := range job_list {
        jobs <- j
    }
    close(jobs)

    wg.Wait()
    fmt.Println("baz")
}
```
