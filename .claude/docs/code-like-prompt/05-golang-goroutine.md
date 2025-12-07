# 05-golang-goroutine Specification

## Concept

Test Claude's ability to interpret Go-style concurrency constructs, focusing on:
1. Goroutine spawning and parallel execution
2. Channel communication (send/receive)
3. Channel buffering
4. Select statement for multi-channel operations
5. WaitGroup synchronization
6. Potential mapping to Claude subagents

## Test Scenarios

### 05a: Basic Goroutine Spawn

Test parallel execution of goroutines.

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

Expected: "foo" and "bar" in some order (parallel), then "baz"
Possible outputs: foo bar baz, bar foo baz

### 05b: Channel Communication

Test sending and receiving on channels.

```go
package main

import "fmt"

func main() {
    ch := make(chan string)

    go func() {
        ch <- "foo"
        ch <- "bar"
    }()

    fmt.Println(<-ch)
    fmt.Println(<-ch)
    fmt.Println("baz")
}
```

Expected: foo, bar, baz (in order due to channel synchronization)

### 05c: Buffered Channel

Test buffered channel behavior.

```go
package main

import "fmt"

func main() {
    ch := make(chan string, 2)

    ch <- "foo"
    ch <- "bar"
    // ch <- "baz"  // Would block if uncommented

    fmt.Println(<-ch)
    fmt.Println(<-ch)
    fmt.Println("qux")
}
```

Expected: foo, bar, qux

### 05d: Select Statement

Test non-deterministic channel selection.

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

Expected: "foo" and "bar" in some order, then "baz"

### 05e: Select with Default (Non-blocking)

Test non-blocking channel operations.

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

Expected: foo, bar (default case taken because channel is empty)

### 05f: Worker Pool Pattern

Test multiple workers processing from a shared channel.

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

    // Start workers
    for i := 1; i <= 2; i++ {
        wg.Add(1)
        go worker(i, jobs, &wg)
    }

    // Send jobs
    jobs <- "A"
    jobs <- "B"
    jobs <- "C"
    close(jobs)

    wg.Wait()
    fmt.Println("baz")
}
```

Expected: bar1A, bar2B, bar1C (or similar distribution) then baz
Jobs distributed among workers, order varies.

### 05g: Channel as Semaphore

Test using buffered channel for limiting concurrency.

```go
package main

import (
    "fmt"
    "sync"
)

func main() {
    sem := make(chan struct{}, 2)  // Max 2 concurrent
    var wg sync.WaitGroup

    for i := 0; i < 4; i++ {
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

Expected: Four "foo" outputs with at most 2 concurrent, then "bar"

### 05h: Pipeline Pattern

Test chained channels for data processing pipeline.

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

Expected: fooX, barX, baz

### 05i: Timeout with Select

Test timeout handling in concurrent operations.

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

    select {
    case msg := <-ch:
        fmt.Println(msg)
    case <-time.After(1 * time.Second):
        fmt.Println("bar")
    }
    fmt.Println("baz")
}
```

Expected: bar (timeout), baz

### 05j: Fan-out/Fan-in

Test distributing work and collecting results.

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
    results := fanOut("X", 3)

    for r := range results {
        fmt.Println(r)
    }
    fmt.Println("bar")
}
```

Expected: foo0X, foo1X, foo2X (some order), then bar

## Subagent Mapping Potential

The goroutine patterns could potentially map to Claude subagent usage:

| Go Pattern | Potential Claude Mapping |
|------------|-------------------------|
| goroutine | Spawn subagent (Task tool) |
| channel send/receive | Agent communication (limited) |
| WaitGroup | Wait for agent completion |
| select | Choose first completing agent |
| worker pool | Multiple parallel agents |

### 05k: Subagent-style Task Delegation

Test whether Go-style syntax can trigger parallel task execution.

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

Expected: result_foo, result_bar, result_baz (some order), then qux

This tests whether Claude interprets the parallel structure and potentially uses parallel tool calls or subagents.

## Command Design

### Arguments

- `--workers`: Number of worker goroutines
- `--jobs`: Comma-separated list of job inputs
- `--timeout`: Timeout duration for timeout tests

### Variants

| Command | Focus |
|---------|-------|
| `05a-basic-goroutine` | Simple parallel execution |
| `05b-channel-sync` | Channel communication |
| `05c-buffered-channel` | Buffer behavior |
| `05d-select` | Multi-channel select |
| `05e-select-default` | Non-blocking operations |
| `05f-worker-pool` | Worker pool pattern |
| `05g-semaphore` | Concurrency limiting |
| `05h-pipeline` | Data processing pipeline |
| `05i-timeout` | Timeout handling |
| `05j-fan-out-in` | Fan-out/fan-in pattern |
| `05k-subagent-style` | Potential subagent mapping |

## Expected Behaviors

1. **Parallel execution**: Goroutines should execute concurrently (order may vary)
2. **Channel blocking**: Unbuffered channels block until matched send/receive
3. **Buffer semantics**: Buffered channels allow sends up to capacity
4. **Select fairness**: When multiple cases ready, choice is non-deterministic
5. **Close semantics**: Closed channels return zero values immediately

## Key Test Points

### Non-determinism (05a, 05d)
Output order should vary or Claude should acknowledge non-determinism.

### Deadlock Detection (implicit)
Some patterns could deadlock if mishandled - Claude should recognize this.

### Subagent Potential (05k)
Tests whether Go concurrency syntax can influence Claude to use parallel processing or subagents.

## Implementation Priority

1. `05k-subagent-style` - Most interesting for Claude-specific behavior
2. `05f-worker-pool` - Common parallel pattern
3. `05h-pipeline` - Tests data flow understanding
4. `05d-select` - Core concurrency primitive
5. `05i-timeout` - Practical timeout handling
6. `05a-basic-goroutine` through `05c-buffered-channel` - Fundamentals
7. `05g-semaphore`, `05j-fan-out-in` - Advanced patterns
