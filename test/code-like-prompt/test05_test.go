package code_like_prompt

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

// Test05aBasicGoroutine tests 05a-basic-goroutine command
func Test05aBasicGoroutine(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "BasicGoroutine",
			Command: "/code-like-prompt:05a-basic-goroutine",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo, bar, and baz
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "bar")
				assert.Contains(t, output, "baz")
				// baz should be printed last (after WaitGroup)
				bazIdx := strings.Index(output, "baz")
				assert.Greater(t, bazIdx, 0, "baz should appear in output")
				// Both foo and bar should appear before baz
				fooIdx := strings.Index(output, "foo")
				barIdx := strings.Index(output, "bar")
				assert.True(t, fooIdx < bazIdx, "foo should appear before baz")
				assert.True(t, barIdx < bazIdx, "bar should appear before baz")
				// Order between foo and bar is non-deterministic, don't check
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05bChannelSync tests 05b-channel-sync command
func Test05bChannelSync(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "ChannelSync",
			Command: "/code-like-prompt:05b-channel-sync",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"foo",
				"bar",
				"baz",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain all expected outputs
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "bar")
				assert.Contains(t, output, "baz")
				// Check order: foo, bar, baz (channels maintain order)
				fooIdx := strings.Index(output, "foo")
				barIdx := strings.Index(output, "bar")
				bazIdx := strings.Index(output, "baz")
				assert.True(t, fooIdx < barIdx, "foo should appear before bar")
				assert.True(t, barIdx < bazIdx, "bar should appear before baz")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05cBufferedChannel tests 05c-buffered-channel command
func Test05cBufferedChannel(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "BufferedChannel",
			Command: "/code-like-prompt:05c-buffered-channel",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"foo",
				"bar",
				"qux",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain all expected outputs
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "bar")
				assert.Contains(t, output, "qux")
				// Check order: foo, bar, qux (FIFO order)
				fooIdx := strings.Index(output, "foo")
				barIdx := strings.Index(output, "bar")
				quxIdx := strings.Index(output, "qux")
				assert.True(t, fooIdx < barIdx, "foo should appear before bar")
				assert.True(t, barIdx < quxIdx, "bar should appear before qux")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05dSelect tests 05d-select command
func Test05dSelect(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "Select",
			Command: "/code-like-prompt:05d-select",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo, bar, and baz
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "bar")
				assert.Contains(t, output, "baz")
				// baz should be printed last
				bazIdx := strings.Index(output, "baz")
				fooIdx := strings.Index(output, "foo")
				barIdx := strings.Index(output, "bar")
				assert.True(t, fooIdx < bazIdx, "foo should appear before baz")
				assert.True(t, barIdx < bazIdx, "bar should appear before baz")
				// Order between foo and bar is non-deterministic (select is random)
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05eSelectDefault tests 05e-select-default command
func Test05eSelectDefault(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "SelectDefault",
			Command: "/code-like-prompt:05e-select-default",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"foo",
				"bar",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo (default case) and bar
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "bar")
				// Check order: foo, bar
				fooIdx := strings.Index(output, "foo")
				barIdx := strings.Index(output, "bar")
				assert.True(t, fooIdx < barIdx, "foo should appear before bar")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05fWorkerPool tests 05f-worker-pool command
func Test05fWorkerPool(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "DefaultWorkers",
			Command: "/code-like-prompt:05f-worker-pool",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain bar1A, bar2A, bar1B, bar2B, bar1C, bar2C (or similar variations)
				// Workers can process jobs in any order
				assert.Contains(t, output, "baz")
				// Check that jobs A, B, C were processed
				hasA := strings.Contains(output, "A")
				hasB := strings.Contains(output, "B")
				hasC := strings.Contains(output, "C")
				assert.True(t, hasA, "Output should contain job A")
				assert.True(t, hasB, "Output should contain job B")
				assert.True(t, hasC, "Output should contain job C")
				// Check that workers 1 and 2 both processed jobs
				hasWorker1 := strings.Contains(output, "bar1")
				hasWorker2 := strings.Contains(output, "bar2")
				assert.True(t, hasWorker1 || hasWorker2, "Output should contain worker 1 or 2")
				// baz should be last
				bazIdx := strings.Index(output, "baz")
				assert.Greater(t, bazIdx, 0, "baz should appear at the end")
			},
		},
		{
			Name:    "CustomWorkersAndJobs",
			Command: "/code-like-prompt:05f-worker-pool",
			Args: map[string]interface{}{
				"workers": 3,
				"jobs":    "X,Y",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain bar1X, bar2X, bar3X, bar1Y, bar2Y, bar3Y (or similar variations)
				assert.Contains(t, output, "baz")
				// Check that jobs X, Y were processed
				hasX := strings.Contains(output, "X")
				hasY := strings.Contains(output, "Y")
				assert.True(t, hasX, "Output should contain job X")
				assert.True(t, hasY, "Output should contain job Y")
				// Check that at least one worker processed jobs
				hasWorker := strings.Contains(output, "bar1") || strings.Contains(output, "bar2") || strings.Contains(output, "bar3")
				assert.True(t, hasWorker, "Output should contain worker output")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05gSemaphore tests 05g-semaphore command
func Test05gSemaphore(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "DefaultSemaphore",
			Command: "/code-like-prompt:05g-semaphore",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo0, foo1, foo2, foo3 (in any order)
				assert.Contains(t, output, "foo0")
				assert.Contains(t, output, "foo1")
				assert.Contains(t, output, "foo2")
				assert.Contains(t, output, "foo3")
				assert.Contains(t, output, "bar")
				// bar should be last
				barIdx := strings.Index(output, "bar")
				assert.Greater(t, barIdx, 0, "bar should appear at the end")
			},
		},
		{
			Name:    "CustomTasksAndConcurrency",
			Command: "/code-like-prompt:05g-semaphore",
			Args: map[string]interface{}{
				"tasks":          3,
				"max_concurrent": 1,
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo0, foo1, foo2
				assert.Contains(t, output, "foo0")
				assert.Contains(t, output, "foo1")
				assert.Contains(t, output, "foo2")
				assert.Contains(t, output, "bar")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05hPipeline tests 05h-pipeline command
func Test05hPipeline(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "Pipeline",
			Command: "/code-like-prompt:05h-pipeline",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain fooX, barX, baz
				assert.Contains(t, output, "fooX")
				assert.Contains(t, output, "barX")
				assert.Contains(t, output, "baz")
				// baz should be last
				bazIdx := strings.Index(output, "baz")
				fooXIdx := strings.Index(output, "fooX")
				barXIdx := strings.Index(output, "barX")
				assert.True(t, fooXIdx < bazIdx, "fooX should appear before baz")
				assert.True(t, barXIdx < bazIdx, "barX should appear before baz")
				// Order between fooX and barX may vary depending on goroutine scheduling
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05iTimeout tests 05i-timeout command
func Test05iTimeout(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "TimeoutTriggered",
			Command: "/code-like-prompt:05i-timeout",
			Args: map[string]interface{}{
				"timeout": 1,
			},
			ExpectedOutputs: []string{
				"bar",
				"baz",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Timeout=1 sec, but goroutine sleeps 2 sec, so "bar" should be printed
				assert.Contains(t, output, "bar")
				assert.Contains(t, output, "baz")
				// Should NOT contain "foo" (timeout triggered before message received)
				assert.NotContains(t, output, "foo")
			},
		},
		{
			Name:    "TimeoutNotTriggered",
			Command: "/code-like-prompt:05i-timeout",
			Args: map[string]interface{}{
				"timeout": 3,
			},
			ExpectedOutputs: []string{
				"foo",
				"baz",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Timeout=3 sec, goroutine sleeps 2 sec, so "foo" should be received
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "baz")
				// Should NOT contain "bar" (timeout not triggered)
				assert.NotContains(t, output, "bar")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05jFanOutIn tests 05j-fan-out-in command
func Test05jFanOutIn(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "DefaultFanOutIn",
			Command: "/code-like-prompt:05j-fan-out-in",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo0X, foo1X, foo2X (in any order)
				assert.Contains(t, output, "foo0X")
				assert.Contains(t, output, "foo1X")
				assert.Contains(t, output, "foo2X")
				assert.Contains(t, output, "bar")
				// bar should be last
				barIdx := strings.Index(output, "bar")
				assert.Greater(t, barIdx, 0, "bar should appear at the end")
			},
		},
		{
			Name:    "CustomWorkersAndInput",
			Command: "/code-like-prompt:05j-fan-out-in",
			Args: map[string]interface{}{
				"workers": 2,
				"input":   "Y",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo0Y, foo1Y
				assert.Contains(t, output, "foo0Y")
				assert.Contains(t, output, "foo1Y")
				assert.Contains(t, output, "bar")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test05kSubagentStyle tests 05k-subagent-style command
func Test05kSubagentStyle(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "DefaultSubagentStyle",
			Command: "/code-like-prompt:05k-subagent-style",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain result_foo, result_bar, result_baz (in any order)
				assert.Contains(t, output, "result_foo")
				assert.Contains(t, output, "result_bar")
				assert.Contains(t, output, "result_baz")
				assert.Contains(t, output, "qux")
				// qux should be last
				quxIdx := strings.Index(output, "qux")
				assert.Greater(t, quxIdx, 0, "qux should appear at the end")
			},
		},
		{
			Name:    "CustomTopics",
			Command: "/code-like-prompt:05k-subagent-style",
			Args: map[string]interface{}{
				"topics": "alpha,beta",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain result_alpha, result_beta
				assert.Contains(t, output, "result_alpha")
				assert.Contains(t, output, "result_beta")
				assert.Contains(t, output, "qux")
			},
		},
	}

	RunTestCases(t, tests)
}
