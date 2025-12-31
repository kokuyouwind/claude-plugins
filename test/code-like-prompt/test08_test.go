package code_like_prompt

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

// Test08aInlineActor tests 08a-inline-actor command
func Test08aInlineActor(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "InlineActor",
			Command: "/code-like-prompt:08a-inline-actor",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo, bar, and baz
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "bar")
				assert.Contains(t, output, "baz")
				// baz should be printed last (after receiving done_foo and done_bar)
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

// Test08bAgentSpawn tests 08b-agent-spawn command
func Test08bAgentSpawn(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "AgentSpawn",
			Command: "/code-like-prompt:08b-agent-spawn",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo1, foo2, and bar
				assert.Contains(t, output, "foo1")
				assert.Contains(t, output, "foo2")
				assert.Contains(t, output, "bar")
				// bar should be printed last (after receiving {done, 1} and {done, 2})
				barIdx := strings.Index(output, "bar")
				assert.Greater(t, barIdx, 0, "bar should appear in output")
				// Both foo1 and foo2 should appear before bar
				foo1Idx := strings.Index(output, "foo1")
				foo2Idx := strings.Index(output, "foo2")
				assert.True(t, foo1Idx < barIdx, "foo1 should appear before bar")
				assert.True(t, foo2Idx < barIdx, "foo2 should appear before bar")
				// Order between foo1 and foo2 is non-deterministic, don't check
			},
		},
	}

	RunTestCases(t, tests)
}

// Test08cMessageDirect tests 08c-message-direct command
func Test08cMessageDirect(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "MessageDirect",
			Command: "/code-like-prompt:08c-message-direct",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"processed_foo",
				"bar",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain processed_foo and bar
				assert.Contains(t, output, "processed_foo")
				assert.Contains(t, output, "bar")
				// Check order: processed_foo, bar
				processedIdx := strings.Index(output, "processed_foo")
				barIdx := strings.Index(output, "bar")
				assert.True(t, processedIdx < barIdx, "processed_foo should appear before bar")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test08dMessageHelper tests 08d-message-helper command
func Test08dMessageHelper(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "MessageHelper",
			Command: "/code-like-prompt:08d-message-helper",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"result_foo",
				"bar",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain result_foo and bar
				assert.Contains(t, output, "result_foo")
				assert.Contains(t, output, "bar")
				// Check order: result_foo, bar
				resultIdx := strings.Index(output, "result_foo")
				barIdx := strings.Index(output, "bar")
				assert.True(t, resultIdx < barIdx, "result_foo should appear before bar")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test08eSelectiveReceive tests 08e-selective-receive command
func Test08eSelectiveReceive(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "SelectiveReceive",
			Command: "/code-like-prompt:08e-selective-receive",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"foo",
				"bar",
				"123",
				"baz",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain foo, bar, 123, and baz
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "bar")
				assert.Contains(t, output, "123")
				assert.Contains(t, output, "baz")
				// Check order: foo, bar, 123, baz (selective receive pattern)
				fooIdx := strings.Index(output, "foo")
				barIdx := strings.Index(output, "bar")
				num123Idx := strings.Index(output, "123")
				bazIdx := strings.Index(output, "baz")
				assert.True(t, fooIdx < barIdx, "foo should appear before bar")
				assert.True(t, barIdx < num123Idx, "bar should appear before 123")
				assert.True(t, num123Idx < bazIdx, "123 should appear before baz")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test08fMultiActor tests 08f-multi-actor command
func Test08fMultiActor(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "MultiActor",
			Command: "/code-like-prompt:08f-multi-actor",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain "foobar" (concatenated) or "barfoo" (order depends on worker execution)
				// and baz
				hasFoobar := strings.Contains(output, "foobar")
				hasBarfoo := strings.Contains(output, "barfoo")
				assert.True(t, hasFoobar || hasBarfoo, "Output should contain 'foobar' or 'barfoo'")
				assert.Contains(t, output, "baz")
				// baz should be last
				bazIdx := strings.Index(output, "baz")
				assert.Greater(t, bazIdx, 0, "baz should appear in output")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test08gSupervisor tests 08g-supervisor command
func Test08gSupervisor(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "Supervisor",
			Command: "/code-like-prompt:08g-supervisor",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"bar",
				"baz",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain bar and baz
				assert.Contains(t, output, "bar")
				assert.Contains(t, output, "baz")
				// Check order: bar, baz
				barIdx := strings.Index(output, "bar")
				bazIdx := strings.Index(output, "baz")
				assert.True(t, barIdx < bazIdx, "bar should appear before baz")
				// Should NOT contain "foo" (no crash in this demo)
				assert.NotContains(t, output, "foo")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test08hPluginAgentSpawn tests 08h-plugin-agent-spawn command
func Test08hPluginAgentSpawn(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "PluginAgentSpawnDefault",
			Command: "/code-like-prompt:08h-plugin-agent-spawn",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain "hello" (default message) and "processed_hello" (worker response)
				assert.Contains(t, output, "hello")
				assert.Contains(t, output, "processed_hello")
				// Should show main/worker flow
				helloIdx := strings.Index(output, "hello")
				processedIdx := strings.Index(output, "processed_hello")
				assert.True(t, helloIdx < processedIdx, "hello should appear before processed_hello")
			},
		},
		{
			Name:    "PluginAgentSpawnCustomMessage",
			Command: "/code-like-prompt:08h-plugin-agent-spawn",
			Args:    map[string]interface{}{"message": "test"},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain "test" and "processed_test"
				assert.Contains(t, output, "test")
				assert.Contains(t, output, "processed_test")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test08iPluginAgentMessaging tests 08i-plugin-agent-messaging command
func Test08iPluginAgentMessaging(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "PluginAgentMessagingDefault",
			Command: "/code-like-prompt:08i-plugin-agent-messaging",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain default messages "foo" and "bar"
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "bar")
				// Should contain processed results
				assert.Contains(t, output, "processed_foo")
				assert.Contains(t, output, "processed_bar")
				// Should contain combined result
				hasFoobar := strings.Contains(output, "processed_fooprocessed_bar")
				hasBarfoo := strings.Contains(output, "processed_barprocessed_foo")
				assert.True(t, hasFoobar || hasBarfoo, "Output should contain combined result")
			},
		},
		{
			Name:    "PluginAgentMessagingCustomMessages",
			Command: "/code-like-prompt:08i-plugin-agent-messaging",
			Args:    map[string]interface{}{"message1": "alpha", "message2": "beta"},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain custom messages
				assert.Contains(t, output, "alpha")
				assert.Contains(t, output, "beta")
				// Should contain processed results
				assert.Contains(t, output, "processed_alpha")
				assert.Contains(t, output, "processed_beta")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test08jPluginAgentScriptMessaging tests 08j-plugin-agent-script-messaging command
func Test08jPluginAgentScriptMessaging(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "PluginAgentScriptMessagingDefault",
			Command: "/code-like-prompt:08j-plugin-agent-script-messaging",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain default messages "foo" and "bar"
				assert.Contains(t, output, "foo")
				assert.Contains(t, output, "bar")
				// Should mention script usage or message passing
				hasScriptUsage := strings.Contains(output, "send-message.sh") ||
					strings.Contains(output, "receive-message.sh") ||
					strings.Contains(output, "/tmp/erlang-messages")
				hasProcessedResults := strings.Contains(output, "processed_foo") &&
					strings.Contains(output, "processed_bar")
				// At least one of these should be true
				assert.True(t, hasScriptUsage || hasProcessedResults, "Output should show script usage or processed results")
			},
		},
		{
			Name:    "PluginAgentScriptMessagingCustomMessages",
			Command: "/code-like-prompt:08j-plugin-agent-script-messaging",
			Args:    map[string]interface{}{"message1": "gamma", "message2": "delta"},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain custom messages
				assert.Contains(t, output, "gamma")
				assert.Contains(t, output, "delta")
			},
		},
	}

	RunTestCases(t, tests)
}
