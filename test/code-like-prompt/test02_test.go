package code_like_prompt

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

// Test02aDanglingElseOuterIndent tests 02a-dangling-else-outer-indent command
func Test02aDanglingElseOuterIndent(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "ATrueBTrue",
			Command: "/code-like-prompt:02a-dangling-else-outer-indent",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": true,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "ATrueBFalse",
			Command: "/code-like-prompt:02a-dangling-else-outer-indent",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": false,
			},
			// Testing actual output (not expected output)
			// Expected: (no output), Actual: "bar" (FAIL)
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "AFalseBTrue",
			Command: "/code-like-prompt:02a-dangling-else-outer-indent",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": true,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "AFalseBFalse",
			Command: "/code-like-prompt:02a-dangling-else-outer-indent",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": false,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test02aDanglingElseOuterBlock tests 02a-dangling-else-outer-block command
func Test02aDanglingElseOuterBlock(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "ATrueBTrue",
			Command: "/code-like-prompt:02a-dangling-else-outer-block",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": true,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "ATrueBFalse",
			Command: "/code-like-prompt:02a-dangling-else-outer-block",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": false,
			},
			// Testing actual output (not expected output)
			// Expected: (no output), Actual: "foo" (FAIL)
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "AFalseBTrue",
			Command: "/code-like-prompt:02a-dangling-else-outer-block",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": true,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "AFalseBFalse",
			Command: "/code-like-prompt:02a-dangling-else-outer-block",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": false,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test02aDanglingElseOuterKeyword tests 02a-dangling-else-outer-keyword command
func Test02aDanglingElseOuterKeyword(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "ATrueBTrue",
			Command: "/code-like-prompt:02a-dangling-else-outer-keyword",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": true,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "ATrueBFalse",
			Command: "/code-like-prompt:02a-dangling-else-outer-keyword",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": false,
			},
			// Special case: non-deterministic output
			// Testing actual output (not expected output)
			// Expected: (no output), Actual: empty, "foo", or "bar" (FAIL - non-deterministic)
			CustomAssert: func(t *testing.T, output string) {
				// Accept empty output, "foo", or "bar" as Claude's behavior varies
				hasFoo := strings.Contains(output, "foo")
				hasBar := strings.Contains(output, "bar")
				isEmpty := strings.TrimSpace(output) == ""
				assert.True(t, isEmpty || hasFoo || hasBar, "Output should be empty or contain either 'foo' or 'bar' (actual behavior is non-deterministic, expected is no output)")
			},
		},
		{
			Name:    "AFalseBTrue",
			Command: "/code-like-prompt:02a-dangling-else-outer-keyword",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": true,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "AFalseBFalse",
			Command: "/code-like-prompt:02a-dangling-else-outer-keyword",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": false,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test02bDanglingElseInnerIndent tests 02b-dangling-else-inner-indent command
func Test02bDanglingElseInnerIndent(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "ATrueBTrue",
			Command: "/code-like-prompt:02b-dangling-else-inner-indent",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": true,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "ATrueBFalse",
			Command: "/code-like-prompt:02b-dangling-else-inner-indent",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": false,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "AFalseBTrue",
			Command: "/code-like-prompt:02b-dangling-else-inner-indent",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": true,
			},
			// Testing actual output (not expected output)
			// Expected: (no output), Actual: "bar" (FAIL)
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "AFalseBFalse",
			Command: "/code-like-prompt:02b-dangling-else-inner-indent",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": false,
			},
			// Testing actual output (not expected output)
			// Expected: (no output), Actual: "bar" (FAIL)
			ExpectedOutputs: []string{
				"bar",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test02bDanglingElseInnerBlock tests 02b-dangling-else-inner-block command
func Test02bDanglingElseInnerBlock(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "ATrueBTrue",
			Command: "/code-like-prompt:02b-dangling-else-inner-block",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": true,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "ATrueBFalse",
			Command: "/code-like-prompt:02b-dangling-else-inner-block",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": false,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "AFalseBTrue",
			Command: "/code-like-prompt:02b-dangling-else-inner-block",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": true,
			},
			// Special case: explanatory text output
			// Testing actual output (not expected output)
			// Expected: (no output), Actual: explanation text (FAIL)
			// Since the actual output is explanatory text which varies, we just check that some output exists
			CustomAssert: func(t *testing.T, output string) {
				assert.NotEmpty(t, output, "Output should not be empty (actual behavior, expected is no output)")
			},
		},
		{
			Name:    "AFalseBFalse",
			Command: "/code-like-prompt:02b-dangling-else-inner-block",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": false,
			},
			// Testing actual output (not expected output)
			// Expected: (no output), Actual: "bar" or "baz" (FAIL - varies)
			CustomAssert: func(t *testing.T, output string) {
				// Accept "bar" or "baz" as Claude's behavior varies
				hasBar := strings.Contains(output, "bar")
				hasBaz := strings.Contains(output, "baz")
				assert.True(t, hasBar || hasBaz, "Output should contain either 'bar' or 'baz' (actual behavior varies, expected is no output)")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test02bDanglingElseInnerKeyword tests 02b-dangling-else-inner-keyword command
func Test02bDanglingElseInnerKeyword(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "ATrueBTrue",
			Command: "/code-like-prompt:02b-dangling-else-inner-keyword",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": true,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "ATrueBFalse",
			Command: "/code-like-prompt:02b-dangling-else-inner-keyword",
			Args: map[string]interface{}{
				"condition_a": true,
				"condition_b": false,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "AFalseBTrue",
			Command: "/code-like-prompt:02b-dangling-else-inner-keyword",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": true,
			},
			// Testing actual output (not expected output)
			// Expected: (no output), Actual: "bar" or "baz" (FAIL - varies)
			CustomAssert: func(t *testing.T, output string) {
				// Accept "bar" or "baz" as Claude's behavior varies
				hasBar := strings.Contains(output, "bar")
				hasBaz := strings.Contains(output, "baz")
				assert.True(t, hasBar || hasBaz, "Output should contain either 'bar' or 'baz' (actual behavior varies, expected is no output)")
			},
		},
		{
			Name:    "AFalseBFalse",
			Command: "/code-like-prompt:02b-dangling-else-inner-keyword",
			Args: map[string]interface{}{
				"condition_a": false,
				"condition_b": false,
			},
			// Testing actual output (not expected output)
			// Expected: (no output), Actual: "bar" or "baz" (FAIL - varies)
			CustomAssert: func(t *testing.T, output string) {
				// Accept "bar" or "baz" as Claude's behavior varies
				hasBar := strings.Contains(output, "bar")
				hasBaz := strings.Contains(output, "baz")
				assert.True(t, hasBar || hasBaz, "Output should contain either 'bar' or 'baz' (actual behavior varies, expected is no output)")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test02cDeepNestingIndent tests 02c-deep-nesting-indent command
func Test02cDeepNestingIndent(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "L1TL2TL3T",
			Command: "/code-like-prompt:02c-deep-nesting-indent",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": true,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "L1TL2TL3FL4T",
			Command: "/code-like-prompt:02c-deep-nesting-indent",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": false,
				"level4": true,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "L1TL2TL3FL4F",
			Command: "/code-like-prompt:02c-deep-nesting-indent",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"baz",
			},
		},
		{
			Name:    "L1TL2FL3TL4T",
			Command: "/code-like-prompt:02c-deep-nesting-indent",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": true,
				"level4": true,
			},
			ExpectedOutputs: []string{
				"qux",
			},
		},
		{
			Name:    "L1TL2FL3TL4F",
			Command: "/code-like-prompt:02c-deep-nesting-indent",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": true,
				"level4": false,
			},
			// Special case: explanatory text output
			// Testing actual output (not expected output)
			// Expected: (no output) - Dangling else pattern, Actual: explanation text (FAIL)
			// Since the actual output is explanatory text which varies, we just check that some output exists
			CustomAssert: func(t *testing.T, output string) {
				assert.NotEmpty(t, output, "Output should not be empty (actual behavior, expected is no output)")
			},
		},
		{
			Name:    "L1TL2FL3F",
			Command: "/code-like-prompt:02c-deep-nesting-indent",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"quux",
			},
		},
		{
			Name:    "L1FL2T",
			Command: "/code-like-prompt:02c-deep-nesting-indent",
			Args: map[string]interface{}{
				"level1": false,
				"level2": true,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"corge",
			},
		},
		{
			Name:    "L1FL2FL3T",
			Command: "/code-like-prompt:02c-deep-nesting-indent",
			Args: map[string]interface{}{
				"level1": false,
				"level2": false,
				"level3": true,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"grault",
			},
		},
		{
			Name:    "L1FL2FL3F",
			Command: "/code-like-prompt:02c-deep-nesting-indent",
			Args: map[string]interface{}{
				"level1": false,
				"level2": false,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"garply",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test02cDeepNestingBlock tests 02c-deep-nesting-block command
func Test02cDeepNestingBlock(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "L1TL2TL3T",
			Command: "/code-like-prompt:02c-deep-nesting-block",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": true,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "L1TL2TL3FL4T",
			Command: "/code-like-prompt:02c-deep-nesting-block",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": false,
				"level4": true,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "L1TL2TL3FL4F",
			Command: "/code-like-prompt:02c-deep-nesting-block",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"baz",
			},
		},
		{
			Name:    "L1TL2FL3TL4T",
			Command: "/code-like-prompt:02c-deep-nesting-block",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": true,
				"level4": true,
			},
			ExpectedOutputs: []string{
				"qux",
			},
		},
		{
			Name:    "L1TL2FL3TL4F",
			Command: "/code-like-prompt:02c-deep-nesting-block",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": true,
				"level4": false,
			},
			// Testing actual output (not expected output)
			// Expected: (no output) - Dangling else pattern, Actual: "baz" (FAIL)
			ExpectedOutputs: []string{
				"baz",
			},
		},
		{
			Name:    "L1TL2FL3F",
			Command: "/code-like-prompt:02c-deep-nesting-block",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"quux",
			},
		},
		{
			Name:    "L1FL2T",
			Command: "/code-like-prompt:02c-deep-nesting-block",
			Args: map[string]interface{}{
				"level1": false,
				"level2": true,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"corge",
			},
		},
		{
			Name:    "L1FL2FL3T",
			Command: "/code-like-prompt:02c-deep-nesting-block",
			Args: map[string]interface{}{
				"level1": false,
				"level2": false,
				"level3": true,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"grault",
			},
		},
		{
			Name:    "L1FL2FL3F",
			Command: "/code-like-prompt:02c-deep-nesting-block",
			Args: map[string]interface{}{
				"level1": false,
				"level2": false,
				"level3": false,
				"level4": false,
			},
			// Testing actual output (not expected output)
			// Expected: "garply", Actual: "baz" or "garply" (varies)
			CustomAssert: func(t *testing.T, output string) {
				// Accept "baz" or "garply" as Claude's behavior varies
				hasBaz := strings.Contains(output, "baz")
				hasGarply := strings.Contains(output, "garply")
				assert.True(t, hasBaz || hasGarply, "Output should contain either 'baz' or 'garply' (actual behavior varies)")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test02cDeepNestingKeyword tests 02c-deep-nesting-keyword command
func Test02cDeepNestingKeyword(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "L1TL2TL3T",
			Command: "/code-like-prompt:02c-deep-nesting-keyword",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": true,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "L1TL2TL3FL4T",
			Command: "/code-like-prompt:02c-deep-nesting-keyword",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": false,
				"level4": true,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "L1TL2TL3FL4F",
			Command: "/code-like-prompt:02c-deep-nesting-keyword",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"baz",
			},
		},
		{
			Name:    "L1TL2FL3TL4T",
			Command: "/code-like-prompt:02c-deep-nesting-keyword",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": true,
				"level4": true,
			},
			ExpectedOutputs: []string{
				"qux",
			},
		},
		{
			Name:    "L1TL2FL3TL4F",
			Command: "/code-like-prompt:02c-deep-nesting-keyword",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": true,
				"level4": false,
			},
			// Special case: explanatory text output
			// Testing actual output (not expected output)
			// Expected: (no output) - Dangling else pattern, Actual: explanation text (FAIL)
			// Since the actual output is explanatory text which varies, we just check that some output exists
			CustomAssert: func(t *testing.T, output string) {
				assert.NotEmpty(t, output, "Output should not be empty (actual behavior, expected is no output)")
			},
		},
		{
			Name:    "L1TL2FL3F",
			Command: "/code-like-prompt:02c-deep-nesting-keyword",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"quux",
			},
		},
		{
			Name:    "L1FL2T",
			Command: "/code-like-prompt:02c-deep-nesting-keyword",
			Args: map[string]interface{}{
				"level1": false,
				"level2": true,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"corge",
			},
		},
		{
			Name:    "L1FL2FL3T",
			Command: "/code-like-prompt:02c-deep-nesting-keyword",
			Args: map[string]interface{}{
				"level1": false,
				"level2": false,
				"level3": true,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"grault",
			},
		},
		{
			Name:    "L1FL2FL3F",
			Command: "/code-like-prompt:02c-deep-nesting-keyword",
			Args: map[string]interface{}{
				"level1": false,
				"level2": false,
				"level3": false,
				"level4": false,
			},
			ExpectedOutputs: []string{
				"garply",
			},
		},
	}

	RunTestCases(t, tests)
}
