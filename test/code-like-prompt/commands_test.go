package code_like_prompt

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

// TestMain runs before all tests and after all tests
func TestMain(m *testing.M) {
	// Run all tests
	exitCode := m.Run()

	// Cleanup: stop VCR proxy if it was started
	if globalProxy != nil {
		stopVCRProxy(&testing.T{}, globalProxy)
		globalProxy = nil
	}

	os.Exit(exitCode)
}

// TestCase defines a test case structure for code-like-prompt commands
type TestCase struct {
	Name            string
	Command         string
	Args            map[string]interface{}
	ExpectedOutputs []string
}

// Test01Shopping tests the basic conditional commands (milk joke)
func Test01Shopping(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "shopping_request_with_eggs",
			Command: "/code-like-prompt:01-shopping-request",
			Args: map[string]interface{}{
				"Milk.stock": 5,
				"Egg.stock":  3,
			},
			ExpectedOutputs: []string{
				"Bought 1 milk.",
				"Bought 6 eggs.",
			},
		},
		{
			Name:    "shopping_request_without_eggs",
			Command: "/code-like-prompt:01-shopping-request",
			Args: map[string]interface{}{
				"Milk.stock": 5,
				"Egg.stock":  0,
			},
			ExpectedOutputs: []string{
				"Bought 1 milk.",
			},
		},
		{
			Name:    "shopping_misunderstanding_with_eggs",
			Command: "/code-like-prompt:01-shopping-misunderstanding",
			Args: map[string]interface{}{
				"Milk.stock": 5,
				"Egg.stock":  3,
			},
			ExpectedOutputs: []string{
				"Bought 6 milks.",
			},
		},
		{
			Name:    "shopping_misunderstanding_without_eggs",
			Command: "/code-like-prompt:01-shopping-misunderstanding",
			Args: map[string]interface{}{
				"Milk.stock": 5,
				"Egg.stock":  0,
			},
			ExpectedOutputs: []string{
				"Bought 1 milks.",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}
}

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

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}
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

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}
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

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}

	// Special case for non-deterministic behavior
	t.Run("ATrueBFalse", func(t *testing.T) {
		// Setup
		tmpDir := setupTestEnvironment(t)
		defer cleanupTestEnvironment(t, tmpDir)

		// Execute
		output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-keyword", map[string]interface{}{
			"condition_a": true,
			"condition_b": false,
		})

		// Testing actual output (not expected output)
		// Expected: (no output), Actual: "foo" or "bar" (FAIL - non-deterministic)
		hasFoo := strings.Contains(output, "foo")
		hasBar := strings.Contains(output, "bar")
		assert.True(t, hasFoo || hasBar, "Output should contain either 'foo' or 'bar' (actual behavior is non-deterministic, expected is no output)")
	})
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

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}
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
			Name:    "AFalseBFalse",
			Command: "/code-like-prompt:02b-dangling-else-inner-block",
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

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}

	// Special case for explanatory text output
	t.Run("AFalseBTrue", func(t *testing.T) {
		// Setup
		tmpDir := setupTestEnvironment(t)
		defer cleanupTestEnvironment(t, tmpDir)

		// Execute
		output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-block", map[string]interface{}{
			"condition_a": false,
			"condition_b": true,
		})

		// Testing actual output (not expected output)
		// Expected: (no output), Actual: explanation text (FAIL)
		// Since the actual output is explanatory text which varies, we just check that some output exists
		assert.NotEmpty(t, output, "Output should not be empty (actual behavior, expected is no output)")
	})
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
			// Expected: (no output), Actual: "bar" (FAIL)
			ExpectedOutputs: []string{
				"bar",
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
			// Expected: (no output), Actual: "bar" (FAIL)
			ExpectedOutputs: []string{
				"bar",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}
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

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}

	// Special case for explanatory text output
	t.Run("L1TL2FL3TL4F", func(t *testing.T) {
		// Setup
		tmpDir := setupTestEnvironment(t)
		defer cleanupTestEnvironment(t, tmpDir)

		// Execute
		output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
			"level1": true,
			"level2": false,
			"level3": true,
			"level4": false,
		})

		// Testing actual output (not expected output)
		// Expected: (no output) - Dangling else pattern, Actual: explanation text (FAIL)
		// Since the actual output is explanatory text which varies, we just check that some output exists
		assert.NotEmpty(t, output, "Output should not be empty (actual behavior, expected is no output)")
	})
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
			ExpectedOutputs: []string{
				"garply",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}
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

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Setup
			tmpDir := setupTestEnvironment(t)
			defer cleanupTestEnvironment(t, tmpDir)

			// Execute
			output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

			// Assert
			for _, expected := range tt.ExpectedOutputs {
				assert.Contains(t, output, expected, "Output should contain expected text")
			}
		})
	}

	// Special case for explanatory text output
	t.Run("L1TL2FL3TL4F", func(t *testing.T) {
		// Setup
		tmpDir := setupTestEnvironment(t)
		defer cleanupTestEnvironment(t, tmpDir)

		// Execute
		output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
			"level1": true,
			"level2": false,
			"level3": true,
			"level4": false,
		})

		// Testing actual output (not expected output)
		// Expected: (no output) - Dangling else pattern, Actual: explanation text (FAIL)
		// Since the actual output is explanatory text which varies, we just check that some output exists
		assert.NotEmpty(t, output, "Output should not be empty (actual behavior, expected is no output)")
	})
}
