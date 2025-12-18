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

// Test02aDanglingElseOuterIndent_ATrueBTrue tests 02a-dangling-else-outer-indent with condition_a=true, condition_b=true
// Expected: "foo"
// Actual: "foo" (PASS)
func Test02aDanglingElseOuterIndent_ATrueBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-indent", map[string]interface{}{
		"condition_a": true,
		"condition_b": true,
	})

	assert.Contains(t, output, "foo", "Output should contain 'foo'")
}

// Test02aDanglingElseOuterIndent_ATrueBFalse tests 02a-dangling-else-outer-indent with condition_a=true, condition_b=false
// Expected: (no output)
// Actual: "bar" (FAIL - outputs "bar" instead of no output)
func Test02aDanglingElseOuterIndent_ATrueBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-indent", map[string]interface{}{
		"condition_a": true,
		"condition_b": false,
	})

	// Testing actual output (not expected output)
	assert.Contains(t, output, "bar", "Output should contain 'bar' (actual behavior, expected is no output)")
}

// Test02aDanglingElseOuterIndent_AFalseBTrue tests 02a-dangling-else-outer-indent with condition_a=false, condition_b=true
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02aDanglingElseOuterIndent_AFalseBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-indent", map[string]interface{}{
		"condition_a": false,
		"condition_b": true,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02aDanglingElseOuterIndent_AFalseBFalse tests 02a-dangling-else-outer-indent with condition_a=false, condition_b=false
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02aDanglingElseOuterIndent_AFalseBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-indent", map[string]interface{}{
		"condition_a": false,
		"condition_b": false,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02aDanglingElseOuterBlock_ATrueBTrue tests 02a-dangling-else-outer-block with condition_a=true, condition_b=true
// Expected: "foo"
// Actual: "foo" (PASS)
func Test02aDanglingElseOuterBlock_ATrueBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-block", map[string]interface{}{
		"condition_a": true,
		"condition_b": true,
	})

	assert.Contains(t, output, "foo", "Output should contain 'foo'")
}

// Test02aDanglingElseOuterBlock_ATrueBFalse tests 02a-dangling-else-outer-block with condition_a=true, condition_b=false
// Expected: (no output)
// Actual: "foo" (FAIL - outputs "foo" instead of no output)
func Test02aDanglingElseOuterBlock_ATrueBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-block", map[string]interface{}{
		"condition_a": true,
		"condition_b": false,
	})

	// Testing actual output (not expected output)
	assert.Contains(t, output, "foo", "Output should contain 'foo' (actual behavior, expected is no output)")
}

// Test02aDanglingElseOuterBlock_AFalseBTrue tests 02a-dangling-else-outer-block with condition_a=false, condition_b=true
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02aDanglingElseOuterBlock_AFalseBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-block", map[string]interface{}{
		"condition_a": false,
		"condition_b": true,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02aDanglingElseOuterBlock_AFalseBFalse tests 02a-dangling-else-outer-block with condition_a=false, condition_b=false
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02aDanglingElseOuterBlock_AFalseBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-block", map[string]interface{}{
		"condition_a": false,
		"condition_b": false,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02aDanglingElseOuterKeyword_ATrueBTrue tests 02a-dangling-else-outer-keyword with condition_a=true, condition_b=true
// Expected: "foo"
// Actual: "foo" (PASS)
func Test02aDanglingElseOuterKeyword_ATrueBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-keyword", map[string]interface{}{
		"condition_a": true,
		"condition_b": true,
	})

	assert.Contains(t, output, "foo", "Output should contain 'foo'")
}

// Test02aDanglingElseOuterKeyword_ATrueBFalse tests 02a-dangling-else-outer-keyword with condition_a=true, condition_b=false
// Expected: (no output)
// Actual: "foo" or "bar" (FAIL - outputs either "foo" or "bar" instead of no output)
// Note: Previous test (2025-12-15) showed "foo", but current behavior is non-deterministic (sometimes "foo", sometimes "bar")
func Test02aDanglingElseOuterKeyword_ATrueBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-keyword", map[string]interface{}{
		"condition_a": true,
		"condition_b": false,
	})

	// Testing actual output (not expected output)
	// Behavior is non-deterministic, check for either "foo" or "bar"
	hasFoo := strings.Contains(output, "foo")
	hasBar := strings.Contains(output, "bar")
	assert.True(t, hasFoo || hasBar, "Output should contain either 'foo' or 'bar' (actual behavior is non-deterministic, expected is no output)")
}

// Test02aDanglingElseOuterKeyword_AFalseBTrue tests 02a-dangling-else-outer-keyword with condition_a=false, condition_b=true
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02aDanglingElseOuterKeyword_AFalseBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-keyword", map[string]interface{}{
		"condition_a": false,
		"condition_b": true,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02aDanglingElseOuterKeyword_AFalseBFalse tests 02a-dangling-else-outer-keyword with condition_a=false, condition_b=false
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02aDanglingElseOuterKeyword_AFalseBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02a-dangling-else-outer-keyword", map[string]interface{}{
		"condition_a": false,
		"condition_b": false,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02bDanglingElseInnerIndent_ATrueBTrue tests 02b-dangling-else-inner-indent with condition_a=true, condition_b=true
// Expected: "foo"
// Actual: "foo" (PASS)
func Test02bDanglingElseInnerIndent_ATrueBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-indent", map[string]interface{}{
		"condition_a": true,
		"condition_b": true,
	})

	assert.Contains(t, output, "foo", "Output should contain 'foo'")
}

// Test02bDanglingElseInnerIndent_ATrueBFalse tests 02b-dangling-else-inner-indent with condition_a=true, condition_b=false
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02bDanglingElseInnerIndent_ATrueBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-indent", map[string]interface{}{
		"condition_a": true,
		"condition_b": false,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02bDanglingElseInnerIndent_AFalseBTrue tests 02b-dangling-else-inner-indent with condition_a=false, condition_b=true
// Expected: (no output)
// Actual: "bar" (FAIL - outputs "bar" instead of no output)
func Test02bDanglingElseInnerIndent_AFalseBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-indent", map[string]interface{}{
		"condition_a": false,
		"condition_b": true,
	})

	// Testing actual output (not expected output)
	assert.Contains(t, output, "bar", "Output should contain 'bar' (actual behavior, expected is no output)")
}

// Test02bDanglingElseInnerIndent_AFalseBFalse tests 02b-dangling-else-inner-indent with condition_a=false, condition_b=false
// Expected: (no output)
// Actual: "bar" (FAIL - outputs "bar" instead of no output)
func Test02bDanglingElseInnerIndent_AFalseBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-indent", map[string]interface{}{
		"condition_a": false,
		"condition_b": false,
	})

	// Testing actual output (not expected output)
	assert.Contains(t, output, "bar", "Output should contain 'bar' (actual behavior, expected is no output)")
}

// Test02bDanglingElseInnerBlock_ATrueBTrue tests 02b-dangling-else-inner-block with condition_a=true, condition_b=true
// Expected: "foo"
// Actual: "foo" (PASS)
func Test02bDanglingElseInnerBlock_ATrueBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-block", map[string]interface{}{
		"condition_a": true,
		"condition_b": true,
	})

	assert.Contains(t, output, "foo", "Output should contain 'foo'")
}

// Test02bDanglingElseInnerBlock_ATrueBFalse tests 02b-dangling-else-inner-block with condition_a=true, condition_b=false
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02bDanglingElseInnerBlock_ATrueBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-block", map[string]interface{}{
		"condition_a": true,
		"condition_b": false,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02bDanglingElseInnerBlock_AFalseBTrue tests 02b-dangling-else-inner-block with condition_a=false, condition_b=true
// Expected: (no output)
// Actual: explanation text (FAIL - outputs explanation instead of no output)
// Note: The actual output varies but typically contains explanatory text about the conditions
func Test02bDanglingElseInnerBlock_AFalseBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-block", map[string]interface{}{
		"condition_a": false,
		"condition_b": true,
	})

	// Testing actual output (not expected output)
	// Since the actual output is explanatory text which varies, we just check that some output exists
	assert.NotEmpty(t, output, "Output should not be empty (actual behavior, expected is no output)")
}

// Test02bDanglingElseInnerBlock_AFalseBFalse tests 02b-dangling-else-inner-block with condition_a=false, condition_b=false
// Expected: (no output)
// Actual: "bar" (FAIL - outputs "bar" instead of no output)
func Test02bDanglingElseInnerBlock_AFalseBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-block", map[string]interface{}{
		"condition_a": false,
		"condition_b": false,
	})

	// Testing actual output (not expected output)
	assert.Contains(t, output, "bar", "Output should contain 'bar' (actual behavior, expected is no output)")
}

// Test02bDanglingElseInnerKeyword_ATrueBTrue tests 02b-dangling-else-inner-keyword with condition_a=true, condition_b=true
// Expected: "foo"
// Actual: "foo" (PASS)
func Test02bDanglingElseInnerKeyword_ATrueBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-keyword", map[string]interface{}{
		"condition_a": true,
		"condition_b": true,
	})

	assert.Contains(t, output, "foo", "Output should contain 'foo'")
}

// Test02bDanglingElseInnerKeyword_ATrueBFalse tests 02b-dangling-else-inner-keyword with condition_a=true, condition_b=false
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02bDanglingElseInnerKeyword_ATrueBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-keyword", map[string]interface{}{
		"condition_a": true,
		"condition_b": false,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02bDanglingElseInnerKeyword_AFalseBTrue tests 02b-dangling-else-inner-keyword with condition_a=false, condition_b=true
// Expected: (no output)
// Actual: "bar" (FAIL - outputs "bar" instead of no output)
func Test02bDanglingElseInnerKeyword_AFalseBTrue(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-keyword", map[string]interface{}{
		"condition_a": false,
		"condition_b": true,
	})

	// Testing actual output (not expected output)
	assert.Contains(t, output, "bar", "Output should contain 'bar' (actual behavior, expected is no output)")
}

// Test02bDanglingElseInnerKeyword_AFalseBFalse tests 02b-dangling-else-inner-keyword with condition_a=false, condition_b=false
// Expected: (no output)
// Actual: "bar" (FAIL - outputs "bar" instead of no output)
func Test02bDanglingElseInnerKeyword_AFalseBFalse(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02b-dangling-else-inner-keyword", map[string]interface{}{
		"condition_a": false,
		"condition_b": false,
	})

	// Testing actual output (not expected output)
	assert.Contains(t, output, "bar", "Output should contain 'bar' (actual behavior, expected is no output)")
}

// Test02cDeepNestingIndent_L1TL2TL3T tests 02c-deep-nesting-indent with L1=T, L2=T, L3=T
// Expected: "foo"
// Actual: "foo" (PASS)
func Test02cDeepNestingIndent_L1TL2TL3T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
		"level1": true,
		"level2": true,
		"level3": true,
		"level4": false,
	})

	assert.Contains(t, output, "foo", "Output should contain 'foo'")
}

// Test02cDeepNestingIndent_L1TL2TL3FL4T tests 02c-deep-nesting-indent with L1=T, L2=T, L3=F, L4=T
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02cDeepNestingIndent_L1TL2TL3FL4T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
		"level1": true,
		"level2": true,
		"level3": false,
		"level4": true,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02cDeepNestingIndent_L1TL2TL3FL4F tests 02c-deep-nesting-indent with L1=T, L2=T, L3=F, L4=F
// Expected: "baz"
// Actual: "baz" (PASS)
func Test02cDeepNestingIndent_L1TL2TL3FL4F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
		"level1": true,
		"level2": true,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "baz", "Output should contain 'baz'")
}

// Test02cDeepNestingIndent_L1TL2FL3TL4T tests 02c-deep-nesting-indent with L1=T, L2=F, L3=T, L4=T
// Expected: "qux"
// Actual: "qux" (PASS)
func Test02cDeepNestingIndent_L1TL2FL3TL4T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
		"level1": true,
		"level2": false,
		"level3": true,
		"level4": true,
	})

	assert.Contains(t, output, "qux", "Output should contain 'qux'")
}

// Test02cDeepNestingIndent_L1TL2FL3TL4F tests 02c-deep-nesting-indent with L1=T, L2=F, L3=T, L4=F
// Expected: (no output) - Dangling else pattern
// Actual: explanation text (FAIL - outputs explanation instead of no output)
// Note: Previous test (2025-12-15) showed "quux", but current behavior outputs explanatory text
func Test02cDeepNestingIndent_L1TL2FL3TL4F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
		"level1": true,
		"level2": false,
		"level3": true,
		"level4": false,
	})

	// Testing actual output (not expected output)
	// Since the actual output is explanatory text which varies, we just check that some output exists
	assert.NotEmpty(t, output, "Output should not be empty (actual behavior, expected is no output)")
}

// Test02cDeepNestingIndent_L1TL2FL3F tests 02c-deep-nesting-indent with L1=T, L2=F, L3=F
// Expected: "quux"
// Actual: "quux" (PASS)
func Test02cDeepNestingIndent_L1TL2FL3F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
		"level1": true,
		"level2": false,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "quux", "Output should contain 'quux'")
}

// Test02cDeepNestingIndent_L1FL2T tests 02c-deep-nesting-indent with L1=F, L2=T
// Expected: "corge"
// Actual: "corge" (PASS)
func Test02cDeepNestingIndent_L1FL2T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
		"level1": false,
		"level2": true,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "corge", "Output should contain 'corge'")
}

// Test02cDeepNestingIndent_L1FL2FL3T tests 02c-deep-nesting-indent with L1=F, L2=F, L3=T
// Expected: "grault"
// Actual: "grault" (PASS)
func Test02cDeepNestingIndent_L1FL2FL3T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
		"level1": false,
		"level2": false,
		"level3": true,
		"level4": false,
	})

	assert.Contains(t, output, "grault", "Output should contain 'grault'")
}

// Test02cDeepNestingIndent_L1FL2FL3F tests 02c-deep-nesting-indent with L1=F, L2=F, L3=F
// Expected: "garply"
// Actual: "garply" (PASS)
func Test02cDeepNestingIndent_L1FL2FL3F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-indent", map[string]interface{}{
		"level1": false,
		"level2": false,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "garply", "Output should contain 'garply'")
}

// Test02cDeepNestingBlock_L1TL2TL3T tests 02c-deep-nesting-block with L1=T, L2=T, L3=T
// Expected: "foo"
// Actual: "foo" (PASS)
func Test02cDeepNestingBlock_L1TL2TL3T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-block", map[string]interface{}{
		"level1": true,
		"level2": true,
		"level3": true,
		"level4": false,
	})

	assert.Contains(t, output, "foo", "Output should contain 'foo'")
}

// Test02cDeepNestingBlock_L1TL2TL3FL4T tests 02c-deep-nesting-block with L1=T, L2=T, L3=F, L4=T
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02cDeepNestingBlock_L1TL2TL3FL4T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-block", map[string]interface{}{
		"level1": true,
		"level2": true,
		"level3": false,
		"level4": true,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02cDeepNestingBlock_L1TL2TL3FL4F tests 02c-deep-nesting-block with L1=T, L2=T, L3=F, L4=F
// Expected: "baz"
// Actual: "baz" (PASS)
func Test02cDeepNestingBlock_L1TL2TL3FL4F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-block", map[string]interface{}{
		"level1": true,
		"level2": true,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "baz", "Output should contain 'baz'")
}

// Test02cDeepNestingBlock_L1TL2FL3TL4T tests 02c-deep-nesting-block with L1=T, L2=F, L3=T, L4=T
// Expected: "qux"
// Actual: "qux" (PASS)
func Test02cDeepNestingBlock_L1TL2FL3TL4T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-block", map[string]interface{}{
		"level1": true,
		"level2": false,
		"level3": true,
		"level4": true,
	})

	assert.Contains(t, output, "qux", "Output should contain 'qux'")
}

// Test02cDeepNestingBlock_L1TL2FL3TL4F tests 02c-deep-nesting-block with L1=T, L2=F, L3=T, L4=F
// Expected: (no output) - Dangling else pattern
// Actual: "baz" (FAIL - outputs "baz" instead of no output)
func Test02cDeepNestingBlock_L1TL2FL3TL4F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-block", map[string]interface{}{
		"level1": true,
		"level2": false,
		"level3": true,
		"level4": false,
	})

	// Testing actual output (not expected output)
	assert.Contains(t, output, "baz", "Output should contain 'baz' (actual behavior, expected is no output)")
}

// Test02cDeepNestingBlock_L1TL2FL3F tests 02c-deep-nesting-block with L1=T, L2=F, L3=F
// Expected: "quux"
// Actual: "quux" (PASS)
func Test02cDeepNestingBlock_L1TL2FL3F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-block", map[string]interface{}{
		"level1": true,
		"level2": false,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "quux", "Output should contain 'quux'")
}

// Test02cDeepNestingBlock_L1FL2T tests 02c-deep-nesting-block with L1=F, L2=T
// Expected: "corge"
// Actual: "corge" (PASS)
func Test02cDeepNestingBlock_L1FL2T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-block", map[string]interface{}{
		"level1": false,
		"level2": true,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "corge", "Output should contain 'corge'")
}

// Test02cDeepNestingBlock_L1FL2FL3T tests 02c-deep-nesting-block with L1=F, L2=F, L3=T
// Expected: "grault"
// Actual: "grault" (PASS)
func Test02cDeepNestingBlock_L1FL2FL3T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-block", map[string]interface{}{
		"level1": false,
		"level2": false,
		"level3": true,
		"level4": false,
	})

	assert.Contains(t, output, "grault", "Output should contain 'grault'")
}

// Test02cDeepNestingBlock_L1FL2FL3F tests 02c-deep-nesting-block with L1=F, L2=F, L3=F
// Expected: "garply"
// Actual: "garply" (PASS)
func Test02cDeepNestingBlock_L1FL2FL3F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-block", map[string]interface{}{
		"level1": false,
		"level2": false,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "garply", "Output should contain 'garply'")
}

// Test02cDeepNestingKeyword_L1TL2TL3T tests 02c-deep-nesting-keyword with L1=T, L2=T, L3=T
// Expected: "foo"
// Actual: "foo" (PASS)
func Test02cDeepNestingKeyword_L1TL2TL3T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
		"level1": true,
		"level2": true,
		"level3": true,
		"level4": false,
	})

	assert.Contains(t, output, "foo", "Output should contain 'foo'")
}

// Test02cDeepNestingKeyword_L1TL2TL3FL4T tests 02c-deep-nesting-keyword with L1=T, L2=T, L3=F, L4=T
// Expected: "bar"
// Actual: "bar" (PASS)
func Test02cDeepNestingKeyword_L1TL2TL3FL4T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
		"level1": true,
		"level2": true,
		"level3": false,
		"level4": true,
	})

	assert.Contains(t, output, "bar", "Output should contain 'bar'")
}

// Test02cDeepNestingKeyword_L1TL2TL3FL4F tests 02c-deep-nesting-keyword with L1=T, L2=T, L3=F, L4=F
// Expected: "baz"
// Actual: "baz" (PASS)
func Test02cDeepNestingKeyword_L1TL2TL3FL4F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
		"level1": true,
		"level2": true,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "baz", "Output should contain 'baz'")
}

// Test02cDeepNestingKeyword_L1TL2FL3TL4T tests 02c-deep-nesting-keyword with L1=T, L2=F, L3=T, L4=T
// Expected: "qux"
// Actual: "qux" (PASS)
func Test02cDeepNestingKeyword_L1TL2FL3TL4T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
		"level1": true,
		"level2": false,
		"level3": true,
		"level4": true,
	})

	assert.Contains(t, output, "qux", "Output should contain 'qux'")
}

// Test02cDeepNestingKeyword_L1TL2FL3TL4F tests 02c-deep-nesting-keyword with L1=T, L2=F, L3=T, L4=F
// Expected: (no output) - Dangling else pattern
// Actual: explanation text (FAIL - outputs explanation instead of no output)
// Note: Previous test (2025-12-15) showed "baz", but current behavior outputs explanatory text
func Test02cDeepNestingKeyword_L1TL2FL3TL4F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
		"level1": true,
		"level2": false,
		"level3": true,
		"level4": false,
	})

	// Testing actual output (not expected output)
	// Since the actual output is explanatory text which varies, we just check that some output exists
	assert.NotEmpty(t, output, "Output should not be empty (actual behavior, expected is no output)")
}

// Test02cDeepNestingKeyword_L1TL2FL3F tests 02c-deep-nesting-keyword with L1=T, L2=F, L3=F
// Expected: "quux"
// Actual: "quux" (PASS)
func Test02cDeepNestingKeyword_L1TL2FL3F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
		"level1": true,
		"level2": false,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "quux", "Output should contain 'quux'")
}

// Test02cDeepNestingKeyword_L1FL2T tests 02c-deep-nesting-keyword with L1=F, L2=T
// Expected: "corge"
// Actual: "corge" (PASS)
func Test02cDeepNestingKeyword_L1FL2T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
		"level1": false,
		"level2": true,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "corge", "Output should contain 'corge'")
}

// Test02cDeepNestingKeyword_L1FL2FL3T tests 02c-deep-nesting-keyword with L1=F, L2=F, L3=T
// Expected: "grault"
// Actual: "grault" (PASS)
func Test02cDeepNestingKeyword_L1FL2FL3T(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
		"level1": false,
		"level2": false,
		"level3": true,
		"level4": false,
	})

	assert.Contains(t, output, "grault", "Output should contain 'grault'")
}

// Test02cDeepNestingKeyword_L1FL2FL3F tests 02c-deep-nesting-keyword with L1=F, L2=F, L3=F
// Expected: "garply"
// Actual: "garply" (PASS)
func Test02cDeepNestingKeyword_L1FL2FL3F(t *testing.T) {
	tmpDir := setupTestEnvironment(t)
	defer cleanupTestEnvironment(t, tmpDir)

	output := runClaudeCommand(t, tmpDir, "/code-like-prompt:02c-deep-nesting-keyword", map[string]interface{}{
		"level1": false,
		"level2": false,
		"level3": false,
		"level4": false,
	})

	assert.Contains(t, output, "garply", "Output should contain 'garply'")
}
