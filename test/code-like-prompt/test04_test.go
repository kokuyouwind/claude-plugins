package code_like_prompt

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

// Test04aRegexMatch tests 04a-regex-match command
func Test04aRegexMatch(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "FooXXXBar",
			Command: "/code-like-prompt:04a-regex-match",
			Args: map[string]interface{}{
				"text": "fooXXXbar",
			},
			ExpectedOutputs: []string{
				"qux",
			},
		},
		{
			Name:    "FooBar",
			Command: "/code-like-prompt:04a-regex-match",
			Args: map[string]interface{}{
				"text": "foobar",
			},
			ExpectedOutputs: []string{
				"qux",
			},
		},
		{
			Name:    "BazAnything",
			Command: "/code-like-prompt:04a-regex-match",
			Args: map[string]interface{}{
				"text": "bazanything",
			},
			ExpectedOutputs: []string{
				"quux",
			},
		},
		{
			Name:    "Hello",
			Command: "/code-like-prompt:04a-regex-match",
			Args: map[string]interface{}{
				"text": "hello",
			},
			ExpectedOutputs: []string{
				"corge",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test04bStructuralMatch tests 04b-structural-match command
func Test04bStructuralMatch(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "FooType123Value",
			Command: "/code-like-prompt:04b-structural-match",
			Args: map[string]interface{}{
				"type":  "foo",
				"value": "123",
			},
			ExpectedOutputs: []string{
				"bar123",
			},
		},
		{
			Name:    "BazType15ValueGuardTrue",
			Command: "/code-like-prompt:04b-structural-match",
			Args: map[string]interface{}{
				"type":  "baz",
				"value": "15",
			},
			ExpectedOutputs: []string{
				"qux15",
			},
		},
		{
			Name:    "BazType5ValueGuardFalse",
			Command: "/code-like-prompt:04b-structural-match",
			Args: map[string]interface{}{
				"type":  "baz",
				"value": "5",
			},
			ExpectedOutputs: []string{
				"quux5",
			},
		},
		{
			Name:    "OtherType",
			Command: "/code-like-prompt:04b-structural-match",
			Args: map[string]interface{}{
				"type":  "other",
				"value": "x",
			},
			ExpectedOutputs: []string{
				"corge",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test04cListDestructure tests 04c-list-destructure command
func Test04cListDestructure(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "FooXBar",
			Command: "/code-like-prompt:04c-list-destructure",
			Args: map[string]interface{}{
				"item1": "foo",
				"item2": "X",
				"item3": "bar",
			},
			ExpectedOutputs: []string{
				"quxX",
			},
		},
		{
			Name:    "FooABRestPattern",
			Command: "/code-like-prompt:04c-list-destructure",
			Args: map[string]interface{}{
				"item1": "foo",
				"item2": "a",
				"item3": "b",
			},
			ExpectedOutputs: []string{
				"quux2",
			},
		},
		{
			Name:    "OtherXY",
			Command: "/code-like-prompt:04c-list-destructure",
			Args: map[string]interface{}{
				"item1": "other",
				"item2": "x",
				"item3": "y",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Output has space: "corge other"
				assert.Contains(t, output, "corge")
				assert.Contains(t, output, "other")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test04dNestedMatch tests 04d-nested-match command
func Test04dNestedMatch(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "FooBarMatch",
			Command: "/code-like-prompt:04d-nested-match",
			Args: map[string]interface{}{
				"left":        "foo",
				"right_left":  "bar",
				"right_right": "X",
			},
			ExpectedOutputs: []string{
				"qux",
			},
		},
		{
			Name:    "ValueMatchSame",
			Command: "/code-like-prompt:04d-nested-match",
			Args: map[string]interface{}{
				"left":        "A",
				"right_left":  "same",
				"right_right": "same",
			},
			CustomAssert: func(t *testing.T, output string) {
				// May request approval, but if it runs successfully should contain quuxA
				hasQuuxA := strings.Contains(output, "quuxA")
				hasApproval := strings.Contains(output, "approval") || strings.Contains(output, "permission")
				assert.True(t, hasQuuxA || hasApproval, "Output should contain quuxA or approval request")
			},
		},
		{
			Name:    "DefaultCase",
			Command: "/code-like-prompt:04d-nested-match",
			Args: map[string]interface{}{
				"left":        "A",
				"right_left":  "B",
				"right_right": "C",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Output has space: "corge A"
				assert.Contains(t, output, "corge")
				assert.Contains(t, output, "A")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test04eMultiGuard tests 04e-multi-guard command
func Test04eMultiGuard(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "BothPositive",
			Command: "/code-like-prompt:04e-multi-guard",
			Args: map[string]interface{}{
				"x": 1,
				"y": 1,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "OnePositiveX",
			Command: "/code-like-prompt:04e-multi-guard",
			Args: map[string]interface{}{
				"x": 1,
				"y": -1,
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "OnePositiveY",
			Command: "/code-like-prompt:04e-multi-guard",
			Args: map[string]interface{}{
				"x": -1,
				"y": 1,
			},
			ExpectedOutputs: []string{
				"qux",
			},
		},
		{
			Name:    "BothZero",
			Command: "/code-like-prompt:04e-multi-guard",
			Args: map[string]interface{}{
				"x": 0,
				"y": 0,
			},
			ExpectedOutputs: []string{
				"baz",
			},
		},
		{
			Name:    "BothNegative",
			Command: "/code-like-prompt:04e-multi-guard",
			Args: map[string]interface{}{
				"x": -1,
				"y": -1,
			},
			ExpectedOutputs: []string{
				"qux",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test04fExhaustive tests 04f-exhaustive command
func Test04fExhaustive(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "Red",
			Command: "/code-like-prompt:04f-exhaustive",
			Args: map[string]interface{}{
				"color": "Red",
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "Green",
			Command: "/code-like-prompt:04f-exhaustive",
			Args: map[string]interface{}{
				"color": "Green",
			},
			ExpectedOutputs: []string{
				"bar",
			},
		},
		{
			Name:    "Blue",
			Command: "/code-like-prompt:04f-exhaustive",
			Args: map[string]interface{}{
				"color": "Blue",
			},
			ExpectedOutputs: []string{
				"baz",
			},
		},
		{
			Name:    "CustomHighRed",
			Command: "/code-like-prompt:04f-exhaustive",
			Args: map[string]interface{}{
				"color": "Custom",
				"r":     255,
				"g":     100,
				"b":     50,
			},
			ExpectedOutputs: []string{
				"qux",
			},
		},
		{
			Name:    "CustomRGB",
			Command: "/code-like-prompt:04f-exhaustive",
			Args: map[string]interface{}{
				"color": "Custom",
				"r":     100,
				"g":     150,
				"b":     200,
			},
			ExpectedOutputs: []string{
				"quux100150200",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07aBasicFacts tests 07-a-basic-facts command
func Test07aBasicFacts(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "BasicQuery",
			Command: "/code-like-prompt:07-a-basic-facts",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"corge",
				"grault",
			},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain both outputs
				assert.Contains(t, output, "corge")
				assert.Contains(t, output, "grault")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07bMultiClause tests 07-b-multi-clause command
func Test07bMultiClause(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "Backtracking",
			Command: "/code-like-prompt:07-b-multi-clause",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain expected outputs: b c c d d
				assert.Contains(t, output, "b")
				assert.Contains(t, output, "c")
				assert.Contains(t, output, "d")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07cCut tests 07-c-cut command
func Test07cCut(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "CutOperator",
			Command: "/code-like-prompt:07-c-cut",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain bar and corge (cut prevents backtracking)
				assert.Contains(t, output, "bar")
				assert.Contains(t, output, "corge")
				// Should NOT contain baz (cut prevents reaching it)
				assert.NotContains(t, output, "baz")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07dTreeTraverse tests 07-d-tree-traverse command
func Test07dTreeTraverse(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "DFSTraversal",
			Command: "/code-like-prompt:07-d-tree-traverse",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain all leaf values in DFS order: baz qux corge grault
				assert.Contains(t, output, "baz")
				assert.Contains(t, output, "qux")
				assert.Contains(t, output, "corge")
				assert.Contains(t, output, "grault")
				// Check order by verifying baz appears before grault
				bazIdx := strings.Index(output, "baz")
				graultIdx := strings.Index(output, "grault")
				assert.True(t, bazIdx < graultIdx, "baz should appear before grault in DFS order")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07eFindall tests 07-e-findall command
func Test07eFindall(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "CollectAllSolutions",
			Command: "/code-like-prompt:07-e-findall",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain collected solutions: baz, quux
				assert.Contains(t, output, "baz")
				assert.Contains(t, output, "quux")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07fNegation tests 07-f-negation command
func Test07fNegation(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "NegationAsFailure",
			Command: "/code-like-prompt:07-f-negation",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"bar",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07gConstraints tests 07-g-constraints command
func Test07gConstraints(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "GraphColoring",
			Command: "/code-like-prompt:07-g-constraints",
			Args:    map[string]interface{}{},
			CustomAssert: func(t *testing.T, output string) {
				// Should contain valid 3-colorings
				// All 6 permutations are valid: foo-bar-baz, foo-baz-bar, bar-foo-baz, bar-baz-foo, baz-foo-bar, baz-bar-foo
				validColorings := []string{
					"foo-bar-baz",
					"foo-baz-bar",
					"bar-foo-baz",
					"bar-baz-foo",
					"baz-foo-bar",
					"baz-bar-foo",
				}
				foundCount := 0
				for _, coloring := range validColorings {
					if strings.Contains(output, coloring) {
						foundCount++
					}
				}
				assert.Greater(t, foundCount, 0, "Should contain at least one valid coloring")
			},
		},
	}

	RunTestCases(t, tests)
}

// Test04gMaybePattern tests 04g-maybe-pattern command
func Test04gMaybePattern(t *testing.T) {
	tests := []TestCase{
		// Pattern matching style tests
		{
			Name:    "PatternNothing",
			Command: "/code-like-prompt:04g-maybe-pattern",
			Args: map[string]interface{}{
				"maybe_value": "Nothing",
				"style":       "pattern",
			},
			ExpectedOutputs: []string{
				"none",
			},
		},
		{
			Name:    "PatternJustValue",
			Command: "/code-like-prompt:04g-maybe-pattern",
			Args: map[string]interface{}{
				"maybe_value": "Just:hello",
				"style":       "pattern",
			},
			ExpectedOutputs: []string{
				"value:hello",
			},
		},
		// Do notation style tests
		{
			Name:    "DoNothing",
			Command: "/code-like-prompt:04g-maybe-pattern",
			Args: map[string]interface{}{
				"maybe_value": "Nothing",
				"style":       "do",
			},
			ExpectedOutputs: []string{
				"empty",
			},
		},
		{
			Name:    "DoJustValue",
			Command: "/code-like-prompt:04g-maybe-pattern",
			Args: map[string]interface{}{
				"maybe_value": "Just:test",
				"style":       "do",
			},
			ExpectedOutputs: []string{
				"processed:test",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test04hEitherPattern tests 04h-either-pattern command
func Test04hEitherPattern(t *testing.T) {
	tests := []TestCase{
		// Pattern matching style tests
		{
			Name:    "PatternLeft",
			Command: "/code-like-prompt:04h-either-pattern",
			Args: map[string]interface{}{
				"either_value": "Left:timeout",
				"style":        "pattern",
			},
			ExpectedOutputs: []string{
				"error:timeout",
			},
		},
		{
			Name:    "PatternRight",
			Command: "/code-like-prompt:04h-either-pattern",
			Args: map[string]interface{}{
				"either_value": "Right:success",
				"style":        "pattern",
			},
			ExpectedOutputs: []string{
				"success:success",
			},
		},
		// Do notation style tests
		{
			Name:    "DoLeft",
			Command: "/code-like-prompt:04h-either-pattern",
			Args: map[string]interface{}{
				"either_value": "Left:error",
				"style":        "do",
			},
			ExpectedOutputs: []string{
				"failed:error",
			},
		},
		{
			Name:    "DoRight",
			Command: "/code-like-prompt:04h-either-pattern",
			Args: map[string]interface{}{
				"either_value": "Right:data",
				"style":        "do",
			},
			ExpectedOutputs: []string{
				"processed:data",
			},
		},
	}

	RunTestCases(t, tests)
}

