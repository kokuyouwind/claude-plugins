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
			Name:    "RecordFoo",
			Command: "/code-like-prompt:04b-structural-match",
			Args: map[string]interface{}{
				"record": "Record \"foo\" 5",
			},
			ExpectedOutputs: []string{
				"foo:5",
			},
		},
		{
			Name:    "RecordBarLarge",
			Command: "/code-like-prompt:04b-structural-match",
			Args: map[string]interface{}{
				"record": "Record \"bar\" 15",
			},
			ExpectedOutputs: []string{
				"bar-large",
			},
		},
		{
			Name:    "RecordBarSmall",
			Command: "/code-like-prompt:04b-structural-match",
			Args: map[string]interface{}{
				"record": "Record \"bar\" 5",
			},
			ExpectedOutputs: []string{
				"bar-small",
			},
		},
		{
			Name:    "RecordOther",
			Command: "/code-like-prompt:04b-structural-match",
			Args: map[string]interface{}{
				"record": "Record \"baz\" 10",
			},
			ExpectedOutputs: []string{
				"other:baz",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test04cListDestructure tests 04c-list-destructure command
func Test04cListDestructure(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "EmptyList",
			Command: "/code-like-prompt:04c-list-destructure",
			Args: map[string]interface{}{
				"list": "[]",
			},
			ExpectedOutputs: []string{
				"empty",
			},
		},
		{
			Name:    "SingleElement",
			Command: "/code-like-prompt:04c-list-destructure",
			Args: map[string]interface{}{
				"list": "[\"hello\"]",
			},
			ExpectedOutputs: []string{
				"single:hello",
			},
		},
		{
			Name:    "FooBar",
			Command: "/code-like-prompt:04c-list-destructure",
			Args: map[string]interface{}{
				"list": "[\"foo\",\"bar\"]",
			},
			ExpectedOutputs: []string{
				"foo-bar",
			},
		},
		{
			Name:    "FooXBaz",
			Command: "/code-like-prompt:04c-list-destructure",
			Args: map[string]interface{}{
				"list": "[\"foo\",\"test\",\"baz\"]",
			},
			ExpectedOutputs: []string{
				"foo-x-baz:test",
			},
		},
		{
			Name:    "MultipleElements",
			Command: "/code-like-prompt:04c-list-destructure",
			Args: map[string]interface{}{
				"list": "[\"a\",\"b\",\"c\"]",
			},
			ExpectedOutputs: []string{
				"multi:a,b",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test04dNestedMatch tests 04d-nested-match command
func Test04dNestedMatch(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "LeafNode",
			Command: "/code-like-prompt:04d-nested-match",
			Args: map[string]interface{}{
				"tree": "Leaf \"hello\"",
			},
			ExpectedOutputs: []string{
				"leaf:hello",
			},
		},
		{
			Name:    "FooBarBranch",
			Command: "/code-like-prompt:04d-nested-match",
			Args: map[string]interface{}{
				"tree": "Branch (Leaf \"foo\") (Leaf \"bar\")",
			},
			ExpectedOutputs: []string{
				"foo-bar",
			},
		},
		{
			Name:    "FooAnyBranch",
			Command: "/code-like-prompt:04d-nested-match",
			Args: map[string]interface{}{
				"tree": "Branch (Leaf \"foo\") (Leaf \"baz\")",
			},
			ExpectedOutputs: []string{
				"foo-any",
			},
		},
		{
			Name:    "AnyBarBranch",
			Command: "/code-like-prompt:04d-nested-match",
			Args: map[string]interface{}{
				"tree": "Branch (Leaf \"test\") (Leaf \"bar\")",
			},
			ExpectedOutputs: []string{
				"any-bar",
			},
		},
		{
			Name:    "NestedBranch",
			Command: "/code-like-prompt:04d-nested-match",
			Args: map[string]interface{}{
				"tree": "Branch (Branch (Leaf \"a\") (Leaf \"b\")) (Leaf \"c\")",
			},
			ExpectedOutputs: []string{
				"nested",
			},
		},
		{
			Name:    "OtherBranch",
			Command: "/code-like-prompt:04d-nested-match",
			Args: map[string]interface{}{
				"tree": "Branch (Leaf \"x\") (Leaf \"y\")",
			},
			ExpectedOutputs: []string{
				"other",
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
				"red",
			},
		},
		{
			Name:    "Green",
			Command: "/code-like-prompt:04f-exhaustive",
			Args: map[string]interface{}{
				"color": "Green",
			},
			ExpectedOutputs: []string{
				"green",
			},
		},
		{
			Name:    "Blue",
			Command: "/code-like-prompt:04f-exhaustive",
			Args: map[string]interface{}{
				"color": "Blue",
			},
			ExpectedOutputs: []string{
				"blue",
			},
		},
		{
			Name:    "RGBBright",
			Command: "/code-like-prompt:04f-exhaustive",
			Args: map[string]interface{}{
				"color": "RGB 255 100 50",
			},
			ExpectedOutputs: []string{
				"bright",
			},
		},
		{
			Name:    "RGBDark",
			Command: "/code-like-prompt:04f-exhaustive",
			Args: map[string]interface{}{
				"color": "RGB 100 150 200",
			},
			ExpectedOutputs: []string{
				"rgb:100,150,200",
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

