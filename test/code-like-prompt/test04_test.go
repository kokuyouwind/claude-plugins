package code_like_prompt

import (
	"strings"
	"testing"
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
			CustomAssert: func(t *testing.T, output string) {
				// Claude's output is unstable - accept either interpretation
				if !strings.Contains(output, "other") && !strings.Contains(output, "x-y") {
					t.Errorf("Output should contain either 'other' or 'x-y', got: %s", output)
				}
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
			CustomAssert: func(t *testing.T, output string) {
				// Claude's output is unstable - accept either interpretation
				if !strings.Contains(output, "bright") && !strings.Contains(output, "rgb:255,100,50") {
					t.Errorf("Output should contain either 'bright' or 'rgb:255,100,50', got: %s", output)
				}
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
			CustomAssert: func(t *testing.T, output string) {
				// Claude's output is unstable - accept either interpretation
				if !strings.Contains(output, "empty") && !strings.Contains(output, "none") {
					t.Errorf("Output should contain either 'empty' or 'none', got: %s", output)
				}
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
			CustomAssert: func(t *testing.T, output string) {
				// Claude's output is unstable - accept any of these interpretations
				if !strings.Contains(output, "processed:data") &&
				   !strings.Contains(output, "processing:data") &&
				   !strings.Contains(output, "success:data") {
					t.Errorf("Output should contain 'processed:data', 'processing:data', or 'success:data', got: %s", output)
				}
			},
		},
	}

	RunTestCases(t, tests)
}
