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
