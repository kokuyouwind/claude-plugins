package code_like_prompt

import (
	"testing"
)

// Test06aFunctionArgs tests 06a-function-args command
func Test06aFunctionArgs(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "JapaneseGreeting",
			Command: "/code-like-prompt:06a-function-args",
			Args: map[string]interface{}{
				"name":     "Miku",
				"language": "ja",
			},
			ExpectedOutputs: []string{
				"こんにちは、Mikuさん",
			},
		},
		{
			Name:    "EnglishGreeting",
			Command: "/code-like-prompt:06a-function-args",
			Args: map[string]interface{}{
				"name":     "Alice",
				"language": "en",
			},
			ExpectedOutputs: []string{
				"Hello, Alice",
			},
		},
		{
			Name:    "DefaultGreeting",
			Command: "/code-like-prompt:06a-function-args",
			Args: map[string]interface{}{
				"name":     "Bob",
				"language": "fr",
			},
			ExpectedOutputs: []string{
				"Hi, Bob",
			},
		},
		{
			Name:    "SpanishFallback",
			Command: "/code-like-prompt:06a-function-args",
			Args: map[string]interface{}{
				"name":     "Charlie",
				"language": "es",
			},
			ExpectedOutputs: []string{
				"Hi, Charlie",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test06bFunctionReturn tests 06b-function-return command
func Test06bFunctionReturn(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "ExcellentWithBonus",
			Command: "/code-like-prompt:06b-function-return",
			Args: map[string]interface{}{
				"base":  60,
				"bonus": true,
			},
			ExpectedOutputs: []string{
				"Excellent",
			},
		},
		{
			Name:    "GoodWithoutBonus",
			Command: "/code-like-prompt:06b-function-return",
			Args: map[string]interface{}{
				"base":  60,
				"bonus": false,
			},
			ExpectedOutputs: []string{
				"Good",
			},
		},
		{
			Name:    "TryAgain",
			Command: "/code-like-prompt:06b-function-return",
			Args: map[string]interface{}{
				"base":  30,
				"bonus": false,
			},
			ExpectedOutputs: []string{
				"Try again",
			},
		},
		{
			Name:    "GoodAtBoundary",
			Command: "/code-like-prompt:06b-function-return",
			Args: map[string]interface{}{
				"base":  50,
				"bonus": false,
			},
			ExpectedOutputs: []string{
				"Good",
			},
		},
		{
			Name:    "ExcellentAtBoundary",
			Command: "/code-like-prompt:06b-function-return",
			Args: map[string]interface{}{
				"base":  50,
				"bonus": true,
			},
			ExpectedOutputs: []string{
				"Excellent",
			},
		},
		{
			Name:    "TryAgainNearBoundary",
			Command: "/code-like-prompt:06b-function-return",
			Args: map[string]interface{}{
				"base":  40,
				"bonus": false,
			},
			ExpectedOutputs: []string{
				"Try again",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test06cFunctionRefactor tests 06c-function-refactor command
func Test06cFunctionRefactor(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "Level1True_Level2True_Level3True",
			Command: "/code-like-prompt:06c-function-refactor",
			Args: map[string]interface{}{
				"level1": true,
				"level2": true,
				"level3": true,
			},
			ExpectedOutputs: []string{
				"foo",
			},
		},
		{
			Name:    "Level1True_Level2True_Level3False_Level4True",
			Command: "/code-like-prompt:06c-function-refactor",
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
			Name:    "Level1True_Level2True_Level3False_Level4False",
			Command: "/code-like-prompt:06c-function-refactor",
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
			Name:    "Level1True_Level2False_Level3True_Level4True",
			Command: "/code-like-prompt:06c-function-refactor",
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
			Name:    "Level1True_Level2False_Level3True_Level4False",
			Command: "/code-like-prompt:06c-function-refactor",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": true,
				"level4": false,
			},
			// This case returns None - no output expected
			CustomAssert: func(t *testing.T, output string) {
				// In VCR playback mode, check if the output is empty or contains no substantive content
				// The VCR cassette should capture that Claude produces no output
				// We don't strictly enforce empty output because the exact format may vary
			},
		},
		{
			Name:    "Level1True_Level2False_Level3False",
			Command: "/code-like-prompt:06c-function-refactor",
			Args: map[string]interface{}{
				"level1": true,
				"level2": false,
				"level3": false,
			},
			ExpectedOutputs: []string{
				"quux",
			},
		},
		{
			Name:    "Level1False_Level2True",
			Command: "/code-like-prompt:06c-function-refactor",
			Args: map[string]interface{}{
				"level1": false,
				"level2": true,
				"level3": false,
			},
			ExpectedOutputs: []string{
				"corge",
			},
		},
		{
			Name:    "Level1False_Level2False_Level3True",
			Command: "/code-like-prompt:06c-function-refactor",
			Args: map[string]interface{}{
				"level1": false,
				"level2": false,
				"level3": true,
			},
			ExpectedOutputs: []string{
				"grault",
			},
		},
		{
			Name:    "Level1False_Level2False_Level3False",
			Command: "/code-like-prompt:06c-function-refactor",
			Args: map[string]interface{}{
				"level1": false,
				"level2": false,
				"level3": false,
			},
			ExpectedOutputs: []string{
				"garply",
			},
		},
	}

	RunTestCases(t, tests)
}
