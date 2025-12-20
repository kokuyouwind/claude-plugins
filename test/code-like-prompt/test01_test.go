package code_like_prompt

import (
	"testing"
)

// Test01ShoppingRequest tests the 01-shopping-request command
func Test01ShoppingRequest(t *testing.T) {
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
	}

	RunTestCases(t, tests)
}

// Test01ShoppingMisunderstanding tests the 01-shopping-misunderstanding command
func Test01ShoppingMisunderstanding(t *testing.T) {
	tests := []TestCase{
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

	RunTestCases(t, tests)
}
