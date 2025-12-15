package code_like_prompt

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

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
