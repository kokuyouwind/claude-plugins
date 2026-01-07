package code_like_prompt

import (
	"testing"
)

// Test07aBasicFacts tests the 07-a-basic-facts command
func Test07aBasicFacts(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "basic_facts_query",
			Command: "/code-like-prompt:07-a-basic-facts",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"corge",
				"grault",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07bMultiClause tests the 07-b-multi-clause command
func Test07bMultiClause(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "path_finding_query",
			Command: "/code-like-prompt:07-b-multi-clause",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"b c c d d",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07cCut tests the 07-c-cut command
func Test07cCut(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "cut_operator_query",
			Command: "/code-like-prompt:07-c-cut",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"bar",
				"corge",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07dTreeTraverse tests the 07-d-tree-traverse command
func Test07dTreeTraverse(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "tree_traversal_query",
			Command: "/code-like-prompt:07-d-tree-traverse",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"baz qux corge grault",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07eFindall tests the 07-e-findall command
func Test07eFindall(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "findall_query",
			Command: "/code-like-prompt:07-e-findall",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"[baz, quux]",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07fNegation tests the 07-f-negation command
func Test07fNegation(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "negation_as_failure_query",
			Command: "/code-like-prompt:07-f-negation",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"bar",
			},
		},
	}

	RunTestCases(t, tests)
}

// Test07gConstraints tests the 07-g-constraints command
func Test07gConstraints(t *testing.T) {
	tests := []TestCase{
		{
			Name:    "graph_coloring_query",
			Command: "/code-like-prompt:07-g-constraints",
			Args:    map[string]interface{}{},
			ExpectedOutputs: []string{
				"foo-bar-baz",
				"foo-baz-bar",
				"bar-foo-baz",
				"bar-baz-foo",
				"baz-foo-bar",
				"baz-bar-foo",
			},
		},
	}

	RunTestCases(t, tests)
}
