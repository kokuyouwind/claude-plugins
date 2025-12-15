package code_like_prompt

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

// setupTestEnvironment creates a temporary directory with Claude configuration
// and returns the path to the temporary directory
func setupTestEnvironment(t *testing.T) string {
	t.Helper()

	// Create temporary directory under /tmp
	tmpDir, err := os.MkdirTemp("/tmp", "code-like-prompt-test-*")
	require.NoError(t, err, "Failed to create temporary directory")

	// Create .claude directory
	claudeDir := filepath.Join(tmpDir, ".claude")
	err = os.MkdirAll(claudeDir, 0755)
	require.NoError(t, err, "Failed to create .claude directory")

	// Get repository root path (assuming this test is in test/code-like-prompt)
	testDir, err := os.Getwd()
	require.NoError(t, err, "Failed to get working directory")
	repoPath := filepath.Join(testDir, "..", "..")
	absRepoPath, err := filepath.Abs(repoPath)
	require.NoError(t, err, "Failed to get absolute repository path")

	// Read settings template
	templatePath := filepath.Join(testDir, "testdata", "settings.json.template")
	templateContent, err := os.ReadFile(templatePath)
	require.NoError(t, err, "Failed to read settings template")

	// Replace {{REPO_PATH}} with actual repository path
	settingsContent := strings.ReplaceAll(string(templateContent), "{{REPO_PATH}}", absRepoPath)

	// Write settings.json
	settingsPath := filepath.Join(claudeDir, "settings.json")
	err = os.WriteFile(settingsPath, []byte(settingsContent), 0644)
	require.NoError(t, err, "Failed to write settings.json")

	return tmpDir
}

// cleanupTestEnvironment removes the temporary directory
func cleanupTestEnvironment(t *testing.T, tmpDir string) {
	t.Helper()

	err := os.RemoveAll(tmpDir)
	require.NoError(t, err, "Failed to remove temporary directory")
}

// runClaudeCommand executes a claude command with the given arguments
// and returns the standard output
func runClaudeCommand(t *testing.T, tmpDir string, command string, args map[string]interface{}) string {
	t.Helper()

	// Build command string with JSON arguments
	var cmdStr string
	if len(args) > 0 {
		argsJSON, err := json.Marshal(args)
		require.NoError(t, err, "Failed to marshal arguments to JSON")
		cmdStr = fmt.Sprintf("%s %s", command, string(argsJSON))
	} else {
		cmdStr = command
	}

	// Execute claude command in the temporary directory
	// This uses the project's .claude/settings.json without isolating user authentication
	cmd := exec.Command("claude", "-p", cmdStr)
	cmd.Dir = tmpDir
	cmd.Env = os.Environ() // Use parent environment including authentication

	output, err := cmd.CombinedOutput()
	require.NoError(t, err, "Failed to execute claude command: %s\nOutput: %s", cmdStr, string(output))

	return string(output)
}
