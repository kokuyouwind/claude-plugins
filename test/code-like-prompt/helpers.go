package code_like_prompt

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestCase defines a test case structure for code-like-prompt commands
type TestCase struct {
	Name            string
	Command         string
	Args            map[string]interface{}
	ExpectedOutputs []string
	CustomAssert    func(t *testing.T, output string) // Optional custom assertion function
}

// vcrProxy holds the VCR proxy process information
type vcrProxy struct {
	cmd  *exec.Cmd
	port string
}

var globalProxy *vcrProxy
var globalTmpDir string

// startVCRProxy starts the VCR proxy unless DISABLE_BOOT_VCR environment variable is set
func startVCRProxy(t *testing.T) *vcrProxy {
	t.Helper()

	// Check if VCR should be disabled
	if os.Getenv("DISABLE_BOOT_VCR") != "" {
		return nil
	}

	// Get VCR mode (default: auto)
	mode := os.Getenv("BOOT_VCR_MODE")
	if mode == "" {
		mode = "auto"
	}

	// Get repository root path
	testDir, err := os.Getwd()
	require.NoError(t, err, "Failed to get working directory")
	repoPath := filepath.Join(testDir, "..", "..")
	absRepoPath, err := filepath.Abs(repoPath)
	require.NoError(t, err, "Failed to get absolute repository path")

	// VCR script path
	vcrScript := filepath.Join(absRepoPath, "test", "vcr", "claude_vcr.py")

	// Start mitmdump with VCR script
	port := "8001"
	cmd := exec.Command("mitmdump",
		"-s", vcrScript,
		"--mode", "reverse:https://api.anthropic.com",
		"--listen-port", port)

	// Set VCR mode environment variable
	cmd.Env = append(os.Environ(), fmt.Sprintf("VCR_MODE=%s", mode))

	// Create log file for proxy output
	logFile, err := os.Create("/tmp/vcr_proxy.log")
	require.NoError(t, err, "Failed to create proxy log file")
	cmd.Stdout = logFile
	cmd.Stderr = logFile

	// Start the process
	err = cmd.Start()
	require.NoError(t, err, "Failed to start VCR proxy")

	// Wait a bit for the proxy to start
	time.Sleep(3 * time.Second)

	return &vcrProxy{
		cmd:  cmd,
		port: port,
	}
}

// stopVCRProxy stops the VCR proxy
func stopVCRProxy(t *testing.T, proxy *vcrProxy) {
	t.Helper()

	if proxy == nil || proxy.cmd == nil || proxy.cmd.Process == nil {
		return
	}

	err := proxy.cmd.Process.Kill()
	require.NoError(t, err, "Failed to kill VCR proxy process")

	// Wait for process to exit
	_, _ = proxy.cmd.Process.Wait()
}

// setupTestEnvironmentOnce creates a temporary directory with Claude configuration
// and returns the path to the temporary directory
// This function should be called once in TestMain
func setupTestEnvironmentOnce(t *testing.T) string {
	t.Helper()

	// Use fixed directory path for consistent cache hits
	tmpDir := "/tmp/code-like-prompt-test"

	// Remove existing directory if it exists
	if _, err := os.Stat(tmpDir); err == nil {
		err = os.RemoveAll(tmpDir)
		require.NoError(t, err, "Failed to remove existing test directory")
	}

	// Create the directory
	err := os.MkdirAll(tmpDir, 0755)
	require.NoError(t, err, "Failed to create test directory")

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
	cmdArgs := []string{"--model", "claude-haiku-4-5-20251001"}

	// Add verbose flag if DEBUG environment variable is set
	if os.Getenv("DEBUG") != "" {
		cmdArgs = append(cmdArgs, "--verbose")
	}

	cmdArgs = append(cmdArgs, "-p", cmdStr)

	cmd := exec.Command("claude", cmdArgs...)
	cmd.Dir = tmpDir
	cmd.Env = os.Environ() // Use parent environment including authentication

	// Log the command being executed if DEBUG is set
	if os.Getenv("DEBUG") != "" {
		t.Logf("Executing command: claude %v", cmdArgs)
	}

	// If VCR proxy is running, set ANTHROPIC_BASE_URL
	if globalProxy != nil {
		cmd.Env = append(cmd.Env, fmt.Sprintf("ANTHROPIC_BASE_URL=http://localhost:%s", globalProxy.port))
	}

	output, err := cmd.CombinedOutput()
	require.NoError(t, err, "Failed to execute claude command: %s\nOutput: %s", cmdStr, string(output))

	// Log the output if DEBUG is set
	if os.Getenv("DEBUG") != "" {
		t.Logf("Command output:\n%s", string(output))
	}

	return string(output)
}

// RunTestCases executes a list of test cases with the standard test pattern
func RunTestCases(t *testing.T, tests []TestCase) {
	t.Helper()

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Execute using the global test directory
			output := runClaudeCommand(t, globalTmpDir, tt.Command, tt.Args)

			// Assert
			if tt.CustomAssert != nil {
				// Use custom assertion if provided
				tt.CustomAssert(t, output)
			} else {
				// Default assertion: check all expected outputs
				for _, expected := range tt.ExpectedOutputs {
					assert.Contains(t, output, expected, "Output should contain expected text")
				}
			}
		})
	}
}
