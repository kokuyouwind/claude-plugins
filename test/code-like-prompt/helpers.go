package code_like_prompt

import (
	"bufio"
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

// startVCRProxyForTest starts the VCR proxy for a specific test case
func startVCRProxyForTest(t *testing.T, cassetteName string) *vcrProxy {
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

	// Set VCR mode and cassette name environment variables
	cmd.Env = append(os.Environ(),
		fmt.Sprintf("VCR_MODE=%s", mode),
		fmt.Sprintf("VCR_CASSETTE=%s", cassetteName))

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

// startVCRProxy starts the VCR proxy unless DISABLE_BOOT_VCR environment variable is set
// This is kept for backward compatibility with TestMain
func startVCRProxy(t *testing.T) *vcrProxy {
	return startVCRProxyForTest(t, "default")
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

	// Wait a bit for the port to be released
	time.Sleep(1 * time.Second)
}

// setupTestEnvironmentForCase creates a temporary directory for a specific test case
// with Claude configuration and returns the path to the temporary directory
func setupTestEnvironmentForCase(t *testing.T, testName, caseName string) string {
	t.Helper()

	// Create test-specific directory path
	tmpDir := filepath.Join("/tmp/claude-plugin-test", testName, caseName)

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

// ClaudeResponse represents the JSON response from claude --output-format json
type ClaudeResponse struct {
	Result    string `json:"result"`
	SessionID string `json:"session_id"`
}

// runClaudeCommandWithProxy executes a claude command with the given arguments and proxy
// and returns the standard output
func runClaudeCommandWithProxy(t *testing.T, tmpDir string, proxy *vcrProxy, command string, args map[string]interface{}) string {
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

	// Always use JSON output format for session ID extraction
	cmdArgs := []string{"--model", "claude-haiku-4-5-20251001", "-p", cmdStr, "--output-format", "json"}

	// Execute claude command in the temporary directory
	cmd := exec.Command("claude", cmdArgs...)
	cmd.Dir = tmpDir
	cmd.Env = os.Environ() // Use parent environment including authentication

	// If VCR proxy is running, set ANTHROPIC_BASE_URL
	if proxy != nil {
		cmd.Env = append(cmd.Env, fmt.Sprintf("ANTHROPIC_BASE_URL=http://localhost:%s", proxy.port))
	}

	output, err := cmd.CombinedOutput()
	require.NoError(t, err, "Failed to execute claude command: %s\nOutput: %s", cmdStr, string(output))

	// Parse JSON response to extract session ID and result
	var response ClaudeResponse
	var textOutput string
	var sessionID string

	if err := json.Unmarshal(output, &response); err == nil {
		textOutput = response.Result
		sessionID = response.SessionID
	} else {
		// Fallback to raw output if JSON parsing fails
		textOutput = string(output)
	}

	// If DEBUG=1, print session ID and sidechain messages
	if os.Getenv("DEBUG") == "1" && sessionID != "" {
		fmt.Printf("session-id: %s\n", sessionID)

		// Wait a bit for session file to be written
		time.Sleep(500 * time.Millisecond)

		// Load and print sidechain messages
		messages := loadSidechainMessages(t, tmpDir, sessionID)
		printSidechainMessages(messages)
	}

	return textOutput
}

// runClaudeCommand executes a claude command with the given arguments
// and returns the standard output
func runClaudeCommand(t *testing.T, tmpDir string, command string, args map[string]interface{}) string {
	return runClaudeCommandWithProxy(t, tmpDir, globalProxy, command, args)
}

// Message represents a message in the session history
type Message struct {
	IsSidechain bool   `json:"isSidechain"`
	UserType    string `json:"userType"`
	Type        string `json:"type"`
	Message     struct {
		Role    string        `json:"role"`
		Content []ContentItem `json:"content"`
	} `json:"message"`
	Timestamp string `json:"timestamp"`
	SessionID string `json:"sessionId"`
}

// ContentItem represents a content item in a message
type ContentItem struct {
	Type      string                 `json:"type"`
	Text      string                 `json:"text"`
	ID        string                 `json:"id"`
	Name      string                 `json:"name"`
	Input     map[string]interface{} `json:"input"`
	Content   interface{}            `json:"content"`
	IsError   bool                   `json:"is_error"`
	ToolUseID string                 `json:"tool_use_id"`
}

// getProjectDirName converts a directory path to the project directory name used by Claude
func getProjectDirName(dir string) string {
	// Resolve to absolute path
	absDir, err := filepath.Abs(dir)
	if err != nil {
		return ""
	}

	// Resolve symlinks (e.g., /tmp -> /private/tmp on macOS)
	realDir, err := filepath.EvalSymlinks(absDir)
	if err != nil {
		return ""
	}

	// Remove leading / then replace remaining / with -
	// Then add - prefix to match Claude's format
	projectName := strings.TrimPrefix(realDir, "/")
	projectName = strings.ReplaceAll(projectName, "/", "-")
	projectName = "-" + projectName

	return projectName
}

// loadSidechainMessages loads sidechain messages from session file
func loadSidechainMessages(t *testing.T, tmpDir string, sessionID string) []Message {
	t.Helper()

	if sessionID == "" {
		return nil
	}

	// Get project directory name
	projectDirName := getProjectDirName(tmpDir)
	if projectDirName == "" {
		return nil
	}

	// Session file path
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return nil
	}

	sessionFile := filepath.Join(homeDir, ".claude", "projects", projectDirName, sessionID+".jsonl")

	// Check if file exists
	if _, err := os.Stat(sessionFile); os.IsNotExist(err) {
		return nil
	}

	// Read JSONL file
	file, err := os.Open(sessionFile)
	if err != nil {
		return nil
	}
	defer file.Close()

	var messages []Message
	scanner := bufio.NewScanner(file)

	// Increase buffer size for large messages
	buf := make([]byte, 0, 1024*1024)
	scanner.Buffer(buf, 10*1024*1024)

	for scanner.Scan() {
		var msg Message
		if err := json.Unmarshal(scanner.Bytes(), &msg); err != nil {
			continue
		}

		// Filter: user input, internal thoughts (userType: "internal"), or assistant output
		if msg.Type == "user" || msg.UserType == "internal" || msg.Type == "assistant" {
			messages = append(messages, msg)
		}
	}

	return messages
}

// printSidechainMessages prints messages in simple format
func printSidechainMessages(messages []Message) {
	if len(messages) == 0 {
		return
	}

	for _, msg := range messages {
		// Process each content item in the message
		for _, item := range msg.Message.Content {
			switch item.Type {
			case "text":
				// Regular text content
				label := msg.Type
				if msg.UserType == "internal" {
					label = "internal"
				}
				fmt.Printf("==== %s\n%s\n\n", label, item.Text)

			case "tool_use":
				// Tool use (e.g., Bash command)
				fmt.Printf("==== tool_use (%s)\n", item.Name)
				if inputJSON, err := json.MarshalIndent(item.Input, "", "  "); err == nil {
					fmt.Printf("%s\n\n", string(inputJSON))
				}

			case "tool_result":
				// Tool result
				fmt.Printf("==== tool_result\n")
				if item.IsError {
					fmt.Printf("Error: %v\n\n", item.Content)
				} else {
					fmt.Printf("%v\n\n", item.Content)
				}
			}
		}
	}
}

// RunTestCases executes a list of test cases with the standard test pattern
// Each test case gets its own isolated environment with separate VCR proxy and temp directory
func RunTestCases(t *testing.T, tests []TestCase) {
	t.Helper()

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			// Get test function name from parent test
			testFuncName := t.Name()
			// Remove the subtest name to get parent test name
			// e.g., "Test01ShoppingRequest/shopping_request_with_eggs" -> "Test01ShoppingRequest"
			parts := strings.Split(testFuncName, "/")
			parentTestName := parts[0]

			// Setup test environment for this specific test case
			cassetteName := filepath.Join(parentTestName, tt.Name)
			tmpDir := setupTestEnvironmentForCase(t, parentTestName, tt.Name)
			proxy := startVCRProxyForTest(t, cassetteName)

			// Cleanup after test
			defer func() {
				cleanupTestEnvironment(t, tmpDir)
				if proxy != nil {
					stopVCRProxy(t, proxy)
				}
			}()

			// Execute using the test-specific directory and proxy
			output := runClaudeCommandWithProxy(t, tmpDir, proxy, tt.Command, tt.Args)

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
