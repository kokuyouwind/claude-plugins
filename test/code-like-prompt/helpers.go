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
	cmd := exec.Command("claude", "--model", "claude-haiku-4-5-20251001", "-p", cmdStr)
	cmd.Dir = tmpDir
	cmd.Env = os.Environ() // Use parent environment including authentication

	// If VCR proxy is running, set ANTHROPIC_BASE_URL
	if globalProxy != nil {
		cmd.Env = append(cmd.Env, fmt.Sprintf("ANTHROPIC_BASE_URL=http://localhost:%s", globalProxy.port))
	}

	output, err := cmd.CombinedOutput()
	require.NoError(t, err, "Failed to execute claude command: %s\nOutput: %s", cmdStr, string(output))

	// If DEBUG=1, print sidechain messages
	if os.Getenv("DEBUG") == "1" {
		// Wait a bit for session file to be written
		time.Sleep(500 * time.Millisecond)

		// Extract session ID from the most recent session file
		sessionID := extractSessionID(t, tmpDir, command)

		// Load and print sidechain messages
		messages := loadSidechainMessages(t, tmpDir, sessionID)
		printSidechainMessages(messages)
	}

	return string(output)
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

// extractSessionID extracts session ID from the most recent session file for the project
// that contains the specified command
func extractSessionID(t *testing.T, tmpDir string, command string) string {
	t.Helper()

	// Get project directory name
	projectDirName := getProjectDirName(tmpDir)
	if projectDirName == "" {
		return ""
	}

	// Project directory path
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return ""
	}

	projectDir := filepath.Join(homeDir, ".claude", "projects", projectDirName)

	// Check if project directory exists
	if _, err := os.Stat(projectDir); os.IsNotExist(err) {
		return ""
	}

	// Find all .jsonl files
	files, err := filepath.Glob(filepath.Join(projectDir, "*.jsonl"))
	if err != nil || len(files) == 0 {
		return ""
	}

	// Search for the most recent file that contains the command
	var mostRecent string
	var mostRecentTime time.Time

	for _, file := range files {
		info, err := os.Stat(file)
		if err != nil {
			continue
		}

		// Read the file to check if it contains the command
		content, err := os.ReadFile(file)
		if err != nil {
			continue
		}

		// Check if the file contains the command
		if strings.Contains(string(content), command) {
			if info.ModTime().After(mostRecentTime) {
				mostRecentTime = info.ModTime()
				mostRecent = file
			}
		}
	}

	if mostRecent == "" {
		return ""
	}

	// Extract session ID from filename (remove .jsonl extension)
	sessionID := filepath.Base(mostRecent)
	sessionID = strings.TrimSuffix(sessionID, ".jsonl")

	return sessionID
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
