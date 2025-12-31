package code_like_prompt

import (
	"os"
	"testing"
)

// TestMain runs before all tests and after all tests
// Note: Global proxy and tmpDir are no longer used.
// Each test case now creates its own isolated environment.
func TestMain(m *testing.M) {
	// Run all tests
	exitCode := m.Run()

	// Cleanup all VCR proxy processes
	cleanupAllProxies()

	os.Exit(exitCode)
}
