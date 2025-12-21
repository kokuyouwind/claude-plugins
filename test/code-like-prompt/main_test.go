package code_like_prompt

import (
	"os"
	"testing"
)

// TestMain runs before all tests and after all tests
func TestMain(m *testing.M) {
	// Setup: start VCR proxy and setup test environment once for all tests
	globalProxy = startVCRProxy(&testing.T{})
	globalTmpDir = setupTestEnvironmentOnce(&testing.T{})

	// Run all tests
	exitCode := m.Run()

	// Cleanup: remove test directory and stop VCR proxy
	cleanupTestEnvironment(&testing.T{}, globalTmpDir)
	if globalProxy != nil {
		stopVCRProxy(&testing.T{}, globalProxy)
		globalProxy = nil
	}

	os.Exit(exitCode)
}
