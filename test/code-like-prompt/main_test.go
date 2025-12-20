package code_like_prompt

import (
	"os"
	"testing"
)

// TestMain runs before all tests and after all tests
func TestMain(m *testing.M) {
	// Run all tests
	exitCode := m.Run()

	// Cleanup: stop VCR proxy if it was started
	if globalProxy != nil {
		stopVCRProxy(&testing.T{}, globalProxy)
		globalProxy = nil
	}

	os.Exit(exitCode)
}
