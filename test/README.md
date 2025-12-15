# Test Directory

This directory contains Go-based tests for Claude Code plugins.

## Prerequisites

- Go 1.23 or later
- Claude Code CLI installed and authenticated
- mise (recommended for Go version management)

## Directory Structure

```
test/
├── go.mod                          # Go module configuration with testify
├── go.sum                          # Dependency checksums
├── code-like-prompt/               # Tests for code-like-prompt plugin
│   ├── testdata/
│   │   └── settings.json.template  # Marketplace configuration template
│   ├── helpers.go                  # Test helper functions
│   └── commands_test.go            # Test cases
└── README.md                       # This file
```

## Setup

### Install Go (using mise)

```bash
mise install go@latest
```

### Download Dependencies

```bash
cd test
go mod download
```

## Running Tests

### Run All Tests

```bash
cd test
go test -v ./...
```

### Run Specific Plugin Tests

```bash
cd test
go test -v ./code-like-prompt
```

### Run Specific Test Cases

```bash
cd test
# Run all 01-shopping tests
go test -v ./code-like-prompt -run Test01Shopping

# Run specific test case
go test -v ./code-like-prompt -run Test01Shopping/shopping_request_with_eggs
```

### With Custom Go Version (using mise)

```bash
cd test
mise exec go@latest -- go test -v ./code-like-prompt
```

## How Tests Work

### Test Environment Isolation

Each test runs in an isolated environment to avoid interference from user configurations:

1. **Temporary Directory**: Created under `/tmp` with unique timestamp-based names
2. **Local Marketplace**: `.claude/settings.json` configured to use local plugin code instead of GitHub
3. **Clean Environment**: No CLAUDE.md interference from user/project directories
4. **Authentication**: Uses existing user authentication (not isolated)

### Test Execution Flow

```
Setup
  ↓
Create tmpDir (/tmp/code-like-prompt-test-XXXXXX)
  ↓
Copy .claude/settings.json with local marketplace config
  ↓
Execute: claude -p "command args" in tmpDir
  ↓
Verify output contains expected strings
  ↓
Cleanup tmpDir
```

### Settings Template

`testdata/settings.json.template` is processed to replace `{{REPO_PATH}}` with the absolute path to the repository:

```json
{
  "extraKnownMarketplaces": {
    "kokuyouwind-plugins": {
      "source": {
        "source": "directory",
        "path": "/absolute/path/to/repo"
      }
    }
  }
}
```

This ensures tests always use the current local code, even if the marketplace is configured as GitHub source.

## Adding New Tests

### 1. Create Test File

```bash
touch test/<plugin-name>/<category>_test.go
```

### 2. Define Test Cases (Table Driven Test)

```go
type TestCase struct {
    Name            string
    Command         string
    Args            map[string]interface{}
    ExpectedOutputs []string
}

func TestMyFeature(t *testing.T) {
    tests := []TestCase{
        {
            Name:    "test_case_name",
            Command: "/plugin:command",
            Args: map[string]interface{}{
                "param": "value",
            },
            ExpectedOutputs: []string{
                "expected output 1",
                "expected output 2",
            },
        },
    }

    for _, tt := range tests {
        t.Run(tt.Name, func(t *testing.T) {
            tmpDir := setupTestEnvironment(t)
            defer cleanupTestEnvironment(t, tmpDir)

            output := runClaudeCommand(t, tmpDir, tt.Command, tt.Args)

            for _, expected := range tt.ExpectedOutputs {
                assert.Contains(t, output, expected)
            }
        })
    }
}
```

### 3. Run Your Tests

```bash
cd test
go test -v ./<plugin-name> -run TestMyFeature
```

## Troubleshooting

### Tests Fail with Authentication Error

Ensure Claude Code is authenticated:
```bash
claude /login
```

### Tests Fail with "Plugin not found"

Verify the repository path in the generated settings.json:
```bash
# The path should be absolute, not relative
cat /tmp/code-like-prompt-test-*/\.claude/settings.json
```

### Tests Are Slow

This is expected - each test spawns a new Claude Code process which takes several seconds. Consider:
- Running specific tests instead of the full suite
- Using `-run` flag to filter tests
- Running tests in parallel if they are independent

## CI/CD Integration

Tests can be run in CI/CD pipelines with appropriate authentication setup:

```yaml
- name: Run tests
  env:
    ANTHROPIC_API_KEY: ${{ secrets.ANTHROPIC_API_KEY }}
  run: |
    cd test
    go test -v ./...
```

Note: `ANTHROPIC_API_KEY` is required in headless environments where `/login` is not available.
