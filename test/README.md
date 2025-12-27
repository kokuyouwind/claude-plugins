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

### Install Go

```bash
mise install
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

### Debug Mode with Sidechain Messages

Enable debug mode to see detailed execution logs including user inputs, assistant responses, and tool calls:

```bash
cd test/code-like-prompt

# Run tests with debug output
DEBUG=1 go test -v -run Test01Shopping
```

**Debug Output Example**:
```
session-id: 82988a49-cf80-4c6b-aca7-0325a8587835
==== user
Execute the following code with environment: {"Egg.stock":3,"Milk.stock":5}
...

==== assistant
I'll execute this Ruby code with the provided environment variables.

==== tool_use (Skill)
{
  "args": "{\"Egg.stock\":3,\"Milk.stock\":5}",
  "skill": "code-like-prompt:01-shopping-request"
}

==== tool_result
Error: Execute skill: code-like-prompt:01-shopping-request

==== assistant
Bought 1 milk.
Bought 6 eggs.
```

**What Debug Mode Shows**:
- **session-id**: Unique identifier for the Claude Code session
- **user**: Input prompts sent to Claude
- **assistant**: Claude's responses and reasoning
- **tool_use**: Tools Claude attempted to use (Bash, Write, etc.) with full parameters
- **tool_result**: Results or errors from tool execution
- **internal**: Internal thoughts (userType: "internal") if present

**Finding Session Logs**:

Session logs are stored in `~/.claude/projects/` with directory names based on the test working directory:

```bash
# For test directory /tmp/code-like-prompt-test, the project directory is:
ls ~/.claude/projects/-private-tmp-code-like-prompt-test/

# View a specific session log (use session-id from debug output):
cat ~/.claude/projects/-private-tmp-code-like-prompt-test/82988a49-cf80-4c6b-aca7-0325a8587835.jsonl | jq '.'

# Find recent session files:
ls -lt ~/.claude/projects/-private-tmp-code-like-prompt-test/*.jsonl | head -5
```

**Session Log Format**:
- Each line is a JSON object representing a message or event
- Messages include `type` (user/assistant), `message.content`, `timestamp`, etc.
- Tool calls are stored as content items with `type: "tool_use"` or `type: "tool_result"`

### Using VCR for Faster Tests

VCR (Video Cassette Recorder) mode is **enabled by default** and caches Claude API responses for faster test execution:

```bash
cd test/code-like-prompt

# Default: VCR enabled (auto mode)
go test -v -run Test01Shopping

# Disable VCR to always call actual API
DISABLE_BOOT_VCR=1 go test -v -run Test01Shopping

# Combine with debug mode
DEBUG=1 go test -v -run Test01Shopping
```

**Performance Impact**:
- Without VCR: ~8.7s per test case
- With VCR (cached): ~6.4s per test case
- Speedup: ~25.7% (saves ~2.2s per test)

**VCR Modes**:
```bash
# Auto mode (default) - use cache if available, otherwise record
go test -v

# Record mode - always call API and update cache
BOOT_VCR_MODE=record go test -v

# Replay mode - only use cache (fail on cache miss)
BOOT_VCR_MODE=replay go test -v
```

**Important Notes**:
- VCR proxy runs automatically on port 8001 by default
- To disable VCR, set `DISABLE_BOOT_VCR=1`
- Cache is stored in `test/vcr/cassettes/default/`
- Proxy logs are written to `/tmp/vcr_proxy.log`
- Tests use fixed directory `/tmp/code-like-prompt-test` for cache consistency
- See `test/vcr/README.md` for detailed VCR documentation and performance analysis

## How Tests Work

### Test Environment Isolation

Each test runs in an isolated environment to avoid interference from user configurations:

1. **Temporary Directory**: Fixed path `/tmp/code-like-prompt-test` for VCR cache consistency
2. **Local Marketplace**: `.claude/settings.json` configured to use local plugin code instead of GitHub
3. **Clean Environment**: No CLAUDE.md interference from user/project directories
4. **Authentication**: Uses existing user authentication (not isolated)
5. **VCR Proxy** (optional): Automatically started on port 8001 by default

### Test Execution Flow

```
Setup
  ↓
Create tmpDir (/tmp/code-like-prompt-test)
  ↓
Copy .claude/settings.json with local marketplace config
  ↓
Execute: claude --output-format json -p "command args" in tmpDir
  ↓
Parse JSON response and extract result + session_id
  ↓
If DEBUG=1: Print session_id and sidechain messages
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
cat /tmp/code-like-prompt-test/.claude/settings.json
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
