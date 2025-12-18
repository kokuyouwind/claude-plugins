# Claude API VCR Proxy

VCR (Video Cassette Recorder) style proxy for Claude API testing. Record API requests/responses and replay them for fast, deterministic testing without calling the actual API.

## Features

- **Record Mode**: Call API and save responses to "cassettes"
- **Replay Mode**: Return cached responses without calling API
- **Auto Mode**: Use cache if available, otherwise record (default)
- **Streaming Support**: Correctly handles SSE (Server-Sent Events) responses
- **Multiple Cassettes**: Organize recordings by test suite or purpose

## Installation

1. Install mitmproxy:
```bash
pip3 install mitmproxy
```

2. The VCR scripts are ready to use in this directory.

## Quick Start

### 1. Record API Responses

Start the proxy in record mode:

```bash
VCR_MODE=record mitmdump -s claude_vcr.py \
  --mode reverse:https://api.anthropic.com \
  --listen-port 8000
```

Run Claude Code through the proxy:

```bash
ANTHROPIC_BASE_URL="http://localhost:8000" \
  claude -p "Hello, Claude!"
```

The request/response will be saved to `cassettes/default/`.

### 2. Replay Recorded Responses

Start the proxy in replay mode:

```bash
VCR_MODE=replay mitmdump -s claude_vcr.py \
  --mode reverse:https://api.anthropic.com \
  --listen-port 8000
```

Run the same command again:

```bash
ANTHROPIC_BASE_URL="http://localhost:8000" \
  claude -p "Hello, Claude!"
```

This time, the response comes from cache instantly, without calling the API!

## VCR Modes

### Record Mode

Always calls the API and saves responses to cassette.

```bash
VCR_MODE=record mitmdump -s claude_vcr.py \
  --mode reverse:https://api.anthropic.com \
  --listen-port 8000
```

**Use when:**
- Creating new test fixtures
- Updating existing recordings
- Capturing new API behaviors

### Replay Mode

Always uses cached responses. Returns error on cache miss (default: strict).

```bash
VCR_MODE=replay mitmdump -s claude_vcr.py \
  --mode reverse:https://api.anthropic.com \
  --listen-port 8000
```

**Use when:**
- Running tests
- Working offline
- Ensuring no API calls are made

**With fallback** (call API on cache miss):

```bash
VCR_MODE=replay VCR_REPLAY_FALLBACK=fallback \
  mitmdump -s claude_vcr.py \
  --mode reverse:https://api.anthropic.com \
  --listen-port 8000
```

### Auto Mode (Default)

Uses cache if available, otherwise records.

```bash
# No VCR_MODE specified = auto mode
mitmdump -s claude_vcr.py \
  --mode reverse:https://api.anthropic.com \
  --listen-port 8000
```

**Use when:**
- Developing and testing simultaneously
- Want convenience over strict control

## Environment Variables

| Variable | Values | Default | Description |
|----------|--------|---------|-------------|
| `VCR_MODE` | `record`, `replay`, `auto` | `auto` | VCR operation mode |
| `VCR_REPLAY_FALLBACK` | `strict`, `fallback` | `strict` | Behavior on cache miss in replay mode |
| `VCR_CASSETTE` | string | `default` | Cassette name (for organizing recordings) |

## Using Multiple Cassettes

Organize recordings by test suite or purpose:

```bash
# Record to "basic_tests" cassette
VCR_MODE=record VCR_CASSETTE=basic_tests mitmdump -s claude_vcr.py \
  --mode reverse:https://api.anthropic.com --listen-port 8000

# Replay from "basic_tests" cassette
VCR_MODE=replay VCR_CASSETTE=basic_tests mitmdump -s claude_vcr.py \
  --mode reverse:https://api.anthropic.com --listen-port 8000
```

Each cassette is stored in `cassettes/<name>/`.

## Managing Cassettes

Use `vcr_manager.py` to inspect and manage recordings.

### List all cassettes

```bash
python3 vcr_manager.py cassettes
```

### List recordings in a cassette

```bash
# Default cassette
python3 vcr_manager.py list

# Specific cassette
python3 vcr_manager.py list my_test
```

### Show statistics

```bash
python3 vcr_manager.py stats
python3 vcr_manager.py stats my_test
```

Example output:
```
================================================================================
Cassette: default
================================================================================

Total recordings: 3
Cassette directory: /path/to/test/vcr/cassettes/default

By model:
  claude-sonnet-4-5-20250929: 2
  claude-haiku-4-5-20251001: 1

Streaming responses: 3
Non-streaming responses: 0

Total size: 15,000 bytes (0.01 MB)

Date range:
  Oldest: 2025-12-17 12:00:00
  Newest: 2025-12-17 12:30:00
```

### Show recording details

```bash
# Can use partial cache key
python3 vcr_manager.py show abc123
```

### Delete a recording

```bash
python3 vcr_manager.py delete abc123
```

### Clear entire cassette

```bash
python3 vcr_manager.py clear default
```

## How It Works

### Cache Key Generation

The VCR proxy generates a cache key from the request body:

```python
cache_fields = {
    'model': request_body.get('model'),
    'messages': request_body.get('messages'),
    'system': request_body.get('system'),
    'tools': request_body.get('tools'),
    'max_tokens': request_body.get('max_tokens'),
    'temperature': request_body.get('temperature'),
    'top_p': request_body.get('top_p'),
    'top_k': request_body.get('top_k'),
}
cache_key = sha256(json.dumps(cache_fields, sort_keys=True))
```

**Included fields**: Model, messages, system prompt, tools, generation parameters
**Excluded fields**: `metadata` (session-specific), `stream` (response format)

Same prompt → Same cache key → Same cached response

### Response Storage

Responses are stored as binary files to preserve exact byte-level content:

```
cassettes/
  default/                      # Cassette name
    ├── index.json              # Recording index
    └── recordings/
        ├── <hash>.request.json   # Request body (JSON)
        ├── <hash>.response.bin   # Response content (binary)
        └── <hash>.meta.json      # Metadata (headers, timing)
```

This ensures:
- Streaming (SSE) responses are replayed identically
- No parsing/reconstruction overhead
- Complete fidelity to original API response

## Use Cases

### Fast Test Execution

Record once, replay many times:

```bash
# Record (slow, calls API)
VCR_MODE=record mitmdump -s claude_vcr.py ... &
./run_tests.sh

# Replay (fast, no API calls)
VCR_MODE=replay mitmdump -s claude_vcr.py ... &
./run_tests.sh  # Runs 10x faster!
```

### Offline Development

Work without internet connection:

```bash
# Record while online
VCR_MODE=record mitmdump -s claude_vcr.py ... &
# ... capture all needed interactions ...

# Later, work offline
VCR_MODE=replay mitmdump -s claude_vcr.py ... &
# All cached interactions work!
```

### Deterministic Testing

Same input always produces same output:

```bash
VCR_MODE=replay mitmdump -s claude_vcr.py ... &
# Tests always get same API responses
# No flakiness from API variations
```

### Cost Reduction

Reduce API calls during development:

```bash
VCR_MODE=auto mitmdump -s claude_vcr.py ... &
# New requests recorded
# Repeated requests replayed from cache
# Save API costs!
```

## Troubleshooting

### Cache miss in replay mode

```
✗ [ERROR] abc123... → Cache miss in replay mode (strict)
```

**Solutions:**
1. Run in record mode first to capture the request
2. Use auto mode instead of replay mode
3. Enable fallback: `VCR_REPLAY_FALLBACK=fallback`

### Wrong response replayed

The cache key might not be unique enough. Check if:
- You're using the same model
- The prompt is exactly the same
- System prompt and tools are identical

Use `vcr_manager.py show <key>` to inspect cached requests.

### Cassette not found

```
Cassette 'my_test' not found
```

Make sure:
1. The cassette name matches: `VCR_CASSETTE=my_test`
2. You've recorded something to that cassette first
3. The cassette directory exists: `cassettes/my_test/`

## Advanced Usage

### Initialize cache from mitmproxy dumps

If you already have mitmproxy dump files, convert them to VCR cassettes:

```python
# Create a script to import dumps
from claude_vcr import CacheManager
from mitmproxy import io
import json

cache_manager = CacheManager("./cassettes", "imported")

with open("dump.mitm", "rb") as f:
    for flow in io.FlowReader(f).stream():
        if "/v1/messages" in flow.request.url:
            # Extract and save recording
            # ... implementation ...
```

### Custom cache key generation

Modify `generate_cache_key()` in `claude_vcr.py` to customize what makes requests "the same":

```python
def generate_cache_key(self, request_body):
    # Example: Ignore max_tokens for matching
    cache_fields = {
        'model': request_body.get('model'),
        'messages': request_body.get('messages'),
        # ... other fields ...
    }
    # ... hash generation ...
```

## Tips

1. **Use descriptive cassette names**: `VCR_CASSETTE=test_tool_use` is better than `VCR_CASSETTE=test1`

2. **Commit cassettes to git**: Share test fixtures with your team

3. **Separate cassettes by test suite**: Easier to manage and update

4. **Use replay mode in CI**: Ensure no API calls in continuous integration

5. **Periodically re-record**: API behavior may change, update your cassettes

## Architecture

```
Client (Claude CLI)
    ↓
mitmproxy (reverse proxy)
    ↓
claude_vcr.py (addon)
    ├─ Record mode → API → Save to cassette
    ├─ Replay mode → Load from cassette
    └─ Auto mode → Check cassette → API if needed
```

### Request Flow

1. Client sends request to proxy
2. VCR addon generates cache key from request
3. Based on mode:
   - **Record**: Forward to API, save response
   - **Replay**: Load from cache, return immediately
   - **Auto**: Check cache, forward to API if miss
4. Response returned to client

## Performance Analysis

### Cache Hit Performance (code-like-prompt tests)

Performance comparison for a single test case (`shopping_request_with_eggs`):

| Execution | Time | Speedup |
|-----------|------|---------|
| Without VCR (direct API) | 8.66s | - |
| With VCR (cache replay) | 6.43s | **25.7% faster** |
| Cache savings | 2.23s | - |

### API Request Breakdown

For each test execution, Claude Code makes the following API requests:

| Endpoint | Count | Cached? | Purpose | Estimated Time |
|----------|-------|---------|---------|----------------|
| `/v1/messages` | 4 | ✅ Yes | Main Claude API requests (warmup + actual) | ~2-3s (cached) |
| `/v1/messages/count_tokens` | 1 | ❌ No | Token counting for cost estimation | ~0.5-1s |
| `/api/event_logging/batch` | 2 | ❌ No | Telemetry/analytics logging | ~1-1.5s |

**Total uncached time**: ~2.0-2.5s per test case

### Why Cache Doesn't Eliminate All Delay

Even with cache hits, tests still take several seconds due to:

1. **Uncached Requests** (~2.2s)
   - `count_tokens` and `event_logging` endpoints are not cached
   - These still make actual API calls even in replay mode

2. **Claude Code Processing** (~4-5s)
   - Response parsing and deserialization
   - Streaming response handling
   - Output formatting and display
   - Test framework overhead

3. **Sequential Execution**
   - Warmup request → actual request flow
   - Each request processed one at a time

### Improvement Opportunities

To further reduce test execution time:

1. **Cache additional endpoints**: Extend VCR to cache `count_tokens` and `event_logging`
2. **Disable telemetry in tests**: Configure Claude Code to skip event logging
3. **Optimize Claude Code**: Reduce response processing overhead

### Current Limitations

- Cache key based on: model, messages, system, tools, and generation parameters
- Not cached: `count_tokens`, `event_logging`, and other non-message endpoints
- Fixed test directory required for cache hits (see test/code-like-prompt/helpers.go)

## License

This is part of the claude-plugins repository.
