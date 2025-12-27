"""
Claude API VCR-Style Proxy for mitmproxy

VCR (Video Cassette Recorder) style proxy that can record and replay
API requests/responses for testing and development.

Usage:
    # Record mode - Call API and save responses
    VCR_MODE=record mitmdump -s claude_vcr.py \\
      --mode reverse:https://api.anthropic.com --listen-port 8000

    # Replay mode - Return cached responses (don't call API)
    VCR_MODE=replay mitmdump -s claude_vcr.py \\
      --mode reverse:https://api.anthropic.com --listen-port 8000

    # Auto mode - Use cache if available, otherwise record (default)
    mitmdump -s claude_vcr.py \\
      --mode reverse:https://api.anthropic.com --listen-port 8000

Then use with Claude Code:
    ANTHROPIC_BASE_URL="http://localhost:8000" claude -p "your prompt"

Environment Variables:
    VCR_MODE: record|replay|auto (default: auto)
    VCR_REPLAY_FALLBACK: fallback|strict (default: strict)
        - fallback: Fall back to API call on cache miss in replay mode
        - strict: Return error on cache miss in replay mode
    VCR_CASSETTE: Cassette name (default: "default")
"""

from mitmproxy import http
import json
import hashlib
from pathlib import Path
from datetime import datetime
import os


class VCRMode:
    """VCR operation modes"""
    RECORD = "record"   # Always call API and save to cache
    REPLAY = "replay"   # Always use cache (error on miss)
    AUTO = "auto"       # Use cache if available, otherwise record


class CacheManager:
    """Manages VCR cassette storage"""

    def __init__(self, cassettes_dir="./cassettes", cassette_name="default"):
        self.cassette_dir = Path(cassettes_dir) / cassette_name
        self.recordings_dir = self.cassette_dir / "recordings"
        self.index_file = self.cassette_dir / "index.json"

        # Create directories
        self.cassette_dir.mkdir(parents=True, exist_ok=True)
        self.recordings_dir.mkdir(exist_ok=True)

        # Load index
        self.index = self.load_index()

    def load_index(self):
        """Load cache index"""
        if self.index_file.exists():
            try:
                with open(self.index_file) as f:
                    data = json.load(f)
                    return data.get('recordings', {})
            except Exception as e:
                print(f"[WARN] Failed to load index: {e}")
                return {}
        return {}

    def save_index(self):
        """Save cache index"""
        try:
            with open(self.index_file, 'w') as f:
                json.dump({
                    'version': '1.0',
                    'cassette': os.path.basename(self.cassette_dir),
                    'recordings': self.index,
                }, f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"[ERROR] Failed to save index: {e}")

    def has_cache(self, cache_key):
        """Check if cache exists"""
        return cache_key in self.index

    def save_recording(self, cache_key, request_data, response_data, meta):
        """Save recording to cassette"""
        try:
            import base64

            # Create single recording file with all data
            recording = {
                'cache_key': cache_key,
                'meta': {
                    **meta,
                    'recorded_at': datetime.now().isoformat(),
                },
                'request': request_data,
                'response': {
                    'status_code': response_data['status_code'],
                    'headers': response_data['headers'],
                    'content': base64.b64encode(response_data['content']).decode('ascii'),
                },
            }

            # Save to single JSON file
            recording_file = self.recordings_dir / f"{cache_key}.json"
            with open(recording_file, 'w') as f:
                json.dump(recording, f, indent=2, ensure_ascii=False)

            # Update index
            self.index[cache_key] = {
                'cache_key': cache_key,
                'model': request_data.get('model'),
                'recorded_at': recording['meta']['recorded_at'],
                'request_size': len(json.dumps(request_data)),
                'response_size': len(response_data['content']),
                'stream': request_data.get('stream', False),
                'status_code': response_data['status_code'],
            }
            self.save_index()

            return True

        except Exception as e:
            print(f"[ERROR] Failed to save recording: {e}")
            import traceback
            traceback.print_exc()
            return False

    def load_recording(self, cache_key):
        """Load recording from cassette"""
        if cache_key not in self.index:
            return None

        try:
            import base64

            # Load recording from single JSON file
            recording_file = self.recordings_dir / f"{cache_key}.json"
            with open(recording_file) as f:
                recording = json.load(f)

            # Decode response content from base64
            content = base64.b64decode(recording['response']['content'])

            return {
                'status_code': recording['response']['status_code'],
                'headers': recording['response']['headers'],
                'content': content,
            }

        except Exception as e:
            print(f"[ERROR] Failed to load recording: {e}")
            return None


class ClaudeVCRProxy:
    """VCR-style proxy for Claude API"""

    def __init__(self):
        # Get configuration from environment
        self.mode = self._get_vcr_mode()
        self.replay_fallback = os.environ.get('VCR_REPLAY_FALLBACK', 'strict').lower() == 'fallback'
        self.cassette_name = os.environ.get('VCR_CASSETTE', 'default')

        # Initialize cache manager
        # Get script directory to find cassettes
        script_dir = Path(__file__).parent
        cassettes_dir = script_dir / "cassettes"
        self.cache_manager = CacheManager(cassettes_dir, self.cassette_name)

        # Statistics
        self.stats = {
            'replay_hits': 0,
            'replay_misses': 0,
            'recordings': 0,
            'errors': 0,
        }

        # Print configuration
        print("\n" + "=" * 80)
        print("Claude API VCR Proxy")
        print("=" * 80)
        print(f"Mode: {self.mode.upper()}")
        print(f"Cassette: {self.cassette_name}")
        print(f"Cassette directory: {self.cache_manager.cassette_dir.absolute()}")
        print(f"Replay fallback: {'enabled' if self.replay_fallback else 'disabled'}")
        print(f"Existing recordings: {len(self.cache_manager.index)}")
        print("=" * 80 + "\n")

    def _get_vcr_mode(self):
        """Get VCR mode from environment"""
        mode = os.environ.get('VCR_MODE', 'auto').lower()
        if mode not in ['record', 'replay', 'auto']:
            print(f"[WARN] Unknown VCR_MODE '{mode}', using 'auto'")
            return VCRMode.AUTO
        return mode

    def is_messages_endpoint(self, url):
        """Check if URL is messages endpoint"""
        return "/v1/messages" in url and "count_tokens" not in url

    def generate_cache_key(self, request_body):
        """Generate cache key from request body"""
        # Fields that should match for cache hit
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

        # Normalize and hash
        cache_str = json.dumps(cache_fields, sort_keys=True, ensure_ascii=False)
        return hashlib.sha256(cache_str.encode()).hexdigest()

    def request(self, flow: http.HTTPFlow):
        """Request hook - check cache and potentially replay"""

        # Only process messages endpoint
        if not self.is_messages_endpoint(flow.request.url):
            return

        try:
            # Parse request body
            request_body = json.loads(flow.request.content)

            # Generate cache key
            cache_key = self.generate_cache_key(request_body)

            # Store in flow metadata for response hook
            flow.metadata['vcr_cache_key'] = cache_key
            flow.metadata['vcr_request_body'] = request_body
            flow.metadata['vcr_request_timestamp'] = flow.request.timestamp_start

            # Mode-specific processing
            if self.mode == VCRMode.RECORD:
                # Record mode: always call API
                print(f"[RECORD] {cache_key[:16]}... → calling API")
                return

            elif self.mode == VCRMode.REPLAY:
                # Replay mode: use cache or error
                if recording := self.cache_manager.load_recording(cache_key):
                    self._replay_response(flow, recording, cache_key)
                    return

                # Cache miss
                if self.replay_fallback:
                    print(f"[REPLAY→RECORD] {cache_key[:16]}... → cache miss, calling API")
                    flow.metadata['vcr_fallback_to_record'] = True
                else:
                    self._error_response(flow, cache_key, "Cache miss in replay mode (strict)")

            elif self.mode == VCRMode.AUTO:
                # Auto mode: use cache if available, otherwise record
                if recording := self.cache_manager.load_recording(cache_key):
                    self._replay_response(flow, recording, cache_key)
                else:
                    print(f"[AUTO→RECORD] {cache_key[:16]}... → cache miss, calling API")

        except Exception as e:
            print(f"[ERROR] Request processing failed: {e}")
            import traceback
            traceback.print_exc()
            self.stats['errors'] += 1

    def _replay_response(self, flow, recording, cache_key):
        """Replay response from cache"""
        try:
            # Create response from cache
            flow.response = http.Response.make(
                recording['status_code'],
                recording['content'],
                {
                    **recording['headers'],
                    'X-VCR-Status': 'REPLAY',
                    'X-VCR-Cache-Key': cache_key[:16],
                }
            )

            # Update statistics
            self.stats['replay_hits'] += 1

            # Log
            info = self.cache_manager.index[cache_key]
            print(f"✓ [REPLAY] {cache_key[:16]}... "
                  f"(recorded: {info['recorded_at'][:19]}, "
                  f"size: {info['response_size']:,} bytes)")

        except Exception as e:
            print(f"[ERROR] Replay failed: {e}")
            self.stats['errors'] += 1

    def _error_response(self, flow, cache_key, message):
        """Return error response"""
        error_body = {
            'error': {
                'type': 'vcr_cache_miss',
                'message': message,
                'cache_key': cache_key,
                'hint': 'Run in record or auto mode to capture this request'
            }
        }

        flow.response = http.Response.make(
            404,
            json.dumps(error_body, indent=2),
            {
                'Content-Type': 'application/json',
                'X-VCR-Status': 'ERROR',
            }
        )

        self.stats['replay_misses'] += 1
        print(f"✗ [ERROR] {cache_key[:16]}... → {message}")

    def response(self, flow: http.HTTPFlow):
        """Response hook - save recording if in record mode"""

        # Get cache key from metadata
        cache_key = flow.metadata.get('vcr_cache_key')
        if not cache_key:
            return

        # Skip if already replayed
        if flow.response and 'X-VCR-Status' in flow.response.headers:
            return

        # Only record if in record mode, auto mode, or fallback
        should_record = (
            self.mode == VCRMode.RECORD or
            self.mode == VCRMode.AUTO or
            flow.metadata.get('vcr_fallback_to_record')
        )

        if not should_record:
            return

        # Record response
        if flow.response and flow.response.content:
            try:
                request_body = flow.metadata['vcr_request_body']

                # Prepare response data
                response_data = {
                    'status_code': flow.response.status_code,
                    'headers': dict(flow.response.headers),
                    'content': flow.response.content,
                }

                # Prepare metadata
                meta = {
                    'cache_key': cache_key,
                    'url': flow.request.url,
                    'method': flow.request.method,
                    'request_headers': dict(flow.request.headers),
                    'recorded_at': datetime.now().isoformat(),
                    'duration': flow.response.timestamp_end - flow.request.timestamp_start if flow.response.timestamp_end else 0,
                }

                # Save recording
                if self.cache_manager.save_recording(cache_key, request_body, response_data, meta):
                    # Add debug headers
                    flow.response.headers['X-VCR-Status'] = 'RECORDED'
                    flow.response.headers['X-VCR-Cache-Key'] = cache_key[:16]

                    self.stats['recordings'] += 1

                    print(f"💾 [RECORDED] {cache_key[:16]}... "
                          f"({len(flow.response.content):,} bytes)")

            except Exception as e:
                print(f"[ERROR] Recording failed: {e}")
                import traceback
                traceback.print_exc()
                self.stats['errors'] += 1

    def done(self):
        """Cleanup hook - print statistics"""
        print("\n" + "=" * 80)
        print("VCR Statistics")
        print("=" * 80)
        print(f"Replay hits: {self.stats['replay_hits']}")
        print(f"Replay misses: {self.stats['replay_misses']}")
        print(f"Recordings: {self.stats['recordings']}")
        print(f"Errors: {self.stats['errors']}")
        print(f"Total recordings in cassette: {len(self.cache_manager.index)}")
        print("=" * 80 + "\n")


# Register addon with mitmproxy
addons = [ClaudeVCRProxy()]
