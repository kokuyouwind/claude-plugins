#!/usr/bin/env python3
"""
VCR Cassette Manager

Manage VCR cassettes (recordings) for Claude API testing.

Usage:
    python3 vcr_manager.py list [cassette_name]           # List recordings
    python3 vcr_manager.py stats [cassette_name]          # Show statistics
    python3 vcr_manager.py show <cache_key> [cassette]    # Show recording details
    python3 vcr_manager.py delete <cache_key> [cassette]  # Delete recording
    python3 vcr_manager.py clear [cassette_name]          # Clear all recordings
    python3 vcr_manager.py cassettes                      # List all cassettes

Examples:
    # List recordings in default cassette
    python3 vcr_manager.py list

    # Show statistics for specific cassette
    python3 vcr_manager.py stats my_test

    # Show recording details (cache key can be partial)
    python3 vcr_manager.py show abc123

    # Delete a recording
    python3 vcr_manager.py delete abc123

    # Clear all recordings in cassette
    python3 vcr_manager.py clear default
"""

import sys
import json
from pathlib import Path
from datetime import datetime


class VCRManager:
    """Manage VCR cassettes"""

    def __init__(self, cassettes_dir=None):
        if cassettes_dir is None:
            # Default to cassettes directory in same location as this script
            script_dir = Path(__file__).parent
            cassettes_dir = script_dir / "cassettes"

        self.cassettes_dir = Path(cassettes_dir)

    def list_cassettes(self):
        """List all available cassettes"""
        if not self.cassettes_dir.exists():
            print("No cassettes directory found")
            return []

        cassettes = []
        for cassette_dir in self.cassettes_dir.iterdir():
            if cassette_dir.is_dir() and (cassette_dir / "index.json").exists():
                # Load index to get recording count
                try:
                    with open(cassette_dir / "index.json") as f:
                        data = json.load(f)
                        recordings_count = len(data.get('recordings', {}))
                except:
                    recordings_count = 0

                cassettes.append({
                    'name': cassette_dir.name,
                    'path': cassette_dir,
                    'recordings': recordings_count,
                })

        return cassettes

    def get_cassette(self, cassette_name='default'):
        """Get cassette manager for specific cassette"""
        cassette_dir = self.cassettes_dir / cassette_name
        if not cassette_dir.exists():
            return None

        index_file = cassette_dir / "index.json"
        if not index_file.exists():
            return None

        try:
            with open(index_file) as f:
                data = json.load(f)
                return {
                    'name': cassette_name,
                    'dir': cassette_dir,
                    'index': data.get('recordings', {}),
                    'index_file': index_file,
                }
        except Exception as e:
            print(f"Error loading cassette: {e}")
            return None

    def list_recordings(self, cassette_name='default', limit=None):
        """List recordings in cassette"""
        cassette = self.get_cassette(cassette_name)
        if not cassette:
            print(f"Cassette '{cassette_name}' not found")
            return

        index = cassette['index']
        if not index:
            print(f"No recordings in cassette '{cassette_name}'")
            return

        print(f"Cassette: {cassette_name}")
        print(f"Total: {len(index)} recordings")
        print("-" * 120)
        print(f"{'Cache Key':<18} {'Model':<35} {'Stream':<7} {'Size':>10} {'Recorded At':<20}")
        print("-" * 120)

        items = list(index.items())
        if limit:
            items = items[:limit]

        for cache_key, info in items:
            model = info.get('model', 'unknown')
            size = info.get('response_size', 0)
            stream = "Yes" if info.get('stream', False) else "No"
            timestamp = info.get('recorded_at', 'unknown')

            # Format timestamp
            try:
                dt = datetime.fromisoformat(timestamp)
                timestamp_str = dt.strftime("%Y-%m-%d %H:%M:%S")
            except:
                timestamp_str = timestamp[:20] if len(timestamp) > 20 else timestamp

            print(f"{cache_key[:16]:<18} {model:<35} {stream:<7} {size:>10,} {timestamp_str:<20}")

        if limit and len(index) > limit:
            print(f"\n... and {len(index) - limit} more")

    def show_stats(self, cassette_name='default'):
        """Show cassette statistics"""
        cassette = self.get_cassette(cassette_name)
        if not cassette:
            print(f"Cassette '{cassette_name}' not found")
            return

        index = cassette['index']

        print("=" * 80)
        print(f"Cassette: {cassette_name}")
        print("=" * 80)

        if not index:
            print("No recordings")
            return

        # Basic stats
        print(f"\nTotal recordings: {len(index)}")
        print(f"Cassette directory: {cassette['dir'].absolute()}")

        # By model
        models = {}
        total_size = 0
        stream_count = 0
        timestamps = []

        for info in index.values():
            model = info.get('model', 'unknown')
            models[model] = models.get(model, 0) + 1
            total_size += info.get('response_size', 0)
            if info.get('stream', False):
                stream_count += 1
            if 'recorded_at' in info:
                timestamps.append(info['recorded_at'])

        print(f"\nBy model:")
        for model, count in sorted(models.items(), key=lambda x: x[1], reverse=True):
            print(f"  {model}: {count}")

        print(f"\nStreaming responses: {stream_count}")
        print(f"Non-streaming responses: {len(index) - stream_count}")

        print(f"\nTotal size: {total_size:,} bytes ({total_size / 1024 / 1024:.2f} MB)")

        # Date range
        if timestamps:
            try:
                dts = [datetime.fromisoformat(ts) for ts in timestamps]
                print(f"\nDate range:")
                print(f"  Oldest: {min(dts).strftime('%Y-%m-%d %H:%M:%S')}")
                print(f"  Newest: {max(dts).strftime('%Y-%m-%d %H:%M:%S')}")
            except:
                pass

    def show_recording(self, cache_key_prefix, cassette_name='default'):
        """Show recording details"""
        cassette = self.get_cassette(cassette_name)
        if not cassette:
            print(f"Cassette '{cassette_name}' not found")
            return

        index = cassette['index']

        # Find matching keys
        matched_keys = [k for k in index.keys() if k.startswith(cache_key_prefix)]

        if not matched_keys:
            print(f"Recording not found: {cache_key_prefix}")
            return

        if len(matched_keys) > 1:
            print(f"Multiple matches found:")
            for key in matched_keys:
                print(f"  {key}")
            print(f"\nPlease specify more characters")
            return

        full_key = matched_keys[0]
        info = index[full_key]
        recordings_dir = cassette['dir'] / "recordings"

        # Load metadata
        meta_file = recordings_dir / f"{full_key}.meta.json"
        if meta_file.exists():
            with open(meta_file) as f:
                meta = json.load(f)
        else:
            meta = {}

        # Display info
        print("=" * 80)
        print(f"Recording: {full_key}")
        print("=" * 80)

        print(f"\nBasic Info:")
        print(f"  Model: {info.get('model', 'unknown')}")
        print(f"  Recorded at: {info.get('recorded_at', 'unknown')}")
        print(f"  Stream: {'Yes' if info.get('stream', False) else 'No'}")
        print(f"  Status code: {info.get('status_code', 'unknown')}")
        print(f"  Request size: {info.get('request_size', 0):,} bytes")
        print(f"  Response size: {info.get('response_size', 0):,} bytes")

        if 'duration' in meta:
            print(f"  Duration: {meta['duration']:.3f}s")

        print(f"\nRequest URL:")
        print(f"  {meta.get('url', 'unknown')}")

        # Load and show request
        request_file = recordings_dir / f"{full_key}.request.json"
        if request_file.exists():
            with open(request_file) as f:
                request = json.load(f)

            print(f"\nRequest Body (keys):")
            for key in request.keys():
                print(f"  - {key}")

            if 'messages' in request:
                messages = request['messages']
                print(f"\nMessages: {len(messages)}")
                for i, msg in enumerate(messages[:2]):
                    content = msg.get('content', '')
                    if isinstance(content, str):
                        preview = content[:100].replace('\n', ' ')
                    elif isinstance(content, list):
                        preview = f"[{len(content)} items]"
                    else:
                        preview = str(type(content))
                    print(f"  [{i}] {msg.get('role', 'unknown')}: {preview}")

        # Response info
        response_file = recordings_dir / f"{full_key}.response.bin"
        if response_file.exists():
            with open(response_file, 'rb') as f:
                content = f.read()

            print(f"\nResponse:")
            print(f"  Size: {len(content):,} bytes")

            # Try to decode and show preview
            try:
                text = content.decode('utf-8')
                if 'data:' in text:
                    lines = text.split('\n')
                    data_lines = [l for l in lines if l.startswith('data:')]
                    print(f"  SSE events: {len(data_lines)}")

                    # Show first event
                    if data_lines:
                        first_event = data_lines[0][5:].strip()
                        if first_event and first_event != '[DONE]':
                            try:
                                event_json = json.loads(first_event)
                                print(f"  First event type: {event_json.get('type', 'unknown')}")
                            except:
                                pass

                print(f"\nResponse preview (first 500 chars):")
                print("-" * 80)
                print(text[:500])
                if len(text) > 500:
                    print(f"\n... (truncated, total {len(text)} characters)")
            except:
                print(f"  Binary content")

        print("\n" + "=" * 80)
        print(f"Files:")
        print(f"  Request: {request_file}")
        print(f"  Response: {response_file}")
        print(f"  Metadata: {meta_file}")

    def delete_recording(self, cache_key_prefix, cassette_name='default'):
        """Delete a recording"""
        cassette = self.get_cassette(cassette_name)
        if not cassette:
            print(f"Cassette '{cassette_name}' not found")
            return

        index = cassette['index']

        # Find matching keys
        matched_keys = [k for k in index.keys() if k.startswith(cache_key_prefix)]

        if not matched_keys:
            print(f"Recording not found: {cache_key_prefix}")
            return

        if len(matched_keys) > 1:
            print(f"Multiple matches found:")
            for key in matched_keys:
                print(f"  {key}")
            print(f"\nPlease specify more characters")
            return

        full_key = matched_keys[0]
        recordings_dir = cassette['dir'] / "recordings"

        # Delete files
        for suffix in ['.request.json', '.response.bin', '.meta.json']:
            file_path = recordings_dir / f"{full_key}{suffix}"
            if file_path.exists():
                file_path.unlink()
                print(f"✓ Deleted: {file_path.name}")

        # Update index
        del index[full_key]
        with open(cassette['index_file'], 'w') as f:
            json.dump({
                'version': '1.0',
                'cassette': cassette_name,
                'recordings': index,
            }, f, indent=2, ensure_ascii=False)

        print(f"✓ Removed from index: {full_key}")

    def clear_cassette(self, cassette_name='default'):
        """Clear all recordings in cassette"""
        cassette = self.get_cassette(cassette_name)
        if not cassette:
            print(f"Cassette '{cassette_name}' not found")
            return

        count = len(cassette['index'])
        print(f"This will delete all {count} recordings in cassette '{cassette_name}'")
        confirm = input("Are you sure? (yes/no): ")

        if confirm.lower() != 'yes':
            print("Cancelled")
            return

        # Delete all files
        recordings_dir = cassette['dir'] / "recordings"
        if recordings_dir.exists():
            for file_path in recordings_dir.glob("*"):
                file_path.unlink()
            print(f"✓ Deleted all recording files")

        # Clear index
        with open(cassette['index_file'], 'w') as f:
            json.dump({
                'version': '1.0',
                'cassette': cassette_name,
                'recordings': {},
            }, f, indent=2, ensure_ascii=False)

        print(f"✓ Cleared cassette index")


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    manager = VCRManager()
    command = sys.argv[1]

    if command == "cassettes":
        cassettes = manager.list_cassettes()
        if not cassettes:
            print("No cassettes found")
        else:
            print(f"Available cassettes:")
            print("-" * 60)
            print(f"{'Name':<20} {'Recordings':>15} {'Path'}")
            print("-" * 60)
            for c in cassettes:
                print(f"{c['name']:<20} {c['recordings']:>15} {c['path']}")

    elif command == "list":
        cassette_name = sys.argv[2] if len(sys.argv) > 2 else 'default'
        limit = int(sys.argv[3]) if len(sys.argv) > 3 else None
        manager.list_recordings(cassette_name, limit)

    elif command == "stats":
        cassette_name = sys.argv[2] if len(sys.argv) > 2 else 'default'
        manager.show_stats(cassette_name)

    elif command == "show":
        if len(sys.argv) < 3:
            print("Usage: vcr_manager.py show <cache_key> [cassette_name]")
            sys.exit(1)
        cache_key = sys.argv[2]
        cassette_name = sys.argv[3] if len(sys.argv) > 3 else 'default'
        manager.show_recording(cache_key, cassette_name)

    elif command == "delete":
        if len(sys.argv) < 3:
            print("Usage: vcr_manager.py delete <cache_key> [cassette_name]")
            sys.exit(1)
        cache_key = sys.argv[2]
        cassette_name = sys.argv[3] if len(sys.argv) > 3 else 'default'
        manager.delete_recording(cache_key, cassette_name)

    elif command == "clear":
        cassette_name = sys.argv[2] if len(sys.argv) > 2 else 'default'
        manager.clear_cassette(cassette_name)

    else:
        print(f"Unknown command: {command}")
        print(__doc__)
        sys.exit(1)


if __name__ == "__main__":
    main()
