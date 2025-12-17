#!/usr/bin/env python3
"""
Import mitmproxy dump files into VCR cassettes

Converts existing mitmproxy dump files into VCR cassette format for replay.

Usage:
    python3 import_dump.py <dump_file> [dump_file2 ...] [--cassette NAME]

Examples:
    # Import single dump to default cassette
    python3 import_dump.py /tmp/mitmproxy_test/claude_api.dump

    # Import multiple dumps
    python3 import_dump.py /tmp/mitmproxy_test/*.dump

    # Import to specific cassette
    python3 import_dump.py dump.mitm --cassette my_test
"""

import sys
import json
import hashlib
from pathlib import Path
from datetime import datetime
from mitmproxy import io, http
from mitmproxy.exceptions import FlowReadException


class DumpImporter:
    """Import mitmproxy dumps into VCR cassettes"""

    def __init__(self, cassettes_dir=None, cassette_name="default"):
        if cassettes_dir is None:
            # Default to cassettes directory in same location as this script
            script_dir = Path(__file__).parent
            cassettes_dir = script_dir / "cassettes"

        self.cassette_dir = Path(cassettes_dir) / cassette_name
        self.recordings_dir = self.cassette_dir / "recordings"
        self.index_file = self.cassette_dir / "index.json"

        # Create directories
        self.cassette_dir.mkdir(parents=True, exist_ok=True)
        self.recordings_dir.mkdir(exist_ok=True)

        # Load or initialize index
        self.index = self.load_index()

    def load_index(self):
        """Load existing index"""
        if self.index_file.exists():
            try:
                with open(self.index_file) as f:
                    data = json.load(f)
                    return data.get('recordings', {})
            except Exception as e:
                print(f"Warning: Failed to load index: {e}")
                return {}
        return {}

    def save_index(self):
        """Save index"""
        with open(self.index_file, 'w') as f:
            json.dump({
                'version': '1.0',
                'cassette': self.cassette_dir.name,
                'recordings': self.index,
            }, f, indent=2, ensure_ascii=False)

    def generate_cache_key(self, request_body):
        """Generate cache key (same logic as claude_vcr.py)"""
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
        cache_str = json.dumps(cache_fields, sort_keys=True, ensure_ascii=False)
        return hashlib.sha256(cache_str.encode()).hexdigest()

    def is_messages_endpoint(self, url):
        """Check if URL is messages endpoint"""
        return "/v1/messages" in url and "count_tokens" not in url

    def import_dump(self, dump_path):
        """Import a dump file"""
        dump_path = Path(dump_path)
        if not dump_path.exists():
            print(f"Error: File not found: {dump_path}")
            return 0

        print(f"\n{'=' * 80}")
        print(f"Importing: {dump_path}")
        print('=' * 80)

        added_count = 0
        skipped_count = 0

        try:
            with open(dump_path, "rb") as logfile:
                freader = io.FlowReader(logfile)

                for i, flow in enumerate(freader.stream()):
                    if not isinstance(flow, http.HTTPFlow):
                        continue

                    # Only process messages endpoint
                    if not self.is_messages_endpoint(flow.request.url):
                        continue

                    # Skip if no response
                    if not flow.response or not flow.response.content:
                        print(f"  Flow {i+1}: Skipped (no response)")
                        skipped_count += 1
                        continue

                    try:
                        # Parse request body
                        request_body = json.loads(flow.request.content)

                        # Generate cache key
                        cache_key = self.generate_cache_key(request_body)

                        # Skip if already cached
                        if cache_key in self.index:
                            print(f"  Flow {i+1}: Skipped (already cached) {cache_key[:16]}...")
                            skipped_count += 1
                            continue

                        # Save recording
                        if self._save_recording(cache_key, flow, request_body):
                            print(f"  Flow {i+1}: Added {cache_key[:16]}... "
                                  f"({len(flow.response.content):,} bytes, "
                                  f"model: {request_body.get('model', 'unknown')})")
                            added_count += 1
                        else:
                            print(f"  Flow {i+1}: Failed to save")
                            skipped_count += 1

                    except json.JSONDecodeError as e:
                        print(f"  Flow {i+1}: Skipped (invalid JSON): {e}")
                        skipped_count += 1
                    except Exception as e:
                        print(f"  Flow {i+1}: Error: {e}")
                        skipped_count += 1

        except FlowReadException as e:
            print(f"Error reading flow file: {e}")
            return added_count

        print(f"\nSummary: {added_count} added, {skipped_count} skipped")
        return added_count

    def _save_recording(self, cache_key, flow, request_body):
        """Save a recording to cassette"""
        try:
            # Save request
            request_file = self.recordings_dir / f"{cache_key}.request.json"
            with open(request_file, 'w') as f:
                json.dump(request_body, f, indent=2, ensure_ascii=False)

            # Save response (binary)
            response_file = self.recordings_dir / f"{cache_key}.response.bin"
            with open(response_file, 'wb') as f:
                f.write(flow.response.content)

            # Save metadata
            meta_file = self.recordings_dir / f"{cache_key}.meta.json"
            with open(meta_file, 'w') as f:
                meta = {
                    'cache_key': cache_key,
                    'url': flow.request.url,
                    'method': flow.request.method,
                    'request_headers': dict(flow.request.headers),
                    'response_headers': dict(flow.response.headers),
                    'status_code': flow.response.status_code,
                    'recorded_at': datetime.now().isoformat(),
                    'imported_from': 'dump',
                }
                if flow.response.timestamp_end and flow.request.timestamp_start:
                    meta['duration'] = flow.response.timestamp_end - flow.request.timestamp_start

                json.dump(meta, f, indent=2, ensure_ascii=False)

            # Update index
            self.index[cache_key] = {
                'cache_key': cache_key,
                'model': request_body.get('model'),
                'recorded_at': datetime.now().isoformat(),
                'request_size': len(json.dumps(request_body)),
                'response_size': len(flow.response.content),
                'stream': request_body.get('stream', False),
                'status_code': flow.response.status_code,
            }

            return True

        except Exception as e:
            print(f"Error saving recording: {e}")
            import traceback
            traceback.print_exc()
            return False

    def show_stats(self):
        """Show import statistics"""
        print("\n" + "=" * 80)
        print("Cassette Statistics")
        print("=" * 80)
        print(f"Cassette: {self.cassette_dir.name}")
        print(f"Total recordings: {len(self.index)}")
        print(f"Cassette directory: {self.cassette_dir.absolute()}")

        if self.index:
            # By model
            models = {}
            total_size = 0
            for info in self.index.values():
                model = info.get('model', 'unknown')
                models[model] = models.get(model, 0) + 1
                total_size += info.get('response_size', 0)

            print(f"\nBy model:")
            for model, count in sorted(models.items(), key=lambda x: x[1], reverse=True):
                print(f"  {model}: {count}")

            print(f"\nTotal size: {total_size:,} bytes ({total_size / 1024 / 1024:.2f} MB)")


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    # Parse arguments
    dump_files = []
    cassette_name = "default"

    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]
        if arg == "--cassette":
            if i + 1 >= len(sys.argv):
                print("Error: --cassette requires a name")
                sys.exit(1)
            cassette_name = sys.argv[i + 1]
            i += 2
        else:
            dump_files.append(arg)
            i += 1

    if not dump_files:
        print("Error: No dump files specified")
        print(__doc__)
        sys.exit(1)

    # Import dumps
    importer = DumpImporter(cassette_name=cassette_name)
    total_added = 0

    for dump_file in dump_files:
        added = importer.import_dump(dump_file)
        total_added += added

    # Save index
    if total_added > 0:
        importer.save_index()
        print(f"\n✓ Cassette index saved ({total_added} new recordings)")

    # Show statistics
    importer.show_stats()


if __name__ == "__main__":
    main()
