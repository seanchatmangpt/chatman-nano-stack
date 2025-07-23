#!/usr/bin/env python3
"""
Run the generated AOT validation benchmark
"""

import glob
import subprocess
import sys
from pathlib import Path


def find_latest_binary():
    """Find the most recently generated binary"""
    # Look for binaries in the cache directory
    cache_dir = Path.home() / ".cache" / "tmp"
    pattern = str(cache_dir / "aot_compile_*" / "output" / "*" / "aot_validator_*")

    binaries = glob.glob(pattern)
    if not binaries:
        print("No compiled binaries found!")
        return None

    # Get the most recent one
    latest_binary = max(binaries, key=lambda p: Path(p).stat().st_mtime)
    return Path(latest_binary)

def run_benchmark():
    """Run the benchmark and capture output"""
    binary_path = find_latest_binary()

    if not binary_path:
        print("Could not find a compiled binary to run.")
        print("Please run test_aot_compilation.py first.")
        return False

    print(f"Found binary: {binary_path}")
    print(f"Size: {binary_path.stat().st_size} bytes")
    print()

    # Run the benchmark
    try:
        result = subprocess.run(
            [str(binary_path)],
            capture_output=True,
            text=True,
            timeout=30
        )

        print("=== BENCHMARK OUTPUT ===")
        print(result.stdout)

        if result.stderr:
            print("=== ERRORS ===")
            print(result.stderr)

        print(f"\nExit code: {result.returncode}")

        # Look for Mermaid diagrams in the output
        if "```mermaid" in result.stdout:
            print("\n✓ OpenTelemetry Mermaid diagrams generated successfully!")
        else:
            print("\n⚠️  No Mermaid diagrams found in output")

        return result.returncode == 0

    except subprocess.TimeoutExpired:
        print("Benchmark timed out after 30 seconds")
        return False
    except Exception as e:
        print(f"Error running benchmark: {e}")
        return False

def main():
    """Main entry point"""
    success = run_benchmark()
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
