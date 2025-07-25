#!/usr/bin/env python3
"""
CNS Setup Script - Bootstrap the Chatman Nano Stack environment
Built for reliability. Designed to last.
"""

import subprocess
import sys
import shlex
from typing import Union, List


def run_command(cmd: Union[str, List[str]], description: str) -> bool:
    """Run a shell command and report success/failure
    
    Args:
        cmd: Command to run - can be a string (will be split) or list of args
        description: Human-readable description of the command
        
    Returns:
        bool: True if command succeeded, False otherwise
    """
    print(f"→ {description}")
    try:
        # Convert string commands to list for security (no shell injection)
        if isinstance(cmd, str):
            cmd_list = shlex.split(cmd)
        else:
            cmd_list = cmd
            
        # Run without shell=True to prevent injection attacks
        result = subprocess.run(cmd_list, shell=False, check=True, capture_output=True, text=True)
        print(f"✓ {description}")
        return True
    except subprocess.CalledProcessError as e:
        print(f"✗ {description} failed: {e.stderr}")
        return False
    except Exception as e:
        print(f"✗ {description} failed with error: {e}")
        return False

def main():
    """Initialize the CNS development environment"""
    print("CNS Setup - James I. Chatman & Sean A. Chatman")
    print("=" * 50)

    # Check if uv is installed
    if not run_command("uv --version", "Checking uv installation"):
        print("\n❌ uv is not installed. Please install it first:")
        print("   curl -LsSf https://astral.sh/uv/install.sh | sh")
        return 1

    # Create virtual environment
    if not run_command("uv venv", "Creating Python virtual environment"):
        return 1

    # Sync dependencies
    if not run_command("uv sync", "Installing base dependencies"):
        return 1

    # Install development dependencies
    if not run_command("uv sync --extra dev --extra benchmark --extra trading",
                      "Installing development dependencies"):
        return 1

    # Verify installation
    if not run_command("uv run python -c 'import rdflib, jinja2; print(\"Core deps OK\")'",
                      "Verifying core dependencies"):
        return 1

    print("\n" + "=" * 50)
    print("✅ CNS environment ready!")
    print("\nNext steps:")
    print("  make help           - See all available commands")
    print("  make owl-compile    - Test the OWL compiler")
    print("  make python-test    - Run the test suite")
    print("  make full-benchmark - Run all benchmarks")

    return 0

if __name__ == "__main__":
    sys.exit(main())
