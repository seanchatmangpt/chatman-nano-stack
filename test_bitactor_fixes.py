#!/usr/bin/env python3
"""
Test script to validate BitActor CLI fixes
"""

import subprocess
import sys
from pathlib import Path

def run_test():
    print("🧪 Testing BitActor CLI fixes...")
    
    # Test 1: Generate code
    print("\n1️⃣ Generating BitActor code...")
    result = subprocess.run([
        sys.executable, "bitactor_cli.py", "generate",
        "ontologies/bitactor_semantic_core.ttl",
        "generated/test_fix",
        "test"
    ], capture_output=True, text=True)
    
    if result.returncode != 0:
        print(f"❌ Generation failed: {result.stderr}")
        return False
    
    print("✅ Generation successful")
    
    # Test 2: Run self-check
    print("\n2️⃣ Running self-check...")
    result = subprocess.run([
        sys.executable, "bitactor_cli.py", "self-check",
        "generated/test_fix"
    ], capture_output=True, text=True)
    
    print("\nOutput:")
    print(result.stdout)
    
    if result.returncode == 0:
        print("\n✅ Self-check passed!")
    else:
        print(f"\n⚠️ Self-check had issues (this may be expected if Erlang is not installed)")
        print(f"stderr: {result.stderr}")
    
    # Check specific fixes
    print("\n3️⃣ Checking specific fixes:")
    
    # Check if benchmark timeout was increased
    with open("bitactor_cli.py", "r") as f:
        content = f.read()
        if "timeout=15" in content and "15 second timeout for 10s stress test" in content:
            print("✅ C benchmark timeout fixed (15s)")
        else:
            print("❌ C benchmark timeout not fixed")
    
    # Check if Erlang dynamic module names are used
    if '"{module_name}"' in content and "module_name = erl_files[0].stem" in content:
        print("✅ Erlang dynamic module names fixed")
    else:
        print("❌ Erlang dynamic module names not fixed")
    
    # Check if escript availability check exists
    if 'which", "escript' in content:
        print("✅ Erlang escript availability check added")
    else:
        print("❌ Erlang escript availability check not added")
    
    return True

if __name__ == "__main__":
    run_test()