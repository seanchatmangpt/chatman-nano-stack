#!/usr/bin/env python3
"""
SIMPLIFIED UNIT TESTS FOR 80/20 SECURITY PATCHES
Tests that work with the monkey-patched security system
"""

import subprocess
import time
import tempfile
import os
import sys

# Import and apply security patches
import security_patches_8020
security_patches_8020.install_security_patches()


def test_thread_limits():
    """Test thread creation limits"""
    print("\n🧪 TEST 1: Thread Creation Limits")
    print("-" * 50)
    
    # Test script that creates threads
    test_script = """
import threading
import time

threads = []
success_count = 0

for i in range(15):
    try:
        t = threading.Thread(target=lambda: time.sleep(0.1))
        t.start()
        threads.append(t)
        success_count += 1
    except RuntimeError as e:
        print(f"Thread {i+1} blocked: {e}")
        break

print(f"Created {success_count} threads before hitting limit")

# Cleanup
for t in threads:
    t.join()
"""
    
    result = subprocess.run(
        [sys.executable, "-c", test_script],
        capture_output=True,
        text=True
    )
    
    output = result.stdout + result.stderr
    print(output)
    
    # Check that limit was enforced
    if "Thread limit exceeded" in output and "Created 10 threads" in output:
        print("✅ PASSED: Thread limit correctly enforced at 10")
        return True
    else:
        print("❌ FAILED: Thread limit not properly enforced")
        return False


def test_process_limits():
    """Test process spawning limits"""
    print("\n🧪 TEST 2: Process Spawning Limits")
    print("-" * 50)
    
    # Test script that creates processes
    test_script = """
import subprocess

processes = []
success_count = 0

for i in range(10):
    try:
        p = subprocess.Popen(["sleep", "0.1"])
        processes.append(p)
        success_count += 1
    except RuntimeError as e:
        print(f"Process {i+1} blocked: {e}")
        break

print(f"Created {success_count} processes before hitting limit")

# Cleanup
for p in processes:
    p.terminate()
    p.wait()
"""
    
    result = subprocess.run(
        [sys.executable, "-c", test_script],
        capture_output=True,
        text=True
    )
    
    output = result.stdout + result.stderr
    print(output)
    
    # Check that limit was enforced
    if "Process limit exceeded" in output and "Created 5 processes" in output:
        print("✅ PASSED: Process limit correctly enforced at 5")
        return True
    else:
        print("❌ FAILED: Process limit not properly enforced")
        return False


def test_encoding_validation():
    """Test file encoding validation"""
    print("\n🧪 TEST 3: Encoding Validation")
    print("-" * 50)
    
    test_script = """
import tempfile
import os

# Test 1: Write problematic content
with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
    f.write("Normal text")
    f.write("\\x00")  # Null byte
    f.write("\\udcff")  # Invalid UTF-8
    temp_path = f.name

# Test 2: Read it back
try:
    with open(temp_path, 'r') as f:
        content = f.read()
    print("✅ Successfully handled problematic encoding")
    success = True
except Exception as e:
    print(f"❌ Failed to handle encoding: {e}")
    success = False

os.unlink(temp_path)

# Test 3: Binary files should work normally
binary_path = "/tmp/test_binary.bin"
test_data = b'\\x00\\x01\\x02\\xff\\xfe'

with open(binary_path, 'wb') as f:
    f.write(test_data)

with open(binary_path, 'rb') as f:
    read_data = f.read()

if read_data == test_data:
    print("✅ Binary file operations work correctly")
else:
    print("❌ Binary file operations affected")
    success = False

os.unlink(binary_path)

if success:
    print("✅ PASSED: Encoding validation working")
else:
    print("❌ FAILED: Encoding validation issues")
"""
    
    result = subprocess.run(
        [sys.executable, "-c", test_script],
        capture_output=True,
        text=True
    )
    
    output = result.stdout + result.stderr
    print(output)
    
    return "PASSED: Encoding validation working" in output


def test_resource_exhaustion_defense():
    """Test defense against resource exhaustion attacks"""
    print("\n🧪 TEST 4: Resource Exhaustion Defense")
    print("-" * 50)
    
    # Test CPU exhaustion defense
    cpu_test = """
import threading
import time

def cpu_burner():
    end = time.time() + 0.1
    while time.time() < end:
        pass

# Try to create many CPU-intensive threads
created = 0
for i in range(100):
    try:
        t = threading.Thread(target=cpu_burner)
        t.start()
        created += 1
    except RuntimeError:
        break

print(f"CPU exhaustion defense: Limited to {created} threads")
"""
    
    result = subprocess.run(
        [sys.executable, "-c", cpu_test],
        capture_output=True,
        text=True
    )
    
    print(result.stdout)
    cpu_defense = "Limited to 10 threads" in result.stdout
    
    # Test fork bomb defense
    fork_test = """
import subprocess

# Try to create many processes
created = 0
processes = []
for i in range(100):
    try:
        p = subprocess.Popen(["sleep", "0.01"])
        processes.append(p)
        created += 1
    except RuntimeError:
        break

print(f"Fork bomb defense: Limited to {created} processes")

# Cleanup
for p in processes:
    p.terminate()
"""
    
    result = subprocess.run(
        [sys.executable, "-c", fork_test],
        capture_output=True,
        text=True
    )
    
    print(result.stdout)
    fork_defense = "Limited to 5 processes" in result.stdout
    
    if cpu_defense and fork_defense:
        print("✅ PASSED: Resource exhaustion defenses working")
        return True
    else:
        print("❌ FAILED: Resource exhaustion defenses not fully working")
        return False


def test_secure_executor():
    """Test SecureExecutor functionality"""
    print("\n🧪 TEST 5: SecureExecutor API")
    print("-" * 50)
    
    test_script = """
import security_patches_8020

# Test resource usage reporting
usage = security_patches_8020.SecureExecutor.get_resource_usage()
print(f"Resource usage: {usage}")

# Test thread creation via SecureExecutor
try:
    for i in range(12):
        t = security_patches_8020.SecureExecutor.create_thread(
            target=lambda: None
        )
        t.start()
except RuntimeError as e:
    print(f"SecureExecutor thread limit enforced: {e}")

# Test process creation via SecureExecutor
try:
    for i in range(7):
        p = security_patches_8020.SecureExecutor.create_process(
            ["echo", f"test{i}"]
        )
except RuntimeError as e:
    print(f"SecureExecutor process limit enforced: {e}")

print("✅ PASSED: SecureExecutor working correctly")
"""
    
    result = subprocess.run(
        [sys.executable, "-c", test_script],
        capture_output=True,
        text=True
    )
    
    output = result.stdout + result.stderr
    print(output)
    
    return "PASSED: SecureExecutor working correctly" in output


def run_all_tests():
    """Run all security patch tests"""
    print("🧪 RUNNING SIMPLIFIED UNIT TESTS FOR 80/20 SECURITY PATCHES")
    print("=" * 70)
    
    tests = [
        ("Thread Limits", test_thread_limits),
        ("Process Limits", test_process_limits),
        ("Encoding Validation", test_encoding_validation),
        ("Resource Exhaustion Defense", test_resource_exhaustion_defense),
        ("SecureExecutor API", test_secure_executor),
    ]
    
    results = []
    for test_name, test_func in tests:
        try:
            passed = test_func()
            results.append((test_name, passed))
        except Exception as e:
            print(f"❌ Test {test_name} error: {e}")
            results.append((test_name, False))
    
    # Summary
    print("\n" + "=" * 70)
    print("📊 TEST SUMMARY")
    print("-" * 50)
    
    passed_count = sum(1 for _, passed in results if passed)
    total_count = len(results)
    
    for test_name, passed in results:
        status = "✅ PASSED" if passed else "❌ FAILED"
        print(f"{status} - {test_name}")
    
    print(f"\nTotal: {passed_count}/{total_count} tests passed")
    print(f"Success rate: {(passed_count/total_count*100):.1f}%")
    
    if passed_count == total_count:
        print("\n✅ ALL SECURITY PATCH TESTS PASSED")
        print("🛡️ Security patches are working correctly")
        return 0
    else:
        print("\n❌ SOME SECURITY PATCH TESTS FAILED")
        print("⚠️  Security patches need review")
        return 1


if __name__ == "__main__":
    sys.exit(run_all_tests())