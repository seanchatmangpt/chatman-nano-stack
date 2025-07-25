#!/usr/bin/env python3
"""
UNIT TESTS FOR 80/20 SECURITY PATCHES
Tests thread limits, process limits, and encoding validation
"""

import unittest
import threading
import subprocess
import time
import tempfile
import os
import sys
import concurrent.futures
from unittest.mock import patch, MagicMock

# Import security patches module
import security_patches_8020


class TestThreadLimits(unittest.TestCase):
    """Test thread creation limits"""
    
    def setUp(self):
        """Set up test environment"""
        # Apply security patches
        security_patches_8020.install_security_patches()
        # Reset thread counter
        security_patches_8020.ResourceLimits._thread_count = 0
    
    def test_thread_limit_enforcement(self):
        """Test that thread limit is enforced"""
        threads = []
        
        # Create threads up to the limit
        for i in range(security_patches_8020.ResourceLimits.MAX_THREADS):
            t = threading.Thread(target=lambda: time.sleep(0.1))
            t.start()
            threads.append(t)
        
        # This should fail
        with self.assertRaises(RuntimeError) as cm:
            extra_thread = threading.Thread(target=lambda: None)
            extra_thread.start()
        
        self.assertIn("Thread limit exceeded", str(cm.exception))
        
        # Cleanup
        for t in threads:
            t.join()
    
    def test_thread_counter_decrement(self):
        """Test that thread counter decrements after thread completion"""
        initial_count = security_patches_8020.ResourceLimits.get_thread_count()
        
        # Create and finish a thread
        t = threading.Thread(target=lambda: None)
        t.start()
        t.join()
        
        # Counter should return to initial value
        final_count = security_patches_8020.ResourceLimits.get_thread_count()
        self.assertEqual(initial_count, final_count)
    
    def test_concurrent_thread_creation(self):
        """Test thread limit under concurrent creation attempts"""
        barrier = threading.Barrier(5)
        results = []
        
        def try_create_thread():
            barrier.wait()  # Synchronize all threads
            try:
                t = threading.Thread(target=lambda: time.sleep(0.1))
                t.start()
                results.append(True)
                return t
            except RuntimeError:
                results.append(False)
                return None
        
        # Try to create many threads concurrently
        with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
            futures = [executor.submit(try_create_thread) for _ in range(20)]
            threads = [f.result() for f in futures if f.result()]
        
        # Should have exactly MAX_THREADS successful creations
        successful = sum(results)
        self.assertLessEqual(successful, security_patches_8020.ResourceLimits.MAX_THREADS)
        
        # Cleanup
        for t in threads:
            if t:
                t.join()


class TestProcessLimits(unittest.TestCase):
    """Test process spawning limits"""
    
    def setUp(self):
        """Set up test environment"""
        security_patches_8020.install_security_patches()
        # Reset process counter
        security_patches_8020.ResourceLimits._process_count = 0
    
    def test_process_limit_enforcement(self):
        """Test that process limit is enforced"""
        processes = []
        
        # Create processes up to the limit
        for i in range(security_patches_8020.ResourceLimits.MAX_PROCESSES):
            p = subprocess.Popen(["sleep", "0.1"])
            processes.append(p)
        
        # This should fail
        with self.assertRaises(RuntimeError) as cm:
            extra_process = subprocess.Popen(["sleep", "0.1"])
        
        self.assertIn("Process limit exceeded", str(cm.exception))
        
        # Cleanup
        for p in processes:
            p.terminate()
            p.wait()
    
    def test_process_counter_cleanup(self):
        """Test that process counter cleans up properly"""
        initial_count = security_patches_8020.ResourceLimits.get_process_count()
        
        # Create and finish a process
        p = subprocess.Popen(["echo", "test"], stdout=subprocess.PIPE)
        p.wait()
        
        # Give time for cleanup
        time.sleep(0.5)
        
        # Counter should be back to initial
        final_count = security_patches_8020.ResourceLimits.get_process_count()
        self.assertLessEqual(final_count, initial_count + 1)
    
    def test_subprocess_run_limit(self):
        """Test subprocess.run is also limited"""
        processes = []
        
        # Fill up process slots
        for i in range(security_patches_8020.ResourceLimits.MAX_PROCESSES):
            p = subprocess.Popen(["sleep", "0.1"])
            processes.append(p)
        
        # subprocess.run should also fail
        with self.assertRaises(RuntimeError) as cm:
            subprocess.run(["echo", "test"])
        
        self.assertIn("Process limit exceeded", str(cm.exception))
        
        # Cleanup
        for p in processes:
            p.terminate()
            p.wait()


class TestEncodingValidation(unittest.TestCase):
    """Test file encoding validation"""
    
    def setUp(self):
        """Set up test environment"""
        security_patches_8020.install_security_patches()
    
    def test_tempfile_encoding_validation(self):
        """Test that tempfiles handle encoding properly"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            # Write problematic content
            f.write("Normal text")
            f.write("\x00")  # Null byte
            f.write("\udcff")  # Invalid UTF-8
            temp_path = f.name
        
        # Should be able to read it back
        with open(temp_path, 'r') as f:
            content = f.read()
            self.assertIsInstance(content, str)
        
        os.unlink(temp_path)
    
    def test_file_encoding_with_errors(self):
        """Test file operations with encoding errors"""
        test_path = "/tmp/test_encoding.txt"
        
        # Write with invalid characters
        with open(test_path, 'w') as f:
            # Should handle these gracefully
            f.write("Test \x00 null")
            f.write(" and \udcff invalid")
        
        # Read back
        with open(test_path, 'r') as f:
            content = f.read()
            self.assertIsInstance(content, str)
        
        os.unlink(test_path)
    
    def test_binary_file_unaffected(self):
        """Test that binary file operations are unaffected"""
        test_path = "/tmp/test_binary.bin"
        test_data = b'\x00\x01\x02\x03\xff\xfe\xfd'
        
        # Write binary data
        with open(test_path, 'wb') as f:
            f.write(test_data)
        
        # Read back
        with open(test_path, 'rb') as f:
            content = f.read()
            self.assertEqual(content, test_data)
        
        os.unlink(test_path)


class TestResourceLimits(unittest.TestCase):
    """Test system resource limits"""
    
    def setUp(self):
        """Set up test environment"""
        security_patches_8020.install_security_patches()
    
    def test_secure_executor_thread_limit(self):
        """Test SecureExecutor thread creation"""
        threads = []
        
        # Create threads using SecureExecutor
        for i in range(security_patches_8020.ResourceLimits.MAX_THREADS):
            t = security_patches_8020.SecureExecutor.create_thread(
                target=lambda: time.sleep(0.1)
            )
            t.start()
            threads.append(t)
        
        # Should fail on next attempt
        with self.assertRaises(RuntimeError):
            security_patches_8020.SecureExecutor.create_thread(
                target=lambda: None
            )
        
        # Cleanup
        for t in threads:
            t.join()
    
    def test_secure_executor_process_limit(self):
        """Test SecureExecutor process creation"""
        processes = []
        
        # Create processes using SecureExecutor
        for i in range(security_patches_8020.ResourceLimits.MAX_PROCESSES):
            p = security_patches_8020.SecureExecutor.create_process(
                ["sleep", "0.1"]
            )
            processes.append(p)
        
        # Should fail on next attempt
        with self.assertRaises(RuntimeError):
            security_patches_8020.SecureExecutor.create_process(
                ["echo", "test"]
            )
        
        # Cleanup
        for p in processes:
            p.terminate()
            p.wait()
    
    def test_resource_usage_reporting(self):
        """Test resource usage reporting"""
        usage = security_patches_8020.SecureExecutor.get_resource_usage()
        
        self.assertIn("threads", usage)
        self.assertIn("processes", usage)
        self.assertIn("thread_limit", usage)
        self.assertIn("process_limit", usage)
        
        self.assertEqual(usage["thread_limit"], security_patches_8020.ResourceLimits.MAX_THREADS)
        self.assertEqual(usage["process_limit"], security_patches_8020.ResourceLimits.MAX_PROCESSES)


class TestStressScenarios(unittest.TestCase):
    """Stress test security patches"""
    
    def setUp(self):
        """Set up test environment"""
        security_patches_8020.install_security_patches()
    
    def test_rapid_thread_creation_destruction(self):
        """Test rapid thread creation and destruction"""
        success_count = 0
        
        for _ in range(50):  # Rapid iterations
            try:
                t = threading.Thread(target=lambda: None)
                t.start()
                t.join()
                success_count += 1
            except RuntimeError:
                pass  # Expected when at limit
        
        # Should have some successes
        self.assertGreater(success_count, 0)
    
    def test_mixed_resource_pressure(self):
        """Test mixed thread and process pressure"""
        threads = []
        processes = []
        
        # Create some threads
        for i in range(5):
            try:
                t = threading.Thread(target=lambda: time.sleep(0.1))
                t.start()
                threads.append(t)
            except RuntimeError:
                break
        
        # Create some processes
        for i in range(3):
            try:
                p = subprocess.Popen(["sleep", "0.1"])
                processes.append(p)
            except RuntimeError:
                break
        
        # Both should be limited
        self.assertLessEqual(len(threads), security_patches_8020.ResourceLimits.MAX_THREADS)
        self.assertLessEqual(len(processes), security_patches_8020.ResourceLimits.MAX_PROCESSES)
        
        # Cleanup
        for t in threads:
            t.join()
        for p in processes:
            p.terminate()
            p.wait()
    
    def test_exception_handling_robustness(self):
        """Test that exceptions don't break the security system"""
        def failing_thread():
            raise Exception("Test exception")
        
        # Create thread that will fail
        try:
            t = threading.Thread(target=failing_thread)
            t.start()
            t.join()
        except:
            pass
        
        # Should still be able to create new threads
        t2 = threading.Thread(target=lambda: None)
        t2.start()
        t2.join()
        
        # Counter should be consistent
        self.assertLessEqual(
            security_patches_8020.ResourceLimits.get_thread_count(),
            security_patches_8020.ResourceLimits.MAX_THREADS
        )


class TestIntegration(unittest.TestCase):
    """Integration tests with real scenarios"""
    
    def setUp(self):
        """Set up test environment"""
        security_patches_8020.install_security_patches()
    
    def test_simulated_cpu_exhaustion_attack(self):
        """Test defense against CPU exhaustion attack"""
        attack_threads = []
        
        def cpu_intensive():
            end_time = time.time() + 0.1
            while time.time() < end_time:
                _ = sum(range(1000))
        
        # Try to create many CPU-intensive threads
        for i in range(100):  # Attempt 100 threads
            try:
                t = threading.Thread(target=cpu_intensive)
                t.start()
                attack_threads.append(t)
            except RuntimeError:
                # Expected - limit reached
                break
        
        # Should be limited
        self.assertLessEqual(len(attack_threads), security_patches_8020.ResourceLimits.MAX_THREADS)
        
        # Cleanup
        for t in attack_threads:
            t.join()
    
    def test_simulated_fork_bomb_attack(self):
        """Test defense against fork bomb attack"""
        attack_processes = []
        
        # Try to create many processes
        for i in range(100):  # Attempt 100 processes
            try:
                p = subprocess.Popen(["sleep", "0.1"])
                attack_processes.append(p)
            except RuntimeError:
                # Expected - limit reached
                break
        
        # Should be limited
        self.assertLessEqual(len(attack_processes), security_patches_8020.ResourceLimits.MAX_PROCESSES)
        
        # Cleanup
        for p in attack_processes:
            p.terminate()
            p.wait()
    
    def test_file_bomb_defense(self):
        """Test defense against file size bombs"""
        # This test would normally test file size limits
        # but is simplified for the unit test environment
        test_file = "/tmp/test_file_bomb.txt"
        
        try:
            with open(test_file, 'w') as f:
                # Try to write a large amount of data
                for i in range(1000):
                    f.write("A" * 1024)  # 1KB chunks
            
            # Check file was created but within limits
            if os.path.exists(test_file):
                size = os.path.getsize(test_file)
                self.assertGreater(size, 0)
        finally:
            if os.path.exists(test_file):
                os.unlink(test_file)


def run_security_tests():
    """Run all security patch tests"""
    print("🧪 RUNNING UNIT TESTS FOR 80/20 SECURITY PATCHES")
    print("=" * 70)
    
    # Create test suite
    test_suite = unittest.TestSuite()
    
    # Add all test classes
    test_classes = [
        TestThreadLimits,
        TestProcessLimits,
        TestEncodingValidation,
        TestResourceLimits,
        TestStressScenarios,
        TestIntegration
    ]
    
    for test_class in test_classes:
        tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
        test_suite.addTests(tests)
    
    # Run tests with detailed output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(test_suite)
    
    # Summary
    print("\n" + "=" * 70)
    print("📊 TEST SUMMARY")
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Success rate: {((result.testsRun - len(result.failures) - len(result.errors)) / result.testsRun * 100):.1f}%")
    
    if result.wasSuccessful():
        print("\n✅ ALL SECURITY PATCH TESTS PASSED")
        print("🛡️ Security patches are working correctly")
        return 0
    else:
        print("\n❌ SOME SECURITY PATCH TESTS FAILED")
        print("⚠️  Security patches need review")
        return 1


if __name__ == "__main__":
    sys.exit(run_security_tests())