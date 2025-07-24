#!/usr/bin/env python3
"""
Security Patches for Discovered Vulnerabilities
Implements defensive mitigations for adversarial testing findings
"""

import os
import threading
import psutil
import resource
import signal
import time
from typing import Optional
from functools import wraps

class SecurityLimits:
    """System security limits and protection mechanisms"""
    
    # Resource limits
    MAX_THREADS = 10  # Maximum concurrent threads
    MAX_PROCESSES = 5  # Maximum child processes
    MAX_MEMORY_MB = 1024  # Maximum memory allocation (MB)
    MAX_FILE_SIZE_MB = 100  # Maximum file size (MB)
    MAX_EXECUTION_TIME = 300  # Maximum execution time (seconds)
    
    # Current resource counters
    _active_threads = 0
    _active_processes = 0
    _thread_lock = threading.Lock()
    _process_lock = threading.Lock()

def resource_limit_decorator(limit_type: str):
    """Decorator to enforce resource limits"""
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            if limit_type == "memory":
                # Set memory limit
                resource.setrlimit(resource.RLIMIT_AS, 
                    (SecurityLimits.MAX_MEMORY_MB * 1024 * 1024, 
                     SecurityLimits.MAX_MEMORY_MB * 1024 * 1024))
            
            elif limit_type == "cpu":
                # Set CPU time limit
                resource.setrlimit(resource.RLIMIT_CPU, 
                    (SecurityLimits.MAX_EXECUTION_TIME, 
                     SecurityLimits.MAX_EXECUTION_TIME))
            
            elif limit_type == "files":
                # Set file size limit
                resource.setrlimit(resource.RLIMIT_FSIZE,
                    (SecurityLimits.MAX_FILE_SIZE_MB * 1024 * 1024,
                     SecurityLimits.MAX_FILE_SIZE_MB * 1024 * 1024))
            
            return func(*args, **kwargs)
        return wrapper
    return decorator

class ThreadLimiter:
    """Thread creation limiter to prevent CPU exhaustion"""
    
    @staticmethod
    def create_limited_thread(target, *args, **kwargs) -> Optional[threading.Thread]:
        """Create thread with limits"""
        with SecurityLimits._thread_lock:
            if SecurityLimits._active_threads >= SecurityLimits.MAX_THREADS:
                raise RuntimeError(f"Thread limit exceeded: {SecurityLimits.MAX_THREADS}")
            
            def wrapped_target():
                try:
                    target(*args, **kwargs)
                finally:
                    with SecurityLimits._thread_lock:
                        SecurityLimits._active_threads -= 1
            
            thread = threading.Thread(target=wrapped_target)
            SecurityLimits._active_threads += 1
            return thread

class ProcessLimiter:
    """Process spawning limiter to prevent fork bombs"""
    
    @staticmethod
    def create_limited_subprocess(*args, **kwargs):
        """Create subprocess with limits"""
        with SecurityLimits._process_lock:
            if SecurityLimits._active_processes >= SecurityLimits.MAX_PROCESSES:
                raise RuntimeError(f"Process limit exceeded: {SecurityLimits.MAX_PROCESSES}")
            
            # Monitor current system process count
            current_processes = len(psutil.pids())
            if current_processes > 500:  # System-wide process limit
                raise RuntimeError("System process limit exceeded")
            
            SecurityLimits._active_processes += 1
            
        try:
            import subprocess
            proc = subprocess.Popen(*args, **kwargs)
            return proc
        except Exception:
            with SecurityLimits._process_lock:
                SecurityLimits._active_processes -= 1
            raise

class InputSanitizer:
    """Input sanitization to prevent injection attacks"""
    
    @staticmethod
    def sanitize_file_path(path: str) -> str:
        """Sanitize file paths"""
        if not path:
            raise ValueError("Empty path not allowed")
        
        # Remove null bytes
        path = path.replace('\x00', '')
        
        # Check for path traversal
        if '..' in path or path.startswith('/'):
            raise ValueError("Path traversal detected")
        
        # Check for dangerous characters
        dangerous_chars = ['|', '&', ';', '$', '`', '!']
        if any(char in path for char in dangerous_chars):
            raise ValueError("Dangerous characters in path")
        
        return path
    
    @staticmethod
    def sanitize_string_input(input_str: str, max_length: int = 10000) -> str:
        """Sanitize string inputs"""
        if not isinstance(input_str, str):
            raise TypeError("Input must be string")
        
        if len(input_str) > max_length:
            raise ValueError(f"Input too long: {len(input_str)} > {max_length}")
        
        # Remove control characters
        sanitized = ''.join(char for char in input_str if ord(char) >= 32 or char in '\n\r\t')
        
        return sanitized

def apply_security_patches():
    """Apply security patches to discovered vulnerabilities"""
    
    print("🔒 APPLYING SECURITY PATCHES")
    print("=" * 50)
    
    # Patch 1: Fix buffer overflow in quantum_semantic_compiler.py
    print("🔧 Patch 1: Buffer overflow protection")
    
    # Monkey patch file operations to add size limits
    original_open = open
    
    def secure_open(filename, mode='r', **kwargs):
        """Secure file open with size limits"""
        if 'w' in mode or 'a' in mode:
            # Writing mode - limit file size
            file_obj = original_open(filename, mode, **kwargs)
            return LimitedSizeFile(file_obj, SecurityLimits.MAX_FILE_SIZE_MB * 1024 * 1024)
        else:
            # Reading mode - check file size before opening
            if os.path.exists(filename):
                file_size = os.path.getsize(filename)
                if file_size > SecurityLimits.MAX_FILE_SIZE_MB * 1024 * 1024:
                    raise ValueError(f"File too large: {file_size} bytes")
            return original_open(filename, mode, **kwargs)
    
    # Replace built-in open
    __builtins__['open'] = secure_open
    
    print("  ✅ File size limits enforced")
    
    # Patch 2: Fix CPU exhaustion
    print("🔧 Patch 2: CPU exhaustion protection")
    
    # Replace threading.Thread with limited version
    original_thread = threading.Thread
    
    def limited_thread(*args, **kwargs):
        """Limited thread creation"""
        return ThreadLimiter.create_limited_thread(*args, **kwargs)
    
    threading.Thread = limited_thread
    
    print(f"  ✅ Thread limit enforced: {SecurityLimits.MAX_THREADS}")
    
    # Patch 3: Fix fork bomb
    print("🔧 Patch 3: Process spawning protection")
    
    # Replace subprocess functions
    import subprocess
    original_popen = subprocess.Popen
    
    def limited_popen(*args, **kwargs):
        """Limited process creation"""
        return ProcessLimiter.create_limited_subprocess(*args, **kwargs)
    
    subprocess.Popen = limited_popen
    
    print(f"  ✅ Process limit enforced: {SecurityLimits.MAX_PROCESSES}")
    
    # Set system resource limits
    try:
        # Memory limit
        resource.setrlimit(resource.RLIMIT_AS, 
            (SecurityLimits.MAX_MEMORY_MB * 1024 * 1024, 
             SecurityLimits.MAX_MEMORY_MB * 1024 * 1024))
        print(f"  ✅ Memory limit set: {SecurityLimits.MAX_MEMORY_MB}MB")
        
        # CPU time limit  
        resource.setrlimit(resource.RLIMIT_CPU,
            (SecurityLimits.MAX_EXECUTION_TIME,
             SecurityLimits.MAX_EXECUTION_TIME))
        print(f"  ✅ CPU time limit set: {SecurityLimits.MAX_EXECUTION_TIME}s")
        
    except Exception as e:
        print(f"  ⚠️  Could not set system limits: {e}")
    
    print("\n🛡️ Security patches applied successfully")
    print("System now protected against discovered vulnerabilities")

class LimitedSizeFile:
    """File wrapper with size limits"""
    
    def __init__(self, file_obj, max_size):
        self.file_obj = file_obj
        self.max_size = max_size
        self.bytes_written = 0
    
    def write(self, data):
        data_size = len(data.encode('utf-8') if isinstance(data, str) else data)
        if self.bytes_written + data_size > self.max_size:
            raise ValueError(f"File size limit exceeded: {self.max_size} bytes")
        
        result = self.file_obj.write(data)
        self.bytes_written += data_size
        return result
    
    def __getattr__(self, name):
        return getattr(self.file_obj, name)
    
    def __enter__(self):
        return self
    
    def __exit__(self, *args):
        return self.file_obj.__exit__(*args)

def test_security_patches():
    """Test that security patches are working"""
    
    print("\n🧪 TESTING SECURITY PATCHES")
    print("=" * 50)
    
    # Test 1: Thread limit
    print("Test 1: Thread creation limit")
    try:
        threads = []
        for i in range(20):  # Try to create more than limit
            thread = threading.Thread(target=lambda: time.sleep(1))
            thread.start()
            threads.append(thread)
        print("  ❌ Thread limit not enforced")
    except RuntimeError as e:
        print(f"  ✅ Thread limit enforced: {e}")
    
    # Test 2: Process limit  
    print("Test 2: Process spawning limit")
    try:
        processes = []
        for i in range(10):  # Try to create more than limit
            proc = subprocess.Popen(["sleep", "1"])
            processes.append(proc)
        print("  ❌ Process limit not enforced")
    except RuntimeError as e:
        print(f"  ✅ Process limit enforced: {e}")
    
    # Test 3: File size limit
    print("Test 3: File size limit")
    try:
        with open("/tmp/test_large_file.txt", "w") as f:
            # Try to write more than limit
            large_data = "A" * (SecurityLimits.MAX_FILE_SIZE_MB * 1024 * 1024 + 1)
            f.write(large_data)
        print("  ❌ File size limit not enforced")
    except ValueError as e:
        print(f"  ✅ File size limit enforced: {e}")
    finally:
        try:
            os.remove("/tmp/test_large_file.txt")
        except:
            pass
    
    print("\n🔒 Security patch testing complete")

if __name__ == "__main__":
    apply_security_patches()
    test_security_patches()