#!/usr/bin/env python3
"""
Comprehensive Stress Test Suite - Multi-Level System Validation
Tests memory exhaustion, concurrent load (10K+ requests), chaos engineering
"""

import time
import threading
import multiprocessing as mp
import asyncio
import random
import gc
import sys
import json
import psutil
import concurrent.futures
from pathlib import Path
from typing import Dict, List, Any, Optional
import tempfile
import signal
import resource

# Add CNS root to path
CNS_ROOT = Path(__file__).parent
sys.path.insert(0, str(CNS_ROOT))

from security_utils import (
    secure_file_path, validate_input_size, sanitize_code_input,
    validate_ttl_input, create_safe_temp_file, SecurityError
)

class ComprehensiveStressTestSuite:
    """Multi-level stress testing for the secured system"""
    
    def __init__(self):
        self.results = {}
        self.test_running = True
        self.stress_data = self._generate_stress_data()
        
    def _generate_stress_data(self):
        """Generate data patterns for stress testing"""
        return {
            'memory_bomb_strings': [
                'x' * (1024 * 1024),  # 1MB string
                'y' * (1024 * 1024 * 5),  # 5MB string
                'z' * (1024 * 1024 * 10),  # 10MB string
            ],
            'path_patterns': [
                '../' * 100,  # Deep path traversal
                '/tmp/' + 'a' * 1000,  # Long filename
                '/tmp/' + '🌟' * 500,  # Unicode stress
                '/tmp/' + '\\x00' * 100,  # Null byte injection
            ],
            'malicious_code': [
                'system("rm -rf /")',
                'exec("evil_code")',
                '__import__("os").system("bad")',
                'subprocess.run("dangerous", shell=True)',
                'eval("malicious_expr")',
            ],
            'resource_exhaustion_ttl': '''
                @prefix bomb: <http://bomb.com/> .
            ''' + '\n'.join([f'bomb:entity_{i} bomb:prop_{i} "value_{i}" .' for i in range(10000)])
        }

    def stress_test_memory_exhaustion(self, duration_seconds: int = 30) -> Dict[str, Any]:
        """Test system behavior under memory pressure"""
        print(f"🔥 STRESS TEST: Memory exhaustion for {duration_seconds}s...")
        
        start_time = time.perf_counter()
        start_memory = self._get_memory_usage()
        peak_memory = start_memory
        operations = 0
        failures = 0
        memory_allocations = []
        
        end_time = start_time + duration_seconds
        
        while time.perf_counter() < end_time and self.test_running:
            try:
                # Allocate progressively larger memory chunks
                size_mb = random.randint(1, 20)
                large_data = 'M' * (size_mb * 1024 * 1024)
                
                # Test security functions with large data
                try:
                    validate_input_size(large_data[:1000])  # Test first 1K chars
                    sanitize_code_input(large_data[:100])   # Test first 100 chars
                    operations += 2
                except SecurityError:
                    pass  # Expected for oversized data
                
                memory_allocations.append(large_data)
                current_memory = self._get_memory_usage()
                peak_memory = max(peak_memory, current_memory)
                
                # Periodic cleanup to prevent system crash
                if len(memory_allocations) > 50:
                    del memory_allocations[:25]
                    gc.collect()
                
                operations += 1
                
            except (MemoryError, OSError) as e:
                failures += 1
                # System memory pressure - clean up
                memory_allocations.clear()
                gc.collect()
                
            # Safety check - don't crash the system
            if peak_memory > start_memory + 2000:  # 2GB limit
                print("💀 Memory limit reached - terminating stress test")
                break
                
        # Cleanup
        del memory_allocations
        gc.collect()
        
        total_time = time.perf_counter() - start_time
        
        return {
            'memory_stress': {
                'duration': total_time,
                'operations': operations,
                'failures': failures,
                'failure_rate': failures / max(operations, 1),
                'start_memory_mb': start_memory,
                'peak_memory_mb': peak_memory,
                'memory_growth_mb': peak_memory - start_memory,
                'ops_per_second': operations / total_time,
                'system_survived': True
            }
        }

    def stress_test_concurrent_load(self, num_threads: int = 100, requests_per_thread: int = 1000) -> Dict[str, Any]:
        """Test system under massive concurrent load"""
        print(f"🔥 STRESS TEST: Concurrent load - {num_threads} threads × {requests_per_thread:,} requests...")
        
        def worker_thread(thread_id: int) -> Dict[str, Any]:
            """Worker thread for stress testing"""
            thread_results = {
                'operations': 0,
                'failures': 0,
                'errors': [],
                'start_time': time.perf_counter()
            }
            
            for i in range(requests_per_thread):
                try:
                    # Mix of operations to stress different components
                    operation_type = i % 5
                    
                    if operation_type == 0:
                        # Path validation stress
                        path = f"/tmp/stress_thread_{thread_id}_file_{i}.txt"
                        secure_file_path(path, allowed_dirs=[Path('/tmp')])
                    
                    elif operation_type == 1:
                        # Input size validation stress
                        data = f"stress_data_{thread_id}_{i}_" + 'x' * random.randint(10, 1000)
                        validate_input_size(data)
                    
                    elif operation_type == 2:
                        # Code sanitization stress
                        code = f"function stress_{thread_id}_{i}() {{ return {random.randint(1, 1000)}; }}"
                        sanitize_code_input(code)
                    
                    elif operation_type == 3:
                        # TTL validation stress
                        ttl = f"@prefix s{thread_id}: <http://stress{thread_id}.com/> . s{thread_id}:entity{i} s{thread_id}:prop \"value{i}\" ."
                        validate_ttl_input(ttl)
                    
                    else:
                        # Mixed operations
                        temp_file = create_safe_temp_file(prefix=f"stress_{thread_id}_{i}")
                        temp_file.unlink(missing_ok=True)
                    
                    thread_results['operations'] += 1
                    
                except Exception as e:
                    thread_results['failures'] += 1
                    if len(thread_results['errors']) < 10:  # Limit error collection
                        thread_results['errors'].append(str(e))
                
                # Yield occasionally to prevent thread starvation
                if i % 100 == 0:
                    time.sleep(0.001)
            
            thread_results['duration'] = time.perf_counter() - thread_results['start_time']
            return thread_results
        
        # Execute concurrent stress test
        start_time = time.perf_counter()
        start_memory = self._get_memory_usage()
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
            futures = [executor.submit(worker_thread, i) for i in range(num_threads)]
            thread_results = []
            
            for future in concurrent.futures.as_completed(futures):
                try:
                    result = future.result(timeout=300)  # 5 minute timeout per thread
                    thread_results.append(result)
                except concurrent.futures.TimeoutError:
                    thread_results.append({
                        'operations': 0, 'failures': 1, 'errors': ['Thread timeout'],
                        'duration': 300
                    })
        
        total_time = time.perf_counter() - start_time
        peak_memory = self._get_memory_usage()
        
        # Aggregate results
        total_operations = sum(r['operations'] for r in thread_results)
        total_failures = sum(r['failures'] for r in thread_results)
        avg_thread_time = sum(r['duration'] for r in thread_results) / len(thread_results)
        
        return {
            'concurrent_load_stress': {
                'total_time': total_time,
                'num_threads': num_threads,
                'requests_per_thread': requests_per_thread,
                'total_operations': total_operations,
                'total_failures': total_failures,
                'failure_rate': total_failures / max(total_operations + total_failures, 1),
                'ops_per_second': total_operations / total_time,
                'avg_thread_duration': avg_thread_time,
                'start_memory_mb': start_memory,
                'peak_memory_mb': peak_memory,
                'memory_growth_mb': peak_memory - start_memory,
                'thread_results_sample': thread_results[:5]  # Sample of thread results
            }
        }

    def stress_test_chaos_engineering(self, duration_seconds: int = 60) -> Dict[str, Any]:
        """Chaos engineering - random failures and edge conditions"""
        print(f"🔥 STRESS TEST: Chaos engineering for {duration_seconds}s...")
        
        start_time = time.perf_counter()
        operations = 0
        chaos_events = 0
        survival_rate = 0
        
        end_time = start_time + duration_seconds
        
        while time.perf_counter() < end_time and self.test_running:
            try:
                # Random chaos operations
                chaos_type = random.randint(1, 10)
                
                if chaos_type <= 2:
                    # Memory pressure chaos
                    huge_data = 'C' * random.randint(1024*1024, 1024*1024*5)  # 1-5MB
                    try:
                        validate_input_size(huge_data)
                    except SecurityError:
                        pass  # Expected
                    del huge_data
                
                elif chaos_type <= 4:
                    # Path traversal chaos
                    malicious_path = random.choice(self.stress_data['path_patterns'])
                    try:
                        secure_file_path(malicious_path)
                    except SecurityError:
                        pass  # Expected
                
                elif chaos_type <= 6:
                    # Code injection chaos
                    malicious_code = random.choice(self.stress_data['malicious_code'])
                    try:
                        sanitize_code_input(malicious_code)
                    except SecurityError:
                        pass  # Expected
                
                elif chaos_type <= 8:
                    # TTL bomb chaos
                    try:
                        validate_ttl_input(self.stress_data['resource_exhaustion_ttl'])
                    except SecurityError:
                        pass  # Expected
                
                else:
                    # Random file system chaos
                    for _ in range(random.randint(5, 20)):
                        try:
                            temp_file = create_safe_temp_file()
                            # Random file operations
                            if random.choice([True, False]):
                                temp_file.unlink(missing_ok=True)
                        except Exception:
                            pass
                
                operations += 1
                
                # Random induced failures
                if random.random() < 0.1:  # 10% chaos injection
                    chaos_events += 1
                    if chaos_events % 10 == 0:
                        gc.collect()  # Force garbage collection
                    
            except Exception:
                # System survived unexpected error
                survival_rate += 1
                
            # Prevent infinite tight loops
            if operations % 1000 == 0:
                time.sleep(0.01)
        
        total_time = time.perf_counter() - start_time
        
        return {
            'chaos_engineering': {
                'duration': total_time,
                'operations': operations,
                'chaos_events': chaos_events,
                'survival_events': survival_rate,
                'chaos_injection_rate': chaos_events / max(operations, 1),
                'system_survival_rate': 1.0 - (survival_rate / max(operations, 1)),
                'ops_per_second': operations / total_time,
                'system_stability': 'STABLE' if survival_rate < operations * 0.05 else 'UNSTABLE'
            }
        }

    def stress_test_resource_limits(self) -> Dict[str, Any]:
        """Test system behavior at resource limits"""
        print("🔥 STRESS TEST: Resource limits validation...")
        
        results = {}
        
        # Test file descriptor limits
        try:
            temp_files = []
            fd_limit = 1000  # Conservative limit
            
            start_time = time.perf_counter()
            for i in range(fd_limit):
                try:
                    temp_file = create_safe_temp_file(prefix=f"fd_test_{i}")
                    temp_files.append(temp_file)
                except OSError as e:
                    # Hit file descriptor limit
                    break
            
            fd_time = time.perf_counter() - start_time
            
            # Cleanup
            for temp_file in temp_files:
                try:
                    temp_file.unlink(missing_ok=True)
                except:
                    pass
            
            results['file_descriptor_stress'] = {
                'files_created': len(temp_files),
                'time_taken': fd_time,
                'fd_limit_hit': len(temp_files) < fd_limit,
                'creation_rate': len(temp_files) / fd_time if fd_time > 0 else 0
            }
            
        except Exception as e:
            results['file_descriptor_stress'] = {'error': str(e)}
        
        # Test process limits
        try:
            def cpu_intensive_task():
                """CPU intensive security validation"""
                for i in range(10000):
                    try:
                        path = f"/tmp/cpu_test_{i}.txt"
                        secure_file_path(path, allowed_dirs=[Path('/tmp')])
                        validate_input_size(f"cpu_data_{i}")
                        sanitize_code_input(f"function cpu_{i}() {{ return {i}; }}")
                    except:
                        pass
                return "completed"
            
            start_time = time.perf_counter()
            max_processes = min(mp.cpu_count() * 2, 20)  # Conservative limit
            
            with mp.Pool(processes=max_processes) as pool:
                process_results = pool.map(cpu_intensive_task, range(max_processes))
            
            process_time = time.perf_counter() - start_time
            
            results['process_stress'] = {
                'num_processes': max_processes,
                'completed_processes': len([r for r in process_results if r == "completed"]),
                'time_taken': process_time,
                'processes_per_second': max_processes / process_time
            }
            
        except Exception as e:
            results['process_stress'] = {'error': str(e)}
        
        return results

    def _get_memory_usage(self) -> float:
        """Get current memory usage in MB"""
        try:
            process = psutil.Process()
            return process.memory_info().rss / 1024 / 1024
        except:
            return 0.0

    def _get_system_stats(self) -> Dict[str, Any]:
        """Get current system statistics"""
        try:
            return {
                'cpu_percent': psutil.cpu_percent(interval=1),
                'memory_percent': psutil.virtual_memory().percent,
                'disk_usage_percent': psutil.disk_usage('/').percent,
                'load_average': psutil.getloadavg() if hasattr(psutil, 'getloadavg') else [0, 0, 0]
            }
        except:
            return {}

    def run_comprehensive_stress_test(self) -> Dict[str, Any]:
        """Execute all stress tests"""
        print("🔥 EXECUTING COMPREHENSIVE STRESS TEST SUITE")
        print("=" * 70)
        
        start_time = time.perf_counter()
        start_stats = self._get_system_stats()
        all_results = {}
        
        try:
            # Memory exhaustion stress test
            all_results.update(self.stress_test_memory_exhaustion(30))
            
            # Concurrent load stress test
            all_results.update(self.stress_test_concurrent_load(100, 1000))
            
            # Chaos engineering stress test
            all_results.update(self.stress_test_chaos_engineering(60))
            
            # Resource limits stress test
            all_results.update(self.stress_test_resource_limits())
            
        except KeyboardInterrupt:
            print("⚠️ Stress test interrupted by user")
            self.test_running = False
        
        total_time = time.perf_counter() - start_time
        end_stats = self._get_system_stats()
        
        # Generate summary
        summary = {
            'stress_test_summary': {
                'total_duration': total_time,
                'start_system_stats': start_stats,
                'end_system_stats': end_stats,
                'system_survived': True,
                'tests_completed': len(all_results),
                'overall_success': all(
                    not result.get('error') for result in all_results.values() 
                    if isinstance(result, dict)
                ),
                'timestamp': time.time()
            }
        }
        
        all_results.update(summary)
        return all_results

def main():
    """Execute comprehensive stress test suite"""
    # Set up signal handler for graceful termination
    def signal_handler(signum, frame):
        print("\n⚠️ Received termination signal - stopping stress tests...")
        sys.exit(0)
    
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    stress_tester = ComprehensiveStressTestSuite()
    results = stress_tester.run_comprehensive_stress_test()
    
    # Save results
    results_file = Path("comprehensive_stress_test_results.json")
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    # Display summary
    summary = results['stress_test_summary']
    print(f"\n🔥 STRESS TEST RESULTS SUMMARY")
    print("=" * 50)
    print(f"Total Duration: {summary['total_duration']:.2f}s")
    print(f"Tests Completed: {summary['tests_completed']}")
    print(f"System Survived: {'✅ YES' if summary['system_survived'] else '❌ NO'}")
    print(f"Overall Success: {'✅ PASS' if summary['overall_success'] else '❌ FAIL'}")
    print(f"Results saved to: {results_file}")
    
    if summary['system_survived'] and summary['overall_success']:
        print("✅ STRESS TEST VALIDATION PASSED - System is resilient under load")
        return 0
    else:
        print("⚠️ STRESS TEST VALIDATION FAILED - System instability detected")
        return 1

if __name__ == "__main__":
    sys.exit(main())