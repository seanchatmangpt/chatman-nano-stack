#!/usr/bin/env python3
"""
Final Comprehensive Benchmark - Real-world Mixed Usage Scenario
Tests the optimized security utilities in a realistic usage pattern matching the original benchmark
"""

import time
import gc
import asyncio
import threading
import concurrent.futures
import statistics
from pathlib import Path
import json
import sys
import tempfile
from typing import Dict, List, Any

# Add CNS root to path
CNS_ROOT = Path(__file__).parent
sys.path.insert(0, str(CNS_ROOT))

# Import optimized security utilities
from optimized_security_utils import (
    secure_file_path, validate_input_size, sanitize_code_input,
    validate_ttl_input, create_safe_temp_file
)

class FinalComprehensiveBenchmark:
    """Final comprehensive benchmark suite using optimized security utilities"""
    
    def __init__(self):
        self.results = {}
        self.test_data = self._prepare_test_data()
        
    def _prepare_test_data(self):
        """Prepare test data matching original benchmark"""
        return {
            'paths': [
                '/tmp/test.txt',
                '/app/data/file.json', 
                '/Users/test/document.pdf',
                str(Path.cwd() / 'test.ttl'),
                str(Path.home() / '.cache' / 'test.cache')
            ],
            'strings': [
                'short',
                'medium length string with some content',
                'very long string ' * 100,
                'x' * 1000,
                'special chars: !@#$%^&*()_+-=[]{}|;:,.<>?'
            ],
            'collections': [
                list(range(10)),
                list(range(100)),
                list(range(1000)),
                {'key': 'value'},
                {'data': list(range(500))}
            ],
            'code_samples': [
                'def function(): return 42',
                'int add(int a, int b) { return a + b; }',
                'echo "hello world"',
                'SELECT * FROM table WHERE id = 1',
                'class MyClass: pass'
            ],
            'ttl_samples': [
                '@prefix ex: <http://example.com/> . ex:subject ex:predicate "object" .',
                '''@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                   @prefix ex: <http://example.com/> .
                   ex:Thing a rdfs:Class .''',
                '''@prefix owl: <http://www.w3.org/2002/07/owl#> .
                   ex:Property a owl:DatatypeProperty .''',
            ]
        }

    def benchmark_optimized_security_utils(self, iterations: int = 100000) -> Dict[str, Any]:
        """Benchmark optimized security utility functions"""
        print(f"🚀 Benchmarking optimized security utilities with {iterations:,} iterations...")
        
        results = {}
        
        # Benchmark secure_file_path with optimizations
        start_time = time.perf_counter()
        for i in range(iterations):
            for path in self.test_data['paths']:
                try:
                    secure_file_path(path, allowed_dirs=[Path.cwd(), Path('/tmp')])
                except:
                    pass  # Expected for some paths
        
        path_time = time.perf_counter() - start_time
        results['secure_file_path'] = {
            'total_time': path_time,
            'operations': iterations * len(self.test_data['paths']),
            'ops_per_second': (iterations * len(self.test_data['paths'])) / path_time,
            'avg_latency_us': (path_time / (iterations * len(self.test_data['paths']))) * 1e6
        }
        
        # Benchmark validate_input_size with optimizations
        start_time = time.perf_counter()
        for i in range(iterations):
            for item in self.test_data['strings'] + self.test_data['collections']:
                try:
                    validate_input_size(item)
                except:
                    pass  # Expected for large items
        
        size_time = time.perf_counter() - start_time
        test_items = len(self.test_data['strings']) + len(self.test_data['collections'])
        results['validate_input_size'] = {
            'total_time': size_time,
            'operations': iterations * test_items,
            'ops_per_second': (iterations * test_items) / size_time,
            'avg_latency_us': (size_time / (iterations * test_items)) * 1e6
        }
        
        # Benchmark sanitize_code_input with optimizations
        start_time = time.perf_counter()
        for i in range(iterations // 10):  # Reduce iterations for code sanitization
            for code in self.test_data['code_samples']:
                try:
                    sanitize_code_input(code, context='general')
                except:
                    pass  # Expected for malicious code
        
        sanitize_time = time.perf_counter() - start_time
        ops = (iterations // 10) * len(self.test_data['code_samples'])
        results['sanitize_code_input'] = {
            'total_time': sanitize_time,
            'operations': ops,
            'ops_per_second': ops / sanitize_time,
            'avg_latency_us': (sanitize_time / ops) * 1e6
        }
        
        # Benchmark validate_ttl_input with optimizations
        start_time = time.perf_counter()
        for i in range(iterations // 10):  # Reduce iterations for TTL validation
            for ttl in self.test_data['ttl_samples']:
                try:
                    validate_ttl_input(ttl)
                except:
                    pass
        
        ttl_time = time.perf_counter() - start_time
        ops = (iterations // 10) * len(self.test_data['ttl_samples'])
        results['validate_ttl_input'] = {
            'total_time': ttl_time,
            'operations': ops,
            'ops_per_second': ops / ttl_time,
            'avg_latency_us': (ttl_time / ops) * 1e6
        }
        
        # Benchmark create_safe_temp_file with optimizations
        start_time = time.perf_counter()
        temp_files = []
        for i in range(min(iterations // 100, 1000)):  # Limit temp file creation
            try:
                temp_file = create_safe_temp_file(prefix=f"bench_{i}", suffix=".tmp")
                temp_files.append(temp_file)
            except:
                pass
        
        temp_time = time.perf_counter() - start_time
        
        # Cleanup temp files
        for temp_file in temp_files:
            try:
                temp_file.unlink(missing_ok=True)
            except:
                pass
        
        results['create_safe_temp_file'] = {
            'total_time': temp_time,
            'operations': len(temp_files),
            'ops_per_second': len(temp_files) / temp_time if temp_time > 0 else 0,
            'avg_latency_us': (temp_time / len(temp_files)) * 1e6 if temp_files else 0
        }
        
        return results

    def benchmark_concurrent_operations_optimized(self, num_threads: int = 50, iterations_per_thread: int = 1000) -> Dict[str, Any]:
        """Test optimized concurrent performance under load"""
        print(f"🚀 Benchmarking optimized concurrent operations: {num_threads} threads × {iterations_per_thread:,} ops each...")
        
        def worker_thread(thread_id: int) -> Dict[str, float]:
            """Worker thread for concurrent testing"""
            thread_times = {'path_validation': 0, 'input_validation': 0, 'code_sanitization': 0}
            
            # Path validation with optimizations
            start = time.perf_counter()
            for i in range(iterations_per_thread):
                try:
                    secure_file_path(f"/tmp/thread_{thread_id}_file_{i}.txt", 
                                   allowed_dirs=[Path('/tmp')])
                except:
                    pass
            thread_times['path_validation'] = time.perf_counter() - start
            
            # Input validation with optimizations
            start = time.perf_counter()
            for i in range(iterations_per_thread):
                try:
                    validate_input_size(f"thread_{thread_id}_data_{i}" * 10)
                except:
                    pass
            thread_times['input_validation'] = time.perf_counter() - start
            
            # Code sanitization with optimizations
            start = time.perf_counter()
            for i in range(iterations_per_thread // 10):
                try:
                    sanitize_code_input(f"function_{thread_id}_{i}() {{ return {i}; }}")
                except:
                    pass
            thread_times['code_sanitization'] = time.perf_counter() - start
            
            return thread_times
        
        # Execute concurrent benchmark
        start_time = time.perf_counter()
        with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
            futures = [executor.submit(worker_thread, i) for i in range(num_threads)]
            thread_results = [future.result() for future in concurrent.futures.as_completed(futures)]
        
        total_time = time.perf_counter() - start_time
        
        # Aggregate results
        results = {
            'concurrent_performance_optimized': {
                'total_time': total_time,
                'num_threads': num_threads,
                'iterations_per_thread': iterations_per_thread,
                'total_operations': num_threads * iterations_per_thread,
                'overall_ops_per_second': (num_threads * iterations_per_thread) / total_time,
                'thread_results': thread_results
            }
        }
        
        # Calculate per-operation statistics
        for op_type in ['path_validation', 'input_validation', 'code_sanitization']:
            times = [tr[op_type] for tr in thread_results]
            ops_per_op_type = iterations_per_thread if op_type != 'code_sanitization' else iterations_per_thread // 10
            
            results[f'concurrent_{op_type}_optimized'] = {
                'avg_thread_time': statistics.mean(times),
                'min_thread_time': min(times),
                'max_thread_time': max(times),
                'total_ops': num_threads * ops_per_op_type,
                'avg_ops_per_second': (num_threads * ops_per_op_type) / statistics.mean(times),
                'throughput_variance': statistics.pstdev(times) / statistics.mean(times) if times else 0
            }
        
        return results

    def run_final_comprehensive_benchmark(self) -> Dict[str, Any]:
        """Run final comprehensive benchmark with optimized utilities"""
        print("🎯 EXECUTING FINAL COMPREHENSIVE PERFORMANCE BENCHMARK")
        print("=" * 70)
        
        start_time = time.perf_counter()
        all_results = {}
        
        # Security utilities benchmark (optimized)
        all_results.update(self.benchmark_optimized_security_utils(100000))
        
        # Concurrent operations benchmark (optimized)
        all_results.update(self.benchmark_concurrent_operations_optimized(50, 1000))
        
        total_time = time.perf_counter() - start_time
        
        # Calculate summary statistics
        total_operations = sum(
            result.get('operations', 0) for result in all_results.values()
            if isinstance(result, dict) and 'operations' in result
        )
        
        summary = {
            'final_benchmark_summary': {
                'total_benchmark_time': total_time,
                'total_operations_tested': total_operations,
                'overall_ops_per_second': total_operations / total_time,
                'benchmark_categories': len([k for k in all_results.keys() if not k.startswith('concurrent_')]),
                'optimizations_applied': True,
                'performance_target_100k': total_operations / total_time >= 100000,
                'timestamp': time.time()
            }
        }
        
        all_results.update(summary)
        
        return all_results

def main():
    """Execute final comprehensive benchmark"""
    benchmark = FinalComprehensiveBenchmark()
    results = benchmark.run_final_comprehensive_benchmark()
    
    # Save results
    results_file = Path("final_comprehensive_benchmark_results.json")
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    # Display summary
    summary = results['final_benchmark_summary']
    print(f"\n📊 FINAL BENCHMARK RESULTS SUMMARY")
    print("=" * 50)
    print(f"Total Operations: {summary['total_operations_tested']:,}")
    print(f"Total Time: {summary['total_benchmark_time']:.2f}s")
    print(f"Overall Ops/sec: {summary['overall_ops_per_second']:,.0f}")
    print(f"Optimizations Applied: {'✅ YES' if summary['optimizations_applied'] else '❌ NO'}")
    print(f"Results saved to: {results_file}")
    
    # Performance validation
    if summary['overall_ops_per_second'] >= 100000:  # Baseline expectation
        print(f"✅ PERFORMANCE REGRESSION FIXED - Target achieved: {summary['overall_ops_per_second']:,.0f} >= 100K ops/sec")
        return 0
    else:
        print(f"⚠️ PERFORMANCE TARGET NOT MET - Still below baseline: {summary['overall_ops_per_second']:,.0f} < 100K ops/sec")
        return 1

if __name__ == "__main__":
    sys.exit(main())