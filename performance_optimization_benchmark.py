#!/usr/bin/env python3
"""
Performance Optimization Benchmark
Compare original vs optimized security utilities performance
"""

import time
import sys
from pathlib import Path
import statistics

# Add CNS root to path
CNS_ROOT = Path(__file__).parent
sys.path.insert(0, str(CNS_ROOT))

# Import both versions
import security_utils as original
import optimized_security_utils as optimized

def benchmark_function(func, *args, iterations=10000, **kwargs):
    """Benchmark a function with the given arguments"""
    times = []
    
    for _ in range(iterations):
        start = time.perf_counter()
        try:
            func(*args, **kwargs)
        except Exception:
            pass  # Expected for some test cases
        end = time.perf_counter()
        times.append(end - start)
    
    return {
        'total_time': sum(times),
        'avg_time': statistics.mean(times),
        'min_time': min(times),
        'max_time': max(times),
        'ops_per_second': iterations / sum(times)
    }

def run_performance_comparison():
    """Run comprehensive performance comparison"""
    print("🚀 PERFORMANCE OPTIMIZATION BENCHMARK")
    print("=" * 60)
    
    test_paths = [
        '/tmp/test.txt',
        '/app/data/file.json',
        str(Path.cwd() / 'test.ttl'),
        str(Path.home() / '.cache' / 'test.cache')
    ]
    
    test_strings = [
        'short string',
        'medium length string with some content',
        'x' * 1000,
        'function test() { return 42; }'
    ]
    
    test_ttl = '@prefix ex: <http://example.com/> . ex:subject ex:predicate "object" .'
    
    results = {}
    
    print("\n🔍 Testing secure_file_path performance...")
    original_path_results = benchmark_function(
        original.secure_file_path, 
        test_paths[0], 
        [Path('/tmp')], 
        iterations=50000
    )
    
    optimized_path_results = benchmark_function(
        optimized.secure_file_path_optimized,
        test_paths[0],
        [Path('/tmp')],
        iterations=50000
    )
    
    results['secure_file_path'] = {
        'original': original_path_results,
        'optimized': optimized_path_results,
        'improvement_factor': optimized_path_results['ops_per_second'] / original_path_results['ops_per_second']
    }
    
    print("\n🔍 Testing validate_input_size performance...")
    original_size_results = benchmark_function(
        original.validate_input_size,
        test_strings[1],
        iterations=100000
    )
    
    optimized_size_results = benchmark_function(
        optimized.validate_input_size_optimized,
        test_strings[1],
        iterations=100000
    )
    
    results['validate_input_size'] = {
        'original': original_size_results,
        'optimized': optimized_size_results,
        'improvement_factor': optimized_size_results['ops_per_second'] / original_size_results['ops_per_second']
    }
    
    print("\n🔍 Testing sanitize_code_input performance...")
    original_code_results = benchmark_function(
        original.sanitize_code_input,
        test_strings[3],
        'test',
        iterations=50000
    )
    
    optimized_code_results = benchmark_function(
        optimized.sanitize_code_input_optimized,
        test_strings[3],
        'test',
        iterations=50000
    )
    
    results['sanitize_code_input'] = {
        'original': original_code_results,
        'optimized': optimized_code_results,
        'improvement_factor': optimized_code_results['ops_per_second'] / original_code_results['ops_per_second']
    }
    
    print("\n🔍 Testing validate_ttl_input performance...")
    original_ttl_results = benchmark_function(
        original.validate_ttl_input,
        test_ttl,
        iterations=30000
    )
    
    optimized_ttl_results = benchmark_function(
        optimized.validate_ttl_input_optimized,
        test_ttl,
        iterations=30000
    )
    
    results['validate_ttl_input'] = {
        'original': original_ttl_results,
        'optimized': optimized_ttl_results,
        'improvement_factor': optimized_ttl_results['ops_per_second'] / original_ttl_results['ops_per_second']
    }
    
    print("\n🔍 Testing create_safe_temp_file performance...")
    original_temp_results = benchmark_function(
        original.create_safe_temp_file,
        'bench_',
        '.tmp',
        iterations=1000
    )
    
    optimized_temp_results = benchmark_function(
        optimized.create_safe_temp_file_optimized,
        'bench_',
        '.tmp',
        iterations=1000
    )
    
    results['create_safe_temp_file'] = {
        'original': original_temp_results,
        'optimized': optimized_temp_results,
        'improvement_factor': optimized_temp_results['ops_per_second'] / original_temp_results['ops_per_second']
    }
    
    # Calculate overall performance improvement
    total_original_ops = sum(r['original']['ops_per_second'] for r in results.values())
    total_optimized_ops = sum(r['optimized']['ops_per_second'] for r in results.values())
    overall_improvement = total_optimized_ops / total_original_ops
    
    print(f"\n📊 PERFORMANCE COMPARISON RESULTS")
    print("=" * 60)
    
    for func_name, result in results.items():
        print(f"\n{func_name}:")
        print(f"  Original:  {result['original']['ops_per_second']:>12,.0f} ops/sec")
        print(f"  Optimized: {result['optimized']['ops_per_second']:>12,.0f} ops/sec")
        print(f"  Improvement: {result['improvement_factor']:>10.2f}x faster")
    
    print(f"\n🎯 OVERALL PERFORMANCE IMPROVEMENT")
    print(f"Total Original Performance:  {total_original_ops:>12,.0f} ops/sec")
    print(f"Total Optimized Performance: {total_optimized_ops:>12,.0f} ops/sec")
    print(f"Overall Improvement Factor:  {overall_improvement:>12.2f}x")
    
    # Check if we've reached the 100K baseline target
    baseline_target = 100000
    if total_optimized_ops >= baseline_target:
        print(f"✅ PERFORMANCE TARGET ACHIEVED: {total_optimized_ops:,.0f} >= {baseline_target:,} ops/sec")
        return 0
    else:
        print(f"⚠️ PERFORMANCE TARGET NOT MET: {total_optimized_ops:,.0f} < {baseline_target:,} ops/sec")
        return 1

if __name__ == "__main__":
    sys.exit(run_performance_comparison())