#!/usr/bin/env python3
"""
Performance Profiler for TTL2DSPy
Identifies real bottlenecks for 80/20 optimization targeting
"""

import cProfile
import pstats
import time
import io
from pathlib import Path
import rdflib
from rdflib import Graph

# Import current implementations
from ttl2dspy import TTL2DSPyTranspiler as OriginalTranspiler
from ttl2dspy_enhanced import EnhancedTTL2DSPyTranspiler

def profile_original_transpiler(ontology_path: Path, iterations: int = 100):
    """Profile the original transpiler to identify bottlenecks"""
    print(f"Profiling original transpiler with {iterations} iterations...")
    
    def run_original():
        for _ in range(iterations):
            g = Graph()
            g.parse(ontology_path, format="turtle")
            transpiler = OriginalTranspiler()
            signatures = transpiler.build_signatures(g)
    
    profiler = cProfile.Profile()
    start_time = time.time()
    profiler.enable()
    run_original()
    profiler.disable()
    end_time = time.time()
    
    # Capture profile stats
    s = io.StringIO()
    ps = pstats.Stats(profiler, stream=s)
    ps.sort_stats('cumulative').print_stats(20)
    
    return {
        'total_time': end_time - start_time,
        'avg_time': (end_time - start_time) / iterations,
        'profile_stats': s.getvalue()
    }

def profile_enhanced_transpiler(ontology_path: Path, iterations: int = 100):
    """Profile the enhanced transpiler"""
    print(f"Profiling enhanced transpiler with {iterations} iterations...")
    
    def run_enhanced():
        for _ in range(iterations):
            g = Graph()
            g.parse(ontology_path, format="turtle")
            transpiler = EnhancedTTL2DSPyTranspiler(enable_caching=True)
            signatures = transpiler.build_signatures(g)
    
    profiler = cProfile.Profile()
    start_time = time.time()
    profiler.enable()
    run_enhanced()
    profiler.disable()
    end_time = time.time()
    
    # Capture profile stats
    s = io.StringIO()
    ps = pstats.Stats(profiler, stream=s)
    ps.sort_stats('cumulative').print_stats(20)
    
    return {
        'total_time': end_time - start_time,
        'avg_time': (end_time - start_time) / iterations,
        'profile_stats': s.getvalue()
    }

def identify_bottlenecks(profile_results):
    """Analyze profile results to identify 80/20 optimization targets"""
    bottlenecks = []
    
    # Parse profile stats to identify high-impact functions
    lines = profile_results['profile_stats'].split('\n')
    for line in lines[5:15]:  # Skip header, get top 10 functions
        if line.strip() and 'function calls' not in line:
            parts = line.split()
            if len(parts) >= 6:
                cumtime = float(parts[3]) if parts[3].replace('.', '').isdigit() else 0
                function_name = ' '.join(parts[5:])
                if cumtime > 0.001:  # Only functions taking significant time
                    bottlenecks.append({
                        'function': function_name,
                        'cumulative_time': cumtime,
                        'percentage': (cumtime / profile_results['total_time']) * 100
                    })
    
    return bottlenecks

def analyze_memory_usage(ontology_path: Path):
    """Analyze memory usage patterns"""
    import tracemalloc
    
    tracemalloc.start()
    
    # Test original
    g = Graph()
    g.parse(ontology_path, format="turtle")
    transpiler = OriginalTranspiler()
    
    snapshot1 = tracemalloc.take_snapshot()
    signatures = transpiler.build_signatures(g)
    snapshot2 = tracemalloc.take_snapshot()
    
    top_stats = snapshot2.compare_to(snapshot1, 'lineno')
    
    memory_usage = []
    for stat in top_stats[:10]:
        memory_usage.append({
            'file': stat.traceback.format()[-1] if stat.traceback.format() else 'unknown',
            'size_diff': stat.size_diff,
            'count_diff': stat.count_diff
        })
    
    tracemalloc.stop()
    return memory_usage

def main():
    """Run comprehensive profiling analysis"""
    test_ontology = Path("test_enhanced_ontology.ttl")
    
    if not test_ontology.exists():
        print("Test ontology not found. Please run the enhanced system first.")
        return
    
    print("🔍 COMPREHENSIVE PERFORMANCE ANALYSIS")
    print("=" * 50)
    
    # Profile original transpiler
    original_results = profile_original_transpiler(test_ontology, 50)
    print(f"\n📊 Original Transpiler Results:")
    print(f"   Total time: {original_results['total_time']:.4f}s")
    print(f"   Average time per run: {original_results['avg_time']:.6f}s")
    
    # Profile enhanced transpiler
    enhanced_results = profile_enhanced_transpiler(test_ontology, 50)
    print(f"\n📈 Enhanced Transpiler Results:")
    print(f"   Total time: {enhanced_results['total_time']:.4f}s")
    print(f"   Average time per run: {enhanced_results['avg_time']:.6f}s")
    
    # Calculate improvement
    improvement = ((original_results['avg_time'] - enhanced_results['avg_time']) / original_results['avg_time']) * 100
    print(f"   Performance improvement: {improvement:.2f}%")
    
    # Identify bottlenecks
    print(f"\n🎯 BOTTLENECK ANALYSIS (80/20 Opportunities):")
    original_bottlenecks = identify_bottlenecks(original_results)
    
    for i, bottleneck in enumerate(original_bottlenecks[:5], 1):
        print(f"   {i}. {bottleneck['function']}")
        print(f"      Time: {bottleneck['cumulative_time']:.4f}s ({bottleneck['percentage']:.1f}%)")
    
    # Memory analysis
    print(f"\n💾 MEMORY USAGE ANALYSIS:")
    memory_stats = analyze_memory_usage(test_ontology)
    for i, stat in enumerate(memory_stats[:3], 1):
        print(f"   {i}. Size diff: {stat['size_diff']} bytes, Count diff: {stat['count_diff']}")
        print(f"      Location: {stat['file']}")
    
    # Generate optimization recommendations
    print(f"\n🚀 80/20 OPTIMIZATION RECOMMENDATIONS:")
    recommendations = []
    
    # Analyze top bottlenecks for 80/20 opportunities
    total_bottleneck_time = sum(b['cumulative_time'] for b in original_bottlenecks[:3])
    if total_bottleneck_time > original_results['total_time'] * 0.5:
        recommendations.append("🎯 Top 3 functions consume >50% of time - high impact target")
    
    for bottleneck in original_bottlenecks[:3]:
        if 'parse' in bottleneck['function'].lower():
            recommendations.append("📝 RDF parsing optimization - use faster parser or cache parsed graphs")
        elif 'find' in bottleneck['function'].lower() or 'subjects' in bottleneck['function'].lower():
            recommendations.append("🔍 Graph traversal optimization - index common patterns")
        elif 'snake_case' in bottleneck['function'].lower() or 'safe_local_name' in bottleneck['function'].lower():
            recommendations.append("🔤 String processing optimization - pre-compute common transformations")
        elif 'build_signatures' in bottleneck['function'].lower():
            recommendations.append("🏗️ Signature generation optimization - template caching and batch processing")
    
    for rec in recommendations:
        print(f"   {rec}")
    
    # Save detailed results
    with open('profiling_results.txt', 'w') as f:
        f.write("ORIGINAL TRANSPILER PROFILE:\n")
        f.write("=" * 40 + "\n")
        f.write(original_results['profile_stats'])
        f.write("\n\nENHANCED TRANSPILER PROFILE:\n")
        f.write("=" * 40 + "\n")  
        f.write(enhanced_results['profile_stats'])
    
    print(f"\n📄 Detailed results saved to profiling_results.txt")

if __name__ == "__main__":
    main()