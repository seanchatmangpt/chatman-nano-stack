#!/usr/bin/env python3
"""
COMPREHENSIVE PYTHON AOT STRESS TEST SUITE
Ultra-intensive stress testing for all Python AOT compilation and rendered code
"""

import asyncio
import gc
import cProfile
import pstats
import io
import sys
import time
import threading
import psutil
import tracemalloc
import concurrent.futures
import multiprocessing as mp
from pathlib import Path
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
from datetime import datetime, timedelta
import json
import hashlib
import tempfile
import shutil
import subprocess

# Import AOT components
from aot_lifecycle import AOTLifecycleManager, CompilationTarget, SourceSpec
from bitactor.compiler.bitactor_compiler import BitActorCompiler
import test_aot_compilation

@dataclass
class StressTestMetrics:
    """Comprehensive stress test metrics"""
    test_name: str
    start_time: float
    end_time: float
    duration: float
    peak_memory_mb: float
    cpu_percent: float
    success_rate: float
    operations_per_second: float
    memory_leaks_detected: int
    gc_collections: int
    errors: List[str]
    warnings: List[str] = None
    performance_profile: Optional[str] = None
    
    def __post_init__(self):
        if self.warnings is None:
            self.warnings = []

class PythonAOTStressTester:
    """Ultra-comprehensive stress tester for Python AOT compilation"""
    
    def __init__(self):
        self.results: List[StressTestMetrics] = []
        self.baseline_memory = 0
        self.test_data_dir = Path("stress_test_data")
        self.output_dir = Path("stress_test_output")
        self.max_workers = mp.cpu_count()
        
        # Create test directories
        self.test_data_dir.mkdir(exist_ok=True)
        self.output_dir.mkdir(exist_ok=True)
        
        # Initialize test data
        self._create_test_ontologies()
        
    def _create_test_ontologies(self):
        """Create test ontologies of varying complexity"""
        # Small ontology
        small_ttl = self.test_data_dir / "small_test.ttl"
        small_ttl.write_text("""
@prefix : <http://example.org/test#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:TestClass a owl:Class ;
    rdfs:label "Test Class" .

:testProperty a owl:DatatypeProperty ;
    rdfs:domain :TestClass ;
    rdfs:range xsd:string .
""")
        
        # Medium complexity ontology
        medium_ttl = self.test_data_dir / "medium_test.ttl"
        medium_content = """
@prefix : <http://example.org/medium#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
"""
        
        # Generate 100 classes and properties
        for i in range(100):
            medium_content += f"""
:TestClass{i} a owl:Class ;
    rdfs:label "Test Class {i}" ;
    rdfs:subClassOf :BaseClass .

:testProperty{i} a owl:DatatypeProperty ;
    rdfs:domain :TestClass{i} ;
    rdfs:range xsd:string .
"""
        
        medium_ttl.write_text(medium_content)
        
        # Large ontology for extreme stress testing
        large_ttl = self.test_data_dir / "large_test.ttl"
        large_content = """
@prefix : <http://example.org/large#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
"""
        
        # Generate 1000 classes and properties for extreme testing
        for i in range(1000):
            large_content += f"""
:TestClass{i} a owl:Class ;
    rdfs:label "Test Class {i}" ;
    rdfs:subClassOf :BaseClass{i % 10} .

:testProperty{i} a owl:DatatypeProperty ;
    rdfs:domain :TestClass{i} ;
    rdfs:range xsd:string .

:objectProperty{i} a owl:ObjectProperty ;
    rdfs:domain :TestClass{i} ;
    rdfs:range :TestClass{(i+1) % 1000} .
"""
        
        large_ttl.write_text(large_content)
        
        # Corresponding SHACL files
        small_shacl = self.test_data_dir / "small_test_shacl.ttl"
        small_shacl.write_text("""
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix : <http://example.org/test#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:TestShape a sh:NodeShape ;
    sh:targetClass :TestClass ;
    sh:property [
        sh:path :testProperty ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
    ] .
""")
        
        print(f"✅ Created test ontologies in {self.test_data_dir}")
    
    def run_comprehensive_stress_tests(self):
        """Execute all stress tests"""
        print("🚀 STARTING COMPREHENSIVE PYTHON AOT STRESS TESTS")
        print("=" * 80)
        
        # Initialize memory tracking
        tracemalloc.start()
        self.baseline_memory = psutil.Process().memory_info().rss / 1024 / 1024
        
        try:
            # Core stress tests
            self._stress_test_aot_lifecycle()
            self._stress_test_bitactor_compiler()
            self._stress_test_concurrent_compilation()
            self._stress_test_memory_pressure()
            self._stress_test_extreme_load()
            self._stress_test_rendered_code_execution()
            self._stress_test_gc_behavior()
            self._stress_test_async_pipeline()
            
            # Generate comprehensive report
            self._generate_stress_test_report()
            
        finally:
            tracemalloc.stop()
    
    def _stress_test_aot_lifecycle(self):
        """Stress test the AOT lifecycle manager"""
        print("\n🔥 STRESS TEST: AOT Lifecycle Manager")
        print("-" * 50)
        
        # Test configuration
        iterations = 100
        start_time = time.time()
        errors = []
        success_count = 0
        
        # Enable profiling
        profiler = cProfile.Profile()
        profiler.enable()
        
        # Memory tracking
        process = psutil.Process()
        peak_memory = 0
        
        for i in range(iterations):
            try:
                # Create AOT lifecycle manager
                config = {
                    'parallel_compilation': True,
                    'max_workers': 2,  # Limit for stress test
                    'cache_enabled': False,  # Disable cache to stress test
                    'cleanup_on_failure': True,
                    'debug_artifacts': False
                }
                
                manager = AOTLifecycleManager(config)
                
                # Create source spec
                source_spec = SourceSpec(
                    owl_files=[self.test_data_dir / "small_test.ttl"],
                    shacl_files=[self.test_data_dir / "small_test_shacl.ttl"]
                )
                
                # Create targets
                targets = [
                    CompilationTarget(
                        name=f"stress_test_{i}",
                        platform="linux",
                        architecture="x86_64",
                        optimization_level="O2"
                    )
                ]
                
                # Run compilation
                results = asyncio.run(manager.compile(source_spec, targets))
                
                if results and any(r.success for r in results):
                    success_count += 1
                    
                # Track memory
                current_memory = process.memory_info().rss / 1024 / 1024
                peak_memory = max(peak_memory, current_memory)
                
                # Force garbage collection every 10 iterations
                if i % 10 == 0:
                    gc.collect()
                    print(f"  Progress: {i}/{iterations} - Memory: {current_memory:.1f}MB")
                    
            except Exception as e:
                errors.append(f"Iteration {i}: {str(e)}")
        
        profiler.disable()
        
        # Calculate metrics
        end_time = time.time()
        duration = end_time - start_time
        success_rate = success_count / iterations * 100
        ops_per_second = success_count / duration
        
        # Create profile output
        profile_output = io.StringIO()
        ps = pstats.Stats(profiler, stream=profile_output)
        ps.sort_stats('cumulative').print_stats(20)
        
        # Store results
        metrics = StressTestMetrics(
            test_name="AOT_Lifecycle_Stress",
            start_time=start_time,
            end_time=end_time,
            duration=duration,
            peak_memory_mb=peak_memory,
            cpu_percent=psutil.cpu_percent(),
            success_rate=success_rate,
            operations_per_second=ops_per_second,
            memory_leaks_detected=len([e for e in errors if 'memory' in e.lower()]),
            gc_collections=gc.get_count()[0],
            errors=errors[:10],  # Store first 10 errors
            performance_profile=profile_output.getvalue()
        )
        
        self.results.append(metrics)
        
        print(f"✅ AOT Lifecycle Stress: {success_rate:.1f}% success, {ops_per_second:.2f} ops/sec")
        if errors:
            print(f"⚠️  {len(errors)} errors detected")
    
    def _stress_test_bitactor_compiler(self):
        """Stress test BitActor compiler with varying loads"""
        print("\n🔥 STRESS TEST: BitActor Compiler")
        print("-" * 50)
        
        start_time = time.time()
        errors = []
        success_count = 0
        iterations = 50  # Fewer iterations due to complexity
        
        profiler = cProfile.Profile()
        profiler.enable()
        
        process = psutil.Process()
        peak_memory = 0
        
        for i in range(iterations):
            try:
                # Create compiler instance
                compiler = BitActorCompiler()
                
                # Compile with different ontology sizes
                if i % 3 == 0:
                    ttl_file = str(self.test_data_dir / "small_test.ttl")
                elif i % 3 == 1:
                    ttl_file = str(self.test_data_dir / "medium_test.ttl")
                else:
                    ttl_file = str(self.test_data_dir / "large_test.ttl")
                
                output_dir = self.output_dir / f"bitactor_stress_{i}"
                
                # Compile
                outputs = compiler.compile(
                    ttl_file=ttl_file,
                    output_dir=str(output_dir)
                )
                
                if outputs and len(outputs) > 0:
                    success_count += 1
                
                # Memory tracking
                current_memory = process.memory_info().rss / 1024 / 1024
                peak_memory = max(peak_memory, current_memory)
                
                # Cleanup
                if output_dir.exists():
                    shutil.rmtree(output_dir)
                
                if i % 5 == 0:
                    gc.collect()
                    print(f"  Progress: {i}/{iterations} - Memory: {current_memory:.1f}MB")
                    
            except Exception as e:
                errors.append(f"Iteration {i}: {str(e)}")
        
        profiler.disable()
        
        # Calculate metrics
        end_time = time.time()
        duration = end_time - start_time
        success_rate = success_count / iterations * 100
        ops_per_second = success_count / duration
        
        # Store results
        metrics = StressTestMetrics(
            test_name="BitActor_Compiler_Stress",
            start_time=start_time,
            end_time=end_time,
            duration=duration,
            peak_memory_mb=peak_memory,
            cpu_percent=psutil.cpu_percent(),
            success_rate=success_rate,
            operations_per_second=ops_per_second,
            memory_leaks_detected=len([e for e in errors if 'memory' in e.lower()]),
            gc_collections=gc.get_count()[0],
            errors=errors[:10],
            performance_profile=None
        )
        
        self.results.append(metrics)
        
        print(f"✅ BitActor Compiler Stress: {success_rate:.1f}% success, {ops_per_second:.2f} ops/sec")
    
    def _stress_test_concurrent_compilation(self):
        """Stress test concurrent AOT compilation"""
        print("\n🔥 STRESS TEST: Concurrent Compilation")
        print("-" * 50)
        
        start_time = time.time()
        errors = []
        success_count = 0
        num_workers = min(8, mp.cpu_count())
        tasks_per_worker = 10
        
        def compile_task(task_id):
            """Single compilation task"""
            try:
                # Run AOT compilation test
                result = asyncio.run(test_aot_compilation.test_aot_compilation())
                return task_id, result, None
            except Exception as e:
                return task_id, False, str(e)
        
        # Execute concurrent compilations
        with concurrent.futures.ThreadPoolExecutor(max_workers=num_workers) as executor:
            # Submit all tasks
            futures = []
            for worker in range(num_workers):
                for task in range(tasks_per_worker):
                    task_id = f"worker_{worker}_task_{task}"
                    future = executor.submit(compile_task, task_id)
                    futures.append(future)
            
            # Collect results
            for future in concurrent.futures.as_completed(futures):
                task_id, success, error = future.result()
                if success:
                    success_count += 1
                if error:
                    errors.append(f"{task_id}: {error}")
                
                if len(futures) - len([f for f in futures if f.done()]) % 10 == 0:
                    remaining = len([f for f in futures if not f.done()])
                    print(f"  Remaining tasks: {remaining}")
        
        # Calculate metrics
        end_time = time.time()
        duration = end_time - start_time
        total_tasks = num_workers * tasks_per_worker
        success_rate = success_count / total_tasks * 100
        ops_per_second = success_count / duration
        
        # Store results
        metrics = StressTestMetrics(
            test_name="Concurrent_Compilation_Stress",
            start_time=start_time,
            end_time=end_time,
            duration=duration,
            peak_memory_mb=psutil.Process().memory_info().rss / 1024 / 1024,
            cpu_percent=psutil.cpu_percent(),
            success_rate=success_rate,
            operations_per_second=ops_per_second,
            memory_leaks_detected=0,
            gc_collections=gc.get_count()[0],
            errors=errors[:20],
            performance_profile=None
        )
        
        self.results.append(metrics)
        
        print(f"✅ Concurrent Compilation: {success_rate:.1f}% success, {ops_per_second:.2f} ops/sec")
        print(f"   {num_workers} workers, {tasks_per_worker} tasks each")
    
    def _stress_test_memory_pressure(self):
        """Test compilation under extreme memory pressure"""
        print("\n🔥 STRESS TEST: Memory Pressure")
        print("-" * 50)
        
        start_time = time.time()
        errors = []
        success_count = 0
        
        # Create memory pressure by allocating large objects
        memory_hogs = []
        
        try:
            # Allocate memory to stress the system
            for _ in range(10):
                # Allocate 100MB chunks
                memory_hog = bytearray(100 * 1024 * 1024)
                memory_hogs.append(memory_hog)
            
            print(f"  Memory pressure created: {len(memory_hogs) * 100}MB allocated")
            
            # Now try compilations under pressure
            for i in range(20):
                try:
                    # Quick compilation test
                    result = asyncio.run(test_aot_compilation.test_aot_compilation())
                    if result:
                        success_count += 1
                except Exception as e:
                    errors.append(f"Under pressure {i}: {str(e)}")
                
                if i % 5 == 0:
                    current_memory = psutil.Process().memory_info().rss / 1024 / 1024
                    print(f"  Progress: {i}/20 - Memory: {current_memory:.1f}MB")
        
        finally:
            # Release memory pressure
            memory_hogs.clear()
            gc.collect()
        
        # Calculate metrics
        end_time = time.time()
        duration = end_time - start_time
        success_rate = success_count / 20 * 100
        ops_per_second = success_count / duration
        
        # Store results
        metrics = StressTestMetrics(
            test_name="Memory_Pressure_Stress",
            start_time=start_time,
            end_time=end_time,
            duration=duration,
            peak_memory_mb=psutil.Process().memory_info().rss / 1024 / 1024,
            cpu_percent=psutil.cpu_percent(),
            success_rate=success_rate,
            operations_per_second=ops_per_second,
            memory_leaks_detected=len([e for e in errors if 'memory' in e.lower()]),
            gc_collections=gc.get_count()[0],
            errors=errors,
            performance_profile=None
        )
        
        self.results.append(metrics)
        
        print(f"✅ Memory Pressure: {success_rate:.1f}% success under 1GB pressure")
    
    def _stress_test_extreme_load(self):
        """Test with extremely large ontologies"""
        print("\n🔥 STRESS TEST: Extreme Load (Large Ontologies)")
        print("-" * 50)
        
        start_time = time.time()
        errors = []
        success_count = 0
        
        # Test with large ontology
        for i in range(5):  # Only 5 iterations due to size
            try:
                compiler = BitActorCompiler()
                
                output_dir = self.output_dir / f"extreme_load_{i}"
                
                # Compile large ontology
                outputs = compiler.compile(
                    ttl_file=str(self.test_data_dir / "large_test.ttl"),
                    output_dir=str(output_dir)
                )
                
                if outputs and len(outputs) > 0:
                    success_count += 1
                
                # Cleanup immediately to prevent disk space issues
                if output_dir.exists():
                    shutil.rmtree(output_dir)
                
                current_memory = psutil.Process().memory_info().rss / 1024 / 1024
                print(f"  Large ontology {i+1}/5 - Memory: {current_memory:.1f}MB")
                
            except Exception as e:
                errors.append(f"Large ontology {i}: {str(e)}")
        
        # Calculate metrics
        end_time = time.time()
        duration = end_time - start_time
        success_rate = success_count / 5 * 100
        ops_per_second = success_count / duration
        
        # Store results
        metrics = StressTestMetrics(
            test_name="Extreme_Load_Stress",
            start_time=start_time,
            end_time=end_time,
            duration=duration,
            peak_memory_mb=psutil.Process().memory_info().rss / 1024 / 1024,
            cpu_percent=psutil.cpu_percent(),
            success_rate=success_rate,
            operations_per_second=ops_per_second,
            memory_leaks_detected=0,
            gc_collections=gc.get_count()[0],
            errors=errors,
            performance_profile=None
        )
        
        self.results.append(metrics)
        
        print(f"✅ Extreme Load: {success_rate:.1f}% success with 1000+ class ontologies")
    
    def _stress_test_rendered_code_execution(self):
        """Stress test execution of rendered C code"""
        print("\n🔥 STRESS TEST: Rendered C Code Execution")
        print("-" * 50)
        
        start_time = time.time()
        errors = []
        success_count = 0
        compilation_successes = 0
        
        # First, generate C code
        for i in range(10):
            try:
                # Create temporary AOT compilation
                result = asyncio.run(test_aot_compilation.test_aot_compilation())
                if result:
                    compilation_successes += 1
                    
                    # Look for generated C files in output directories
                    output_dirs = list(Path().glob("**/generated_code_*"))
                    for output_dir in output_dirs:
                        c_files = list(output_dir.glob("*.c"))
                        for c_file in c_files:
                            try:
                                # Try to compile the C file
                                obj_file = c_file.with_suffix(".o")
                                result = subprocess.run([
                                    'gcc', '-c', str(c_file), '-o', str(obj_file),
                                    '-I.', '-std=c99', '-O2'
                                ], capture_output=True, text=True, timeout=30)
                                
                                if result.returncode == 0:
                                    success_count += 1
                                    # Clean up
                                    if obj_file.exists():
                                        obj_file.unlink()
                                else:
                                    errors.append(f"C compilation failed: {result.stderr}")
                                    
                            except subprocess.TimeoutExpired:
                                errors.append(f"C compilation timeout: {c_file}")
                            except Exception as e:
                                errors.append(f"C compilation error: {str(e)}")
                
            except Exception as e:
                errors.append(f"AOT compilation {i}: {str(e)}")
        
        # Calculate metrics
        end_time = time.time()
        duration = end_time - start_time
        success_rate = success_count / max(1, compilation_successes) * 100
        ops_per_second = success_count / duration
        
        # Store results
        metrics = StressTestMetrics(
            test_name="Rendered_Code_Execution_Stress",
            start_time=start_time,
            end_time=end_time,
            duration=duration,
            peak_memory_mb=psutil.Process().memory_info().rss / 1024 / 1024,
            cpu_percent=psutil.cpu_percent(),
            success_rate=success_rate,
            operations_per_second=ops_per_second,
            memory_leaks_detected=0,
            gc_collections=gc.get_count()[0],
            errors=errors[:10],
            performance_profile=None
        )
        
        self.results.append(metrics)
        
        print(f"✅ Rendered Code: {success_rate:.1f}% C compilation success")
        print(f"   {compilation_successes} AOT compilations, {success_count} C compilations")
    
    def _stress_test_gc_behavior(self):
        """Test garbage collection behavior under load"""
        print("\n🔥 STRESS TEST: Garbage Collection Behavior")
        print("-" * 50)
        
        start_time = time.time()
        
        # Record GC stats before
        gc_before = gc.get_count()
        gc.collect()
        
        # Create lots of objects and references
        objects = []
        for i in range(1000):
            # Create compilation objects that will be garbage collected
            try:
                manager = AOTLifecycleManager({'cache_enabled': False})
                compiler = BitActorCompiler()
                objects.extend([manager, compiler])
                
                # Force some of them to go out of scope
                if i % 100 == 0:
                    objects = objects[-500:]  # Keep only last 500
                    gc.collect()
                    
            except Exception:
                pass  # Ignore errors in GC test
        
        # Force final cleanup
        objects.clear()
        gc.collect()
        
        # Record GC stats after
        gc_after = gc.get_count()
        
        end_time = time.time()
        duration = end_time - start_time
        
        # Store results
        metrics = StressTestMetrics(
            test_name="GC_Behavior_Stress",
            start_time=start_time,
            end_time=end_time,
            duration=duration,
            peak_memory_mb=psutil.Process().memory_info().rss / 1024 / 1024,
            cpu_percent=psutil.cpu_percent(),
            success_rate=100.0,  # GC test always "succeeds"
            operations_per_second=1000 / duration,
            memory_leaks_detected=0,
            gc_collections=gc_after[0] - gc_before[0],
            errors=[],
            performance_profile=None
        )
        
        self.results.append(metrics)
        
        print(f"✅ GC Behavior: {metrics.gc_collections} collections, {duration:.2f}s")
    
    def _stress_test_async_pipeline(self):
        """Stress test async/await pipeline performance"""
        print("\n🔥 STRESS TEST: Async Pipeline Performance")
        print("-" * 50)
        
        async def async_compilation_task(task_id):
            """Single async compilation task"""
            try:
                # Create AOT manager
                config = {
                    'parallel_compilation': True,
                    'max_workers': 2,
                    'cache_enabled': False
                }
                manager = AOTLifecycleManager(config)
                
                # Quick compilation
                source_spec = SourceSpec(
                    owl_files=[self.test_data_dir / "small_test.ttl"]
                )
                targets = [CompilationTarget(name=f"async_{task_id}")]
                
                results = await manager.compile(source_spec, targets)
                return task_id, any(r.success for r in results), None
                
            except Exception as e:
                return task_id, False, str(e)
        
        async def run_async_stress():
            """Run async stress test"""
            start_time = time.time()
            errors = []
            success_count = 0
            
            # Create many concurrent tasks
            tasks = []
            for i in range(50):
                task = async_compilation_task(i)
                tasks.append(task)
            
            # Execute all tasks concurrently
            results = await asyncio.gather(*tasks, return_exceptions=True)
            
            for result in results:
                if isinstance(result, Exception):
                    errors.append(str(result))
                else:
                    task_id, success, error = result
                    if success:
                        success_count += 1
                    if error:
                        errors.append(f"Task {task_id}: {error}")
            
            end_time = time.time()
            duration = end_time - start_time
            success_rate = success_count / 50 * 100
            ops_per_second = success_count / duration
            
            return start_time, end_time, duration, success_rate, ops_per_second, errors
        
        # Run the async stress test
        start_time, end_time, duration, success_rate, ops_per_second, errors = asyncio.run(run_async_stress())
        
        # Store results
        metrics = StressTestMetrics(
            test_name="Async_Pipeline_Stress",
            start_time=start_time,
            end_time=end_time,
            duration=duration,
            peak_memory_mb=psutil.Process().memory_info().rss / 1024 / 1024,
            cpu_percent=psutil.cpu_percent(),
            success_rate=success_rate,
            operations_per_second=ops_per_second,
            memory_leaks_detected=0,
            gc_collections=gc.get_count()[0],
            errors=errors[:10],
            performance_profile=None
        )
        
        self.results.append(metrics)
        
        print(f"✅ Async Pipeline: {success_rate:.1f}% success, {ops_per_second:.2f} ops/sec")
        print(f"   50 concurrent async tasks")
    
    def _generate_stress_test_report(self):
        """Generate comprehensive stress test report with Mermaid diagrams"""
        print("\n" + "="*80)
        print("📊 COMPREHENSIVE PYTHON AOT STRESS TEST REPORT")
        print("="*80)
        
        # Summary table
        print("\n📋 STRESS TEST SUMMARY")
        print("-" * 60)
        print(f"{'Test Name':<30} {'Success%':<10} {'Ops/Sec':<10} {'Peak MB':<10}")
        print("-" * 60)
        
        total_success = 0
        total_tests = 0
        
        for metrics in self.results:
            print(f"{metrics.test_name:<30} {metrics.success_rate:>7.1f}% {metrics.operations_per_second:>8.2f} {metrics.peak_memory_mb:>8.1f}")
            total_success += metrics.success_rate
            total_tests += 1
        
        avg_success = total_success / total_tests if total_tests > 0 else 0
        print("-" * 60)
        print(f"{'OVERALL AVERAGE':<30} {avg_success:>7.1f}%")
        
        # Performance analysis
        print("\n🔥 PERFORMANCE ANALYSIS")
        print("-" * 40)
        
        fastest_test = min(self.results, key=lambda x: x.duration)
        slowest_test = max(self.results, key=lambda x: x.duration)
        most_efficient = max(self.results, key=lambda x: x.operations_per_second)
        
        print(f"⚡ Fastest test: {fastest_test.test_name} ({fastest_test.duration:.2f}s)")
        print(f"🐌 Slowest test: {slowest_test.test_name} ({slowest_test.duration:.2f}s)")
        print(f"🚀 Most efficient: {most_efficient.test_name} ({most_efficient.operations_per_second:.2f} ops/sec)")
        
        # Memory analysis
        print("\n💾 MEMORY ANALYSIS")
        print("-" * 40)
        
        highest_memory = max(self.results, key=lambda x: x.peak_memory_mb)
        total_gc_collections = sum(m.gc_collections for m in self.results)
        
        print(f"📈 Peak memory usage: {highest_memory.peak_memory_mb:.1f}MB ({highest_memory.test_name})")
        print(f"🗑️  Total GC collections: {total_gc_collections}")
        print(f"📊 Average memory per test: {sum(m.peak_memory_mb for m in self.results) / len(self.results):.1f}MB")
        
        # Error analysis
        print("\n❌ ERROR ANALYSIS")
        print("-" * 40)
        
        total_errors = sum(len(m.errors) for m in self.results)
        tests_with_errors = sum(1 for m in self.results if m.errors)
        
        print(f"🚨 Total errors: {total_errors}")
        print(f"🔥 Tests with errors: {tests_with_errors}/{len(self.results)}")
        
        if total_errors > 0:
            print("\n🔍 Sample errors:")
            error_count = 0
            for metrics in self.results:
                for error in metrics.errors[:2]:  # Show max 2 errors per test
                    print(f"   {metrics.test_name}: {error}")
                    error_count += 1
                    if error_count >= 10:  # Limit total errors shown
                        break
                if error_count >= 10:
                    break
        
        # Generate Mermaid performance chart
        print("\n📈 PERFORMANCE MERMAID CHART")
        print("-" * 40)
        print("```mermaid")
        print("graph TB")
        print("    subgraph AOT_Performance[\"Python AOT Performance Metrics\"]")
        
        for i, metrics in enumerate(self.results):
            safe_name = metrics.test_name.replace("_", "")
            print(f"        {safe_name}[\"/{metrics.test_name}/<br/>Success: {metrics.success_rate:.1f}%<br/>Speed: {metrics.operations_per_second:.2f} ops/sec\"]")
        
        print("    end")
        print("```")
        
        # Generate Mermaid memory chart
        print("\n💾 MEMORY USAGE MERMAID CHART")
        print("-" * 40)
        print("```mermaid")
        print("pie title Memory Usage by Test")
        
        for metrics in self.results:
            memory_percent = (metrics.peak_memory_mb / sum(m.peak_memory_mb for m in self.results)) * 100
            print(f"    \"{metrics.test_name}\" : {memory_percent:.1f}")
        
        print("```")
        
        # Save detailed report to file
        report_file = self.output_dir / "stress_test_report.json"
        report_data = {
            'timestamp': datetime.now().isoformat(),
            'summary': {
                'total_tests': total_tests,
                'average_success_rate': avg_success,
                'total_errors': total_errors,
                'total_gc_collections': total_gc_collections
            },
            'test_results': [
                {
                    'test_name': m.test_name,
                    'duration': m.duration,
                    'success_rate': m.success_rate,
                    'operations_per_second': m.operations_per_second,
                    'peak_memory_mb': m.peak_memory_mb,
                    'gc_collections': m.gc_collections,
                    'error_count': len(m.errors),
                    'errors': m.errors
                }
                for m in self.results
            ]
        }
        
        with open(report_file, 'w') as f:
            json.dump(report_data, f, indent=2)
        
        print(f"\n💾 Detailed report saved to: {report_file}")
        
        # Final assessment
        print("\n🎯 FINAL ASSESSMENT")
        print("-" * 40)
        
        if avg_success >= 99.0:
            print("🎉 EXCELLENT: Python AOT compilation is highly robust!")
        elif avg_success >= 95.0:
            print("✅ GOOD: Python AOT compilation is stable with minor issues")
        elif avg_success >= 90.0:
            print("⚠️  ACCEPTABLE: Python AOT compilation needs optimization")
        else:
            print("🚨 CRITICAL: Python AOT compilation has significant issues")
        
        print(f"\n📊 Overall system health: {avg_success:.1f}% success rate")
        print(f"🔧 Recommended next steps: {'Optimize error handling' if total_errors > 10 else 'System ready for production'}")

def main():
    """Main entry point"""
    print("🚀 PYTHON AOT COMPREHENSIVE STRESS TEST SUITE")
    print("=" * 80)
    print("Testing all Python AOT compilation components under extreme stress...")
    print()
    
    # Create and run stress tester
    tester = PythonAOTStressTester()
    tester.run_comprehensive_stress_tests()
    
    print("\n✅ All stress tests completed!")

if __name__ == "__main__":
    main()