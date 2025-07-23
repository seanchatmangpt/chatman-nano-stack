#!/usr/bin/env python3
"""
OpenTelemetry Benchmark Suite for OWL Compiler
Measures performance metrics and generates detailed traces
"""

import json
import statistics
import subprocess
import sys
import time
from pathlib import Path
from typing import Any, Dict, List

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

from owl_compiler import OWLCompiler
from owl_compiler_lifecycle import OWLCompilerLifecycle


class OTelBenchmark:
    """OpenTelemetry-style benchmarking for OWL compiler"""

    def __init__(self):
        self.traces = []
        self.metrics = {
            'spans': [],
            'gauges': {},
            'counters': {},
            'histograms': {}
        }
        self.start_time = time.time()

    def create_span(self, name: str, operation: str, attributes: Dict[str, Any] = None):
        """Create a new span for tracing"""
        span = {
            'traceId': f"trace-{int(time.time() * 1000000)}",
            'spanId': f"span-{int(time.time() * 1000000)}",
            'operationName': operation,
            'serviceName': name,
            'startTime': time.time() * 1000000,  # microseconds
            'attributes': attributes or {},
            'events': [],
            'status': 'OK'
        }
        return span

    def end_span(self, span: Dict[str, Any], status: str = 'OK'):
        """End a span and record duration"""
        span['endTime'] = time.time() * 1000000
        span['duration'] = span['endTime'] - span['startTime']
        span['status'] = status
        self.metrics['spans'].append(span)

    def record_gauge(self, name: str, value: float, unit: str = None, labels: Dict[str, str] = None):
        """Record a gauge metric"""
        metric = {
            'name': name,
            'value': value,
            'unit': unit,
            'labels': labels or {},
            'timestamp': time.time() * 1000000
        }
        if name not in self.metrics['gauges']:
            self.metrics['gauges'][name] = []
        self.metrics['gauges'][name].append(metric)

    def increment_counter(self, name: str, value: int = 1, labels: Dict[str, str] = None):
        """Increment a counter metric"""
        key = f"{name}:{json.dumps(labels or {}, sort_keys=True)}"
        if key not in self.metrics['counters']:
            self.metrics['counters'][key] = {
                'name': name,
                'value': 0,
                'labels': labels or {}
            }
        self.metrics['counters'][key]['value'] += value

    def record_histogram(self, name: str, value: float, unit: str = None, labels: Dict[str, str] = None):
        """Record a histogram metric"""
        key = f"{name}:{json.dumps(labels or {}, sort_keys=True)}"
        if key not in self.metrics['histograms']:
            self.metrics['histograms'][key] = {
                'name': name,
                'values': [],
                'unit': unit,
                'labels': labels or {}
            }
        self.metrics['histograms'][key]['values'].append(value)

    def benchmark_owl_compiler(self, test_file: str) -> Dict[str, Any]:
        """Benchmark the basic OWL compiler"""
        span = self.create_span('owl_compiler', 'compile', {
            'file': test_file,
            'compiler': 'owl_compiler.py'
        })

        results = {
            'file': test_file,
            'success': False,
            'metrics': {}
        }

        try:
            # Full compilation with timing
            output_dir = Path('benchmark_output') / Path(test_file).stem
            output_dir.mkdir(parents=True, exist_ok=True)

            compile_start = time.time()
            compiler = OWLCompiler()
            outputs = compiler.compile(Path(test_file), output_dir)
            total_time = (time.time() - compile_start) * 1000  # ms

            self.record_histogram('owl.compile.duration', total_time, 'ms', {'file': test_file})

            # Get metrics from the compiled result
            if hasattr(compiler, 'graph'):
                self.record_gauge('owl.triples.count', len(compiler.graph), 'triples', {'file': test_file})
            if hasattr(compiler, 'classes'):
                self.record_gauge('owl.classes.count', len(compiler.classes), 'classes', {'file': test_file})
            if hasattr(compiler, 'properties'):
                self.record_gauge('owl.properties.count', len(compiler.properties), 'properties', {'file': test_file})

            # Count generated files
            generated_files = []
            for format_name, file_path in outputs.items():
                if file_path and Path(file_path).exists():
                    generated_files.append(file_path)

            self.increment_counter('owl.files.generated', len(generated_files), {'file': test_file})

            # Measure output sizes
            for format_name, output_file in outputs.items():
                if output_file and Path(output_file).exists():
                    size = Path(output_file).stat().st_size
                    self.record_gauge('owl.output.size', size, 'bytes', {
                        'file': test_file,
                        'output': Path(output_file).name
                    })

            # C compilation test
            c_file = outputs.get('c_implementation')
            if c_file and Path(c_file).exists():
                compile_start = time.time()
                result = subprocess.run(['gcc', '-c', c_file, '-o', f'{c_file}.o'],
                                      capture_output=True, text=True)
                compile_time = (time.time() - compile_start) * 1000

                self.record_histogram('c.compile.duration', compile_time, 'ms', {'file': test_file})

                if result.returncode == 0:
                    self.increment_counter('c.compile.success', labels={'file': test_file})
                else:
                    self.increment_counter('c.compile.failure', labels={'file': test_file})

            results['success'] = True
            results['metrics'] = {
                'total_time_ms': total_time,
                'triple_count': len(compiler.graph) if hasattr(compiler, 'graph') else 0,
                'class_count': len(compiler.classes) if hasattr(compiler, 'classes') else 0,
                'property_count': len(compiler.properties) if hasattr(compiler, 'properties') else 0,
                'files_generated': len(generated_files)
            }

            self.end_span(span, 'OK')

        except Exception as e:
            span['events'].append({
                'name': 'error',
                'timestamp': time.time() * 1000000,
                'attributes': {'error': str(e)}
            })
            self.end_span(span, 'ERROR')
            self.increment_counter('owl.compile.errors', labels={'file': test_file})
            results['error'] = str(e)

        return results

    def benchmark_lifecycle_compiler(self, test_files: List[str]) -> Dict[str, Any]:
        """Benchmark the lifecycle compiler"""
        span = self.create_span('owl_lifecycle', 'pipeline', {
            'files': test_files,
            'compiler': 'owl_compiler_lifecycle.py'
        })

        results = {
            'files': test_files,
            'success': False,
            'metrics': {},
            'stage_metrics': {}
        }

        try:
            lifecycle = OWLCompilerLifecycle({
                'optimization_level': 1,
                'enable_inference': True,
                'strict_mode': False
            })

            # Track each stage
            stage_times = {}

            def stage_callback(stage, status, data=None):
                if status == 'start':
                    stage_times[stage] = time.time()
                elif status in ['complete', 'failed']:
                    duration = (time.time() - stage_times.get(stage, time.time())) * 1000
                    self.record_histogram('lifecycle.stage.duration', duration, 'ms', {
                        'stage': stage,
                        'status': status
                    })
                    results['stage_metrics'][stage] = {
                        'duration_ms': duration,
                        'status': status
                    }

            lifecycle.register_callback(stage_callback)

            # Run the pipeline
            pipeline_start = time.time()
            output_dir = Path('benchmark_output/lifecycle')
            output_dir.mkdir(parents=True, exist_ok=True)

            lifecycle_results = lifecycle.compile_ontologies(
                test_files,
                output_dir=str(output_dir),
                output_formats=['c_header', 'c_implementation', 'json']
            )

            pipeline_time = (time.time() - pipeline_start) * 1000

            self.record_histogram('lifecycle.total.duration', pipeline_time, 'ms')

            if lifecycle_results and lifecycle_results.get('success'):
                results['success'] = True
                results['metrics'] = {
                    'total_time_ms': pipeline_time,
                    'stages_completed': len(results['stage_metrics']),
                    'files_processed': len(test_files)
                }
                self.increment_counter('lifecycle.pipeline.success')
            else:
                self.increment_counter('lifecycle.pipeline.failure')

            self.end_span(span, 'OK' if results['success'] else 'ERROR')

        except Exception as e:
            span['events'].append({
                'name': 'error',
                'timestamp': time.time() * 1000000,
                'attributes': {'error': str(e)}
            })
            self.end_span(span, 'ERROR')
            results['error'] = str(e)

        return results

    def run_benchmarks(self):
        """Run all benchmarks"""
        print("🚀 Starting OpenTelemetry Benchmarks for OWL Compiler\n")

        test_files = [
            'test_data/basic_ontology.ttl',
            'test_data/eightfold_ontology.ttl',
            'test_data/shacl_ontology.ttl'
        ]

        # Benchmark individual files with owl_compiler
        print("📊 Benchmarking owl_compiler.py...")
        compiler_results = []
        for test_file in test_files:
            print(f"  - Processing {test_file}...")
            result = self.benchmark_owl_compiler(test_file)
            compiler_results.append(result)

        # Benchmark lifecycle compiler
        print("\n📊 Benchmarking owl_compiler_lifecycle.py...")
        lifecycle_result = self.benchmark_lifecycle_compiler(test_files)

        # Generate report
        self.generate_report(compiler_results, lifecycle_result)

    def generate_report(self, compiler_results: List[Dict], lifecycle_result: Dict):
        """Generate the final benchmark report"""
        print("\n" + "="*60)
        print("📈 OPENTELEMETRY BENCHMARK RESULTS")
        print("="*60)

        # Compiler results
        print("\n🔧 owl_compiler.py Performance:")
        print("-" * 40)

        for result in compiler_results:
            if result['success']:
                metrics = result['metrics']
                print(f"\n📁 {result['file']}:")
                print(f"  ⏱️  Total Time: {metrics['total_time_ms']:.2f}ms")
                print("  📈 Metrics:")
                print(f"     - Triples:    {metrics['triple_count']}")
                print(f"     - Classes:    {metrics['class_count']}")
                print(f"     - Properties: {metrics['property_count']}")
                print(f"     - Files:      {metrics['files_generated']}")

        # Lifecycle results
        print("\n\n🔄 owl_compiler_lifecycle.py Performance:")
        print("-" * 40)

        if lifecycle_result['success']:
            print(f"\n⏱️  Total Pipeline Time: {lifecycle_result['metrics']['total_time_ms']:.2f}ms")
            print("📊 Stage Breakdown:")
            for stage, metrics in lifecycle_result['stage_metrics'].items():
                status_icon = "✅" if metrics['status'] == 'complete' else "❌"
                print(f"   {status_icon} {stage}: {metrics['duration_ms']:.2f}ms")

        # Aggregate metrics
        print("\n\n📊 Aggregate Metrics:")
        print("-" * 40)

        # Calculate histogram statistics
        for name, histogram in self.metrics['histograms'].items():
            values = histogram['values']
            if values:
                key_parts = name.split(':')
                metric_name = key_parts[0]
                print(f"\n📈 {metric_name}:")
                print(f"   - Count:  {len(values)}")
                print(f"   - Min:    {min(values):.2f} {histogram.get('unit', '')}")
                print(f"   - Max:    {max(values):.2f} {histogram.get('unit', '')}")
                print(f"   - Mean:   {statistics.mean(values):.2f} {histogram.get('unit', '')}")
                if len(values) > 1:
                    print(f"   - StdDev: {statistics.stdev(values):.2f} {histogram.get('unit', '')}")

        # Counter metrics
        print("\n\n📊 Counters:")
        print("-" * 40)
        for key, counter in self.metrics['counters'].items():
            print(f"  {counter['name']}: {counter['value']}")

        # Export detailed traces
        self.export_traces()

    def export_traces(self):
        """Export detailed OpenTelemetry traces"""
        output_dir = Path('benchmark_output/otel')
        output_dir.mkdir(parents=True, exist_ok=True)

        # Export spans
        spans_file = output_dir / 'spans.json'
        with open(spans_file, 'w') as f:
            json.dump(self.metrics['spans'], f, indent=2)

        # Export metrics
        metrics_file = output_dir / 'metrics.json'
        with open(metrics_file, 'w') as f:
            json.dump({
                'gauges': self.metrics['gauges'],
                'counters': self.metrics['counters'],
                'histograms': self.metrics['histograms']
            }, f, indent=2)

        print(f"\n\n💾 Detailed traces exported to: {output_dir}")
        print(f"   - Spans: {spans_file}")
        print(f"   - Metrics: {metrics_file}")


if __name__ == '__main__':
    benchmark = OTelBenchmark()
    benchmark.run_benchmarks()
