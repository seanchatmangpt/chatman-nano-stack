#!/usr/bin/env python3
"""
CNS OpenTelemetry Validation Suite
Validates all CNS implementations against OpenTelemetry metrics
Built for reliability. Designed to last.
"""

import asyncio
import json
import subprocess
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple

# OpenTelemetry imports
from opentelemetry import metrics, trace
from opentelemetry.sdk.metrics import MeterProvider
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.metrics.export import ConsoleMetricExporter, PeriodicExportingMetricReader
from opentelemetry.sdk.trace.export import ConsoleSpanExporter, BatchSpanProcessor


class OTELValidator:
    """Comprehensive OpenTelemetry validation for all CNS components"""
    
    def __init__(self):
        self._setup_telemetry()
        self.validation_results: List[Dict[str, Any]] = []
        self.start_time = time.time()
    
    def _setup_telemetry(self) -> None:
        """Setup OpenTelemetry for validation testing"""
        # Metrics setup with shorter export interval for testing
        metric_reader = PeriodicExportingMetricReader(
            ConsoleMetricExporter(), 
            export_interval_millis=2000
        )
        metrics.set_meter_provider(MeterProvider(metric_readers=[metric_reader]))
        
        # Tracing setup
        trace.set_tracer_provider(TracerProvider())
        tracer_provider = trace.get_tracer_provider()
        span_processor = BatchSpanProcessor(ConsoleSpanExporter())
        tracer_provider.add_span_processor(span_processor)
        
        self.meter = metrics.get_meter("cns.validator", version="1.0.0")
        self.tracer = trace.get_tracer("cns.validator")
        
        # Validation metrics
        self.validation_counter = self.meter.create_counter(
            name="validations_total",
            description="Total number of validations performed",
        )
        
        self.validation_duration = self.meter.create_histogram(
            name="validation_duration_ms",
            description="Validation duration in milliseconds",
            unit="ms"
        )
        
        self.success_rate_gauge = self.meter.create_gauge(
            name="validation_success_rate",
            description="Validation success rate percentage",
            unit="%"
        )
    
    async def run_comprehensive_validation(self) -> Dict[str, Any]:
        """Run comprehensive validation of all CNS components against OTEL"""
        print("🔬 CNS OpenTelemetry Validation Suite")
        print("=====================================")
        print(f"Started: {datetime.now().isoformat()}")
        print()
        
        with self.tracer.start_as_current_span("comprehensive_validation") as root_span:
            # Test all components
            validations = [
                ("CNS Status Command", self._validate_cns_status),
                ("OWL Compiler", self._validate_owl_compiler),
                ("Benchmark System", self._validate_benchmark_system),
                ("Performance Monitor", self._validate_performance_monitor),
                ("Generated C Code", self._validate_generated_code),
                ("OTEL Integration", self._validate_otel_integration)
            ]
            
            for name, validator in validations:
                await self._run_validation_test(name, validator)
            
            # Generate final validation report
            report = self._generate_validation_report()
            
            root_span.set_attributes({
                "total_validations": len(validations),
                "successful_validations": len([r for r in self.validation_results if r["success"]]),
                "validation_duration_s": time.time() - self.start_time
            })
            
            return report
    
    async def _run_validation_test(self, name: str, validator_func) -> None:
        """Run a single validation test with OTEL tracing"""
        start_time = time.time()
        
        with self.tracer.start_as_current_span(f"validate_{name.lower().replace(' ', '_')}") as span:
            try:
                print(f"🧪 Validating {name}...")
                result = await validator_func()
                
                duration_ms = (time.time() - start_time) * 1000
                
                self.validation_results.append({
                    "name": name,
                    "success": result["success"],
                    "duration_ms": duration_ms,
                    "metrics": result.get("metrics", {}),
                    "issues": result.get("issues", []),
                    "timestamp": datetime.now().isoformat()
                })
                
                # Record OTEL metrics
                self.validation_counter.add(1, {"component": name.lower().replace(" ", "_"), "status": "passed" if result["success"] else "failed"})
                self.validation_duration.record(duration_ms, {"component": name.lower().replace(" ", "_")})
                
                span.set_attributes({
                    "validation.success": result["success"],
                    "validation.duration_ms": duration_ms,
                    "validation.metrics_count": len(result.get("metrics", {})),
                    "validation.issues_count": len(result.get("issues", []))
                })
                
                status = "✅ PASS" if result["success"] else "❌ FAIL"
                print(f"   {status} ({duration_ms:.1f}ms)")
                
                if result.get("issues"):
                    for issue in result["issues"]:
                        print(f"   ⚠️  {issue}")
                
            except Exception as e:
                duration_ms = (time.time() - start_time) * 1000
                
                self.validation_results.append({
                    "name": name,
                    "success": False,
                    "duration_ms": duration_ms,
                    "error": str(e),
                    "timestamp": datetime.now().isoformat()
                })
                
                self.validation_counter.add(1, {"component": name.lower().replace(" ", "_"), "status": "error"})
                self.validation_duration.record(duration_ms, {"component": name.lower().replace(" ", "_")})
                
                span.set_attributes({
                    "validation.success": False,
                    "validation.error": str(e),
                    "validation.duration_ms": duration_ms
                })
                
                print(f"   ❌ ERROR ({duration_ms:.1f}ms): {e}")
    
    async def _validate_cns_status(self) -> Dict[str, Any]:
        """Validate CNS status command OTEL integration"""
        issues = []
        metrics = {}
        
        try:
            # Test cns-status command
            result = await asyncio.create_subprocess_exec(
                'uv', 'run', 'python', 'cns_status.py', '--format', 'json',
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd='/Users/sac/cns'
            )
            stdout, stderr = await result.communicate()
            
            if result.returncode != 0:
                issues.append(f"CNS status command failed with code {result.returncode}")
                return {"success": False, "issues": issues}
            
            # Parse JSON output  
            try:
                status_data = json.loads(stdout.decode())
                metrics["health_score"] = status_data.get("health_score", 0)
                metrics["status"] = status_data.get("status", "UNKNOWN")
                
                # Validate expected fields
                required_fields = ["timestamp", "health_score", "status", "system", "cns", "performance"]
                for field in required_fields:
                    if field not in status_data:
                        issues.append(f"Missing required field: {field}")
                
                # Validate OTEL-related fields
                if "performance" in status_data:
                    perf = status_data["performance"]
                    if "avg_latency_ms" not in perf:
                        issues.append("Missing OTEL performance metric: avg_latency_ms")
                    if "throughput_ops_sec" not in perf:
                        issues.append("Missing OTEL performance metric: throughput_ops_sec")
                
            except json.JSONDecodeError:
                issues.append("CNS status output is not valid JSON")
                return {"success": False, "issues": issues}
            
        except Exception as e:
            issues.append(f"CNS status validation error: {e}")
            return {"success": False, "issues": issues}
        
        return {"success": len(issues) == 0, "issues": issues, "metrics": metrics}
    
    async def _validate_owl_compiler(self) -> Dict[str, Any]:
        """Validate OWL compiler functionality and output"""
        issues = []
        metrics = {}
        
        try:
            # Test compilation
            start_time = time.time()
            result = await asyncio.create_subprocess_exec(
                'uv', 'run', 'python', 'owl_compiler.py', 
                'ontologies/generated/realtime/realtime_core.ttl', '--output', 'validation_test',
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd='/Users/sac/cns'
            )
            stdout, stderr = await result.communicate()
            compilation_time = time.time() - start_time
            
            metrics["compilation_time_s"] = compilation_time
            metrics["return_code"] = result.returncode
            
            if result.returncode != 0:
                issues.append(f"OWL compiler failed with code {result.returncode}")
                if stderr:
                    issues.append(f"Stderr: {stderr.decode()[:200]}...")
                return {"success": False, "issues": issues, "metrics": metrics}
            
            # Validate output files
            validation_dir = Path("/Users/sac/cns/validation_test")
            expected_files = ["realtime_core.c", "realtime_core.h", "realtime_core.json", "Makefile"]
            
            for expected_file in expected_files:
                file_path = validation_dir / expected_file
                if not file_path.exists():
                    issues.append(f"Missing expected output file: {expected_file}")
                else:
                    metrics[f"{expected_file}_size"] = file_path.stat().st_size
            
            # Validate generated C code compiles
            if (validation_dir / "realtime_core.c").exists():
                compile_result = await asyncio.create_subprocess_exec(
                    'gcc', '-c', 'realtime_core.c', '-o', 'realtime_core.o',
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                    cwd=str(validation_dir)
                )
                await compile_result.wait()
                
                if compile_result.returncode != 0:
                    issues.append("Generated C code does not compile")
                else:
                    metrics["c_compilation_success"] = True
            
            # Cleanup
            if validation_dir.exists():
                import shutil
                shutil.rmtree(validation_dir)
        
        except Exception as e:
            issues.append(f"OWL compiler validation error: {e}")
            return {"success": False, "issues": issues, "metrics": metrics}
        
        return {"success": len(issues) == 0, "issues": issues, "metrics": metrics}
    
    async def _validate_benchmark_system(self) -> Dict[str, Any]:
        """Validate benchmark system OTEL integration"""
        issues = []
        metrics = {}
        
        try:
            # Test benchmark system
            start_time = time.time()
            result = await asyncio.create_subprocess_exec(
                'uv', 'run', 'python', 'run_benchmark.py',
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd='/Users/sac/cns'
            )
            stdout, stderr = await result.communicate()
            benchmark_time = time.time() - start_time
            
            metrics["benchmark_time_s"] = benchmark_time
            metrics["return_code"] = result.returncode
            
            if result.returncode != 0:
                issues.append(f"Benchmark system failed with code {result.returncode}")
                return {"success": False, "issues": issues, "metrics": metrics}
            
            output = stdout.decode()
            
            # Validate OTEL integration in output
            if "📊 OTEL Metrics:" not in output:
                issues.append("Missing OTEL metrics section in benchmark output")
            
            # Validate Mermaid diagrams
            mermaid_count = output.count("```mermaid")
            metrics["mermaid_diagrams_count"] = mermaid_count
            
            if mermaid_count < 3:
                issues.append(f"Expected at least 3 Mermaid diagrams, found {mermaid_count}")
            
            # Parse performance score
            if "Performance Score:" in output:
                try:
                    score_line = [line for line in output.split('\n') if 'Performance Score:' in line][0]
                    score = float(score_line.split('Performance Score:')[1].strip().split('/')[0])
                    metrics["performance_score"] = score
                    
                    if score < 80.0:
                        issues.append(f"Performance score too low: {score}/100")
                        
                except:
                    issues.append("Could not parse performance score from benchmark output")
            else:
                issues.append("Performance score not found in benchmark output")
        
        except Exception as e:
            issues.append(f"Benchmark validation error: {e}")
            return {"success": False, "issues": issues, "metrics": metrics}
        
        return {"success": len(issues) == 0, "issues": issues, "metrics": metrics}
    
    async def _validate_performance_monitor(self) -> Dict[str, Any]:
        """Validate performance monitor OTEL integration"""
        issues = []
        metrics = {}
        
        try:
            # Test performance monitor with short duration
            start_time = time.time()
            result = await asyncio.create_subprocess_exec(
                'uv', 'run', 'python', 'cns_monitor.py', '--duration', '1', '--interval', '30',
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd='/Users/sac/cns'
            )
            
            # Wait for completion with timeout
            try:
                stdout, stderr = await asyncio.wait_for(result.communicate(), timeout=90.0)
            except asyncio.TimeoutError:
                result.terminate()
                issues.append("Performance monitor validation timed out")
                return {"success": False, "issues": issues, "metrics": metrics}
            
            monitor_time = time.time() - start_time
            metrics["monitor_time_s"] = monitor_time
            metrics["return_code"] = result.returncode
            
            if result.returncode not in [0, -2]:  # 0 = success, -2 = KeyboardInterrupt
                issues.append(f"Performance monitor failed with code {result.returncode}")
            
            output = stdout.decode()
            
            # Validate OTEL metrics presence
            if "cns.monitor" not in output:
                issues.append("OTEL metrics from cns.monitor not found")
            
            # Validate performance monitoring features
            expected_features = [
                "CNS PERFORMANCE MONITOR",
                "system_cpu_percent",
                "system_memory_percent", 
                "cns_health_score",
                "compilation_duration_seconds"
            ]
            
            for feature in expected_features:
                if feature not in output:
                    issues.append(f"Missing expected feature: {feature}")
            
            # Count OTEL metric exports
            otel_exports = output.count('"resource_metrics"')
            metrics["otel_exports_count"] = otel_exports
            
            if otel_exports < 1:
                issues.append("No OTEL metric exports found")
        
        except Exception as e:
            issues.append(f"Performance monitor validation error: {e}")
            return {"success": False, "issues": issues, "metrics": metrics}
        
        return {"success": len(issues) == 0, "issues": issues, "metrics": metrics}
    
    async def _validate_generated_code(self) -> Dict[str, Any]:
        """Validate generated C code functionality"""
        issues = []
        metrics = {}
        
        try:
            binary_path = Path("/Users/sac/cns/live_system/owl_ontology")
            
            if not binary_path.exists():
                issues.append("Generated binary not found")
                return {"success": False, "issues": issues, "metrics": metrics}
            
            metrics["binary_size"] = binary_path.stat().st_size
            
            # Test self-test functionality
            result = await asyncio.create_subprocess_exec(
                str(binary_path), '--self-test',
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await result.communicate()
            
            output = stdout.decode()
            metrics["self_test_return_code"] = result.returncode
            
            # Parse test results
            if "Test Results:" in output:
                try:
                    result_line = [line for line in output.split('\n') if 'Test Results:' in line][0]
                    passed_str = result_line.split('Test Results:')[1].strip().split()[0]
                    passed, total = map(int, passed_str.split('/'))
                    metrics["tests_passed"] = passed
                    metrics["tests_total"] = total
                    
                    if passed < 3:  # At least core tests should pass
                        issues.append(f"Too few tests passed: {passed}/{total}")
                        
                except:
                    issues.append("Could not parse test results")
            else:
                issues.append("Test results not found in self-test output")
            
            # Test other modes
            modes = ["--help", "--deploy-production"]
            for mode in modes:
                mode_result = await asyncio.create_subprocess_exec(
                    str(binary_path), mode,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                await mode_result.wait()
                metrics[f"{mode}_return_code"] = mode_result.returncode
                
                if mode_result.returncode != 0:
                    issues.append(f"Mode {mode} failed with code {mode_result.returncode}")
        
        except Exception as e:
            issues.append(f"Generated code validation error: {e}")
            return {"success": False, "issues": issues, "metrics": metrics}
        
        return {"success": len(issues) == 0, "issues": issues, "metrics": metrics}
    
    async def _validate_otel_integration(self) -> Dict[str, Any]:
        """Validate overall OTEL integration across all components"""
        issues = []
        metrics = {}
        
        try:
            # Test that OTEL is properly configured
            from opentelemetry import metrics as otel_metrics
            from opentelemetry import trace as otel_trace
            
            # Check meter provider
            meter_provider = otel_metrics.get_meter_provider()
            if not meter_provider:
                issues.append("OTEL meter provider not configured")
            else:
                metrics["meter_provider_configured"] = True
            
            # Check tracer provider  
            tracer_provider = otel_trace.get_tracer_provider()
            if not tracer_provider:
                issues.append("OTEL tracer provider not configured")
            else:
                metrics["tracer_provider_configured"] = True
            
            # Test metrics creation
            test_meter = otel_metrics.get_meter("validation.test")
            test_counter = test_meter.create_counter("test_counter")
            test_counter.add(1, {"validation": "otel_integration"})
            metrics["test_metric_created"] = True
            
            # Test tracing
            test_tracer = otel_trace.get_tracer("validation.test")
            with test_tracer.start_as_current_span("test_span") as span:
                span.set_attribute("validation", "otel_integration")
                metrics["test_span_created"] = True
            
        except Exception as e:
            issues.append(f"OTEL integration validation error: {e}")
            return {"success": False, "issues": issues, "metrics": metrics}
        
        return {"success": len(issues) == 0, "issues": issues, "metrics": metrics}
    
    def _generate_validation_report(self) -> Dict[str, Any]:
        """Generate comprehensive validation report with Mermaid diagrams"""
        total_validations = len(self.validation_results)
        successful_validations = len([r for r in self.validation_results if r["success"]])
        success_rate = (successful_validations / total_validations * 100) if total_validations > 0 else 0
        
        # Update success rate gauge
        self.success_rate_gauge.set(success_rate)
        
        print("\n" + "="*80)
        print("🏁 CNS OPENTELEMETRY VALIDATION REPORT")
        print("="*80)
        print(f"Validation Duration: {time.time() - self.start_time:.1f}s")
        print(f"Total Validations: {total_validations}")
        print(f"Successful: {successful_validations}")
        print(f"Failed: {total_validations - successful_validations}")
        print(f"Success Rate: {success_rate:.1f}%")
        print()
        
        # Component results
        for result in self.validation_results:
            status = "✅ PASS" if result["success"] else "❌ FAIL"
            duration = result["duration_ms"]
            print(f"{status} {result['name']:25} ({duration:.1f}ms)")
            
            if not result["success"]:
                issues = result.get("issues", [])
                if result.get("error"):
                    issues.append(result["error"])
                for issue in issues:
                    print(f"     ⚠️  {issue}")
        
        # Mermaid validation flow diagram
        print("\n```mermaid")
        print("graph TD")
        print("    A[CNS OTEL Validation Suite] --> B[CNS Status]")
        print("    A --> C[OWL Compiler]")
        print("    A --> D[Benchmark System]")
        print("    A --> E[Performance Monitor]")
        print("    A --> F[Generated C Code]")
        print("    A --> G[OTEL Integration]")
        
        for result in self.validation_results:
            node_id = result["name"].replace(" ", "").replace("CNS", "").replace("OWL", "")[:1]
            status = "PASS" if result["success"] else "FAIL"
            duration = result["duration_ms"]
            print(f"    {node_id} --> {node_id}1[{status}]")
            print(f"    {node_id}1 --> {node_id}2[{duration:.0f}ms]")
            if result["success"]:
                print(f"    class {node_id}1 pass")
            else:
                print(f"    class {node_id}1 fail")
        
        print("    classDef pass fill:#90EE90")
        print("    classDef fail fill:#FFB6C1")
        print("```")
        
        # Success rate pie chart
        print("\n```mermaid")
        print("pie title CNS OTEL Validation Results")
        print(f'    "Passed" : {successful_validations}')
        print(f'    "Failed" : {total_validations - successful_validations}')
        print("```")
        
        # Performance timeline
        print("\n```mermaid")
        print("timeline")
        print("    title CNS Validation Timeline")
        for result in self.validation_results:
            status = "✅" if result["success"] else "❌"
            duration = result["duration_ms"]
            name = result["name"].replace("CNS ", "").replace(" System", "")
            print(f"    {name} : {status} {duration:.0f}ms")
        print("```")
        
        # Generate final status
        if success_rate == 100.0:
            print(f"\n🎉 ALL VALIDATIONS PASSED - CNS is FULLY VALIDATED against OpenTelemetry")
            final_status = "OPTIMAL"
        elif success_rate >= 80.0:
            print(f"\n⚠️  MOST VALIDATIONS PASSED - CNS is MOSTLY VALIDATED ({success_rate:.1f}%)")
            final_status = "ACCEPTABLE"
        else:
            print(f"\n🚨 VALIDATION FAILURES - CNS needs attention ({success_rate:.1f}% success rate)")
            final_status = "CRITICAL"
        
        return {
            "validation_summary": {
                "total_validations": total_validations,
                "successful_validations": successful_validations,
                "success_rate": success_rate,
                "duration_seconds": time.time() - self.start_time,
                "final_status": final_status
            },
            "validation_results": self.validation_results,
            "timestamp": datetime.now().isoformat()
        }


async def main():
    """Main entry point"""
    validator = OTELValidator()
    
    try:
        await validator.run_comprehensive_validation()
    except KeyboardInterrupt:
        print("\n👋 Validation stopped by user")
    except Exception as e:
        print(f"\n💥 Validation failed with error: {e}")
        raise


if __name__ == "__main__":
    asyncio.run(main())