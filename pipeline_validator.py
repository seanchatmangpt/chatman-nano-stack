#!/usr/bin/env python3
"""
CNS Pipeline Validator - Comprehensive System Validation
Validates entire pipeline with benchmarks, OTEL, and best practices
Built for reliability. Designed to last.
"""

import asyncio
import json
import subprocess
import time
import psutil
import os
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, field

# OpenTelemetry imports
from opentelemetry import metrics, trace
from opentelemetry.sdk.metrics import MeterProvider
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.metrics.export import ConsoleMetricExporter, PeriodicExportingMetricReader
from opentelemetry.sdk.trace.export import ConsoleSpanExporter, BatchSpanProcessor
from opentelemetry.instrumentation.system_metrics import SystemMetricsInstrumentor


@dataclass
class ValidationResult:
    """Validation result with detailed metrics"""
    component: str
    status: str  # PASS, FAIL, WARN
    score: float
    metrics: Dict[str, Any] = field(default_factory=dict)
    issues: List[str] = field(default_factory=list)
    recommendations: List[str] = field(default_factory=list)
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())


class CNSPipelineValidator:
    """Comprehensive pipeline validation system"""
    
    def __init__(self):
        self._setup_telemetry()
        self.results: List[ValidationResult] = []
        self.start_time = time.time()
        self.best_practices = self._load_best_practices()
        
    def _setup_telemetry(self) -> None:
        """Setup comprehensive OpenTelemetry instrumentation"""
        # Metrics setup
        metric_reader = PeriodicExportingMetricReader(
            ConsoleMetricExporter(), export_interval_millis=5000
        )
        metrics.set_meter_provider(MeterProvider(metric_readers=[metric_reader]))
        
        # Tracing setup
        trace.set_tracer_provider(TracerProvider())
        tracer_provider = trace.get_tracer_provider()
        span_processor = BatchSpanProcessor(ConsoleSpanExporter())
        tracer_provider.add_span_processor(span_processor)
        
        # System metrics instrumentation
        SystemMetricsInstrumentor().instrument()
        
        self.meter = metrics.get_meter("cns.pipeline.validator", version="1.0.0")
        self.tracer = trace.get_tracer("cns.pipeline.validator")
        
        # Validation metrics
        self.validation_histogram = self.meter.create_histogram(
            name="pipeline_validation_duration_ms",
            description="Pipeline validation duration",
            unit="ms"
        )
        
        self.component_score_gauge = self.meter.create_gauge(
            name="component_validation_score",
            description="Component validation score (0-100)",
            unit="score"
        )
        
        self.best_practice_violations = self.meter.create_counter(
            name="best_practice_violations_total",
            description="Total best practice violations",
        )
        
        self.pipeline_health_gauge = self.meter.create_gauge(
            name="pipeline_health_score",
            description="Overall pipeline health score",
            unit="score"
        )
    
    def _load_best_practices(self) -> Dict[str, Any]:
        """Load best practices configuration"""
        return {
            "performance": {
                "latency_threshold_ms": 8,  # 8-tick requirement
                "throughput_min_ops": 1000000,  # 1M ops/sec
                "memory_efficiency": 0.95,  # 95% efficiency
                "cpu_efficiency": 0.90,  # 90% efficiency
            },
            "reliability": {
                "error_rate_max": 0.0001,  # Six Sigma (3.4 defects per million)
                "availability_min": 0.9999,  # 99.99% uptime
                "mtbf_hours": 8760,  # Mean time between failures (1 year)
            },
            "code_quality": {
                "test_coverage_min": 0.80,  # 80% coverage
                "cyclomatic_complexity_max": 10,
                "documentation_coverage": 0.90,  # 90% documented
            },
            "security": {
                "vulnerability_score_max": 3.0,  # CVSS score
                "encryption_required": True,
                "audit_logging": True,
            },
            "deployment": {
                "rollback_time_max_seconds": 60,
                "deployment_success_rate": 0.95,
                "monitoring_coverage": 1.0,  # 100% monitoring
            }
        }
    
    async def validate_pipeline(self) -> Dict[str, Any]:
        """Run comprehensive pipeline validation"""
        print("🔬 CNS Pipeline Validation System")
        print("=" * 80)
        print(f"Started: {datetime.now().isoformat()}")
        print(f"Validating against best practices and 8T-8H-8M requirements")
        print()
        
        with self.tracer.start_as_current_span("pipeline_validation") as root_span:
            # Core validations
            validations = [
                ("Build System", self._validate_build_system),
                ("Performance Benchmarks", self._validate_performance),
                ("OpenTelemetry Integration", self._validate_otel_integration),
                ("Code Quality", self._validate_code_quality),
                ("Security Posture", self._validate_security),
                ("Deployment Pipeline", self._validate_deployment),
                ("Monitoring & Observability", self._validate_monitoring),
                ("Neural Integration", self._validate_neural_integration),
                ("Error Handling", self._validate_error_handling),
                ("Documentation", self._validate_documentation),
            ]
            
            for name, validator in validations:
                await self._run_validation(name, validator)
            
            # Generate comprehensive report
            report = self._generate_validation_report()
            
            root_span.set_attributes({
                "total_validations": len(validations),
                "passed_validations": len([r for r in self.results if r.status == "PASS"]),
                "pipeline_health_score": report["summary"]["overall_score"],
                "duration_seconds": time.time() - self.start_time
            })
            
            return report
    
    async def _run_validation(self, name: str, validator_func) -> None:
        """Run individual validation with telemetry"""
        start_time = time.time()
        
        with self.tracer.start_as_current_span(f"validate_{name.lower().replace(' ', '_')}") as span:
            try:
                print(f"🧪 Validating {name}...")
                result = await validator_func()
                
                duration_ms = (time.time() - start_time) * 1000
                self.validation_histogram.record(duration_ms, {"component": name})
                self.component_score_gauge.set(result.score, {"component": name})
                
                # Count violations
                if result.issues:
                    self.best_practice_violations.add(
                        len(result.issues), 
                        {"component": name, "severity": "high" if result.status == "FAIL" else "medium"}
                    )
                
                self.results.append(result)
                
                # Display result
                status_icon = {"PASS": "✅", "FAIL": "❌", "WARN": "⚠️ "}.get(result.status, "❓")
                print(f"   {status_icon} {result.status} - Score: {result.score:.1f}/100 ({duration_ms:.1f}ms)")
                
                if result.issues:
                    for issue in result.issues[:3]:  # Show first 3 issues
                        print(f"      ⚠️  {issue}")
                    if len(result.issues) > 3:
                        print(f"      ... and {len(result.issues) - 3} more issues")
                
                span.set_attributes({
                    "validation.status": result.status,
                    "validation.score": result.score,
                    "validation.issues_count": len(result.issues),
                    "validation.duration_ms": duration_ms
                })
                
            except Exception as e:
                duration_ms = (time.time() - start_time) * 1000
                
                result = ValidationResult(
                    component=name,
                    status="FAIL",
                    score=0.0,
                    issues=[f"Validation error: {str(e)}"]
                )
                self.results.append(result)
                
                print(f"   ❌ ERROR - {str(e)} ({duration_ms:.1f}ms)")
                
                span.set_attributes({
                    "validation.status": "ERROR",
                    "validation.error": str(e),
                    "validation.duration_ms": duration_ms
                })
    
    async def _validate_build_system(self) -> ValidationResult:
        """Validate build system and compilation"""
        issues = []
        recommendations = []
        metrics = {}
        
        # Check build tools
        tools = ["uv", "gcc", "clang", "make", "python3"]
        missing_tools = []
        
        for tool in tools:
            if subprocess.run(["which", tool], capture_output=True).returncode != 0:
                missing_tools.append(tool)
        
        if missing_tools:
            issues.append(f"Missing build tools: {', '.join(missing_tools)}")
        
        # Test compilation
        try:
            start_time = time.time()
            result = await asyncio.create_subprocess_exec(
                "uv", "run", "python", "owl_compiler.py",
                "ontologies/generated/uhft/uhft_core.ttl", "--output", "test_build",
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await result.communicate()
            compile_time = time.time() - start_time
            
            metrics["compilation_time_s"] = compile_time
            metrics["compilation_success"] = result.returncode == 0
            
            if result.returncode != 0:
                issues.append("OWL compilation failed")
                if stderr:
                    issues.append(f"Compilation error: {stderr.decode()[:100]}...")
            
            # Check optimization flags
            if Path("test_build/Makefile").exists():
                makefile_content = Path("test_build/Makefile").read_text()
                if "-O3" not in makefile_content:
                    issues.append("Missing -O3 optimization flag")
                    recommendations.append("Add -O3 flag for production builds")
                if "-march=native" not in makefile_content:
                    recommendations.append("Consider -march=native for target-specific optimizations")
            
            # Cleanup
            if Path("test_build").exists():
                import shutil
                shutil.rmtree("test_build")
                
        except Exception as e:
            issues.append(f"Build validation error: {e}")
        
        # Calculate score
        score = 100.0
        score -= len(issues) * 10
        score -= len(missing_tools) * 20
        score = max(0, score)
        
        return ValidationResult(
            component="Build System",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    async def _validate_performance(self) -> ValidationResult:
        """Validate performance against 8T-8H-8M requirements"""
        issues = []
        recommendations = []
        metrics = {}
        
        try:
            # Run benchmark
            result = await asyncio.create_subprocess_exec(
                "uv", "run", "python", "run_benchmark.py",
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await result.communicate()
            output = stdout.decode()
            
            # Extract metrics
            if "Performance Score:" in output:
                score_line = [l for l in output.split('\n') if 'Performance Score:' in l][0]
                perf_score = float(score_line.split(':')[1].split('/')[0].strip())
                metrics["performance_score"] = perf_score
                
                if perf_score < 80:
                    issues.append(f"Performance score below threshold: {perf_score}/100")
            
            # Check 8-tick latency
            if "Average operation latency:" in output:
                latency_line = [l for l in output.split('\n') if 'Average operation latency:' in l][0]
                latency_ns = float(latency_line.split(':')[1].split('ns')[0].strip())
                metrics["latency_ns"] = latency_ns
                
                # Convert to CPU cycles (assuming 3GHz CPU)
                cpu_cycles = latency_ns * 3.0
                metrics["latency_cycles"] = cpu_cycles
                
                if cpu_cycles > 8:
                    issues.append(f"Latency exceeds 8-tick requirement: {cpu_cycles:.1f} cycles")
                    recommendations.append("Optimize hot paths and reduce memory allocations")
            
            # Check throughput
            if "ops/sec" in output:
                throughput_line = [l for l in output.split('\n') if 'ops/sec' in l][0]
                throughput = float(throughput_line.split(':')[1].split('ops/sec')[0].strip())
                metrics["throughput_ops_sec"] = throughput
                
                if throughput < self.best_practices["performance"]["throughput_min_ops"]:
                    issues.append(f"Throughput below 1M ops/sec: {throughput:.0f}")
                    recommendations.append("Consider batch processing and SIMD optimizations")
            
        except Exception as e:
            issues.append(f"Performance validation error: {e}")
            return ValidationResult(
                component="Performance Benchmarks",
                status="FAIL",
                score=0.0,
                issues=issues
            )
        
        # Calculate score based on 8T-8H-8M compliance
        score = 100.0
        if metrics.get("latency_cycles", 999) > 8:
            score -= 30  # Major penalty for breaking 8-tick contract
        if metrics.get("throughput_ops_sec", 0) < 1000000:
            score -= 20
        score -= len(issues) * 10
        score = max(0, score)
        
        return ValidationResult(
            component="Performance Benchmarks",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    async def _validate_otel_integration(self) -> ValidationResult:
        """Validate OpenTelemetry integration completeness"""
        issues = []
        recommendations = []
        metrics = {}
        
        try:
            # Run OTEL validation
            result = await asyncio.create_subprocess_exec(
                "uv", "run", "python", "validate_otel.py",
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await result.communicate()
            output = stdout.decode()
            
            # Parse validation results
            if "Success Rate:" in output:
                rate_line = [l for l in output.split('\n') if 'Success Rate:' in l][0]
                success_rate = float(rate_line.split(':')[1].split('%')[0].strip())
                metrics["otel_success_rate"] = success_rate
                
                if success_rate < 100:
                    issues.append(f"OTEL validation incomplete: {success_rate}%")
            
            # Check for OTEL exports in output
            otel_exports = output.count('"resource_metrics"')
            trace_exports = output.count('"name":')
            
            metrics["metric_exports"] = otel_exports
            metrics["trace_exports"] = trace_exports
            
            if otel_exports < 5:
                issues.append(f"Insufficient OTEL metric exports: {otel_exports}")
                recommendations.append("Ensure all components export metrics")
            
            if trace_exports < 10:
                recommendations.append("Add more distributed tracing spans")
            
            # Check OTEL configuration
            required_components = [
                "cns.health", "cns.monitor", "cns.neural", "cns.validator"
            ]
            for component in required_components:
                if component not in output:
                    issues.append(f"Missing OTEL instrumentation for {component}")
            
        except Exception as e:
            issues.append(f"OTEL validation error: {e}")
        
        # Calculate score
        score = metrics.get("otel_success_rate", 0)
        if score == 0:
            score = 50 if len(issues) < 5 else 25
        
        return ValidationResult(
            component="OpenTelemetry Integration",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    async def _validate_code_quality(self) -> ValidationResult:
        """Validate code quality metrics"""
        issues = []
        recommendations = []
        metrics = {}
        
        # Check Python code quality
        try:
            # Run ruff
            result = await asyncio.create_subprocess_exec(
                "uv", "run", "ruff", "check", ".",
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await result.communicate()
            
            if result.returncode != 0:
                ruff_issues = len(stdout.decode().split('\n')) - 1
                metrics["ruff_issues"] = ruff_issues
                if ruff_issues > 10:
                    issues.append(f"Too many linting issues: {ruff_issues}")
                    recommendations.append("Run 'uv run ruff check --fix .' to auto-fix")
            
            # Check for type hints
            py_files = list(Path(".").glob("*.py"))
            typed_files = 0
            for py_file in py_files[:10]:  # Sample first 10 files
                content = py_file.read_text()
                if "-> " in content or ": " in content:
                    typed_files += 1
            
            type_coverage = typed_files / min(len(py_files), 10)
            metrics["type_hint_coverage"] = type_coverage
            
            if type_coverage < 0.8:
                issues.append(f"Low type hint coverage: {type_coverage:.0%}")
                recommendations.append("Add type hints for better code maintainability")
            
        except Exception as e:
            issues.append(f"Code quality check error: {e}")
        
        # Check C code quality
        c_files = list(Path(".").glob("**/*.c"))
        if c_files:
            # Check for memory safety patterns
            unsafe_patterns = ["strcpy", "strcat", "gets", "sprintf"]
            unsafe_count = 0
            
            for c_file in c_files[:5]:  # Sample first 5 files
                try:
                    content = c_file.read_text()
                    for pattern in unsafe_patterns:
                        unsafe_count += content.count(pattern)
                except:
                    pass
            
            if unsafe_count > 0:
                issues.append(f"Found {unsafe_count} unsafe C functions")
                recommendations.append("Replace with safe variants (strncpy, strncat, etc)")
        
        # Calculate score
        score = 100.0
        score -= metrics.get("ruff_issues", 0) * 2
        score -= (1 - metrics.get("type_hint_coverage", 0)) * 20
        score -= len(issues) * 10
        score = max(0, score)
        
        return ValidationResult(
            component="Code Quality",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    async def _validate_security(self) -> ValidationResult:
        """Validate security posture"""
        issues = []
        recommendations = []
        metrics = {}
        score = 100.0
        
        # Check for secrets in code
        secret_patterns = [
            r"api[_-]?key",
            r"secret[_-]?key",
            r"password",
            r"token",
            r"private[_-]?key"
        ]
        
        try:
            import re
            py_files = list(Path(".").glob("*.py"))[:20]  # Check first 20 files
            
            for py_file in py_files:
                content = py_file.read_text().lower()
                for pattern in secret_patterns:
                    if re.search(pattern, content):
                        # Check if it's actually hardcoded
                        if "=" in content and pattern in content:
                            lines = content.split('\n')
                            for line in lines:
                                if pattern in line and "=" in line and not "os.environ" in line:
                                    issues.append(f"Potential hardcoded secret in {py_file.name}")
                                    score -= 20
                                    break
        except Exception as e:
            issues.append(f"Security scan error: {e}")
        
        # Check file permissions
        sensitive_files = ["deploy_local.sh", "Makefile.deploy"]
        for file in sensitive_files:
            if Path(file).exists():
                stat = os.stat(file)
                if stat.st_mode & 0o077:  # Check for world/group write
                    issues.append(f"Insecure permissions on {file}")
                    recommendations.append(f"Run: chmod 750 {file}")
                    score -= 10
        
        # Check dependencies
        if Path("pyproject.toml").exists():
            content = Path("pyproject.toml").read_text()
            # Check for version pinning
            if "^" in content or "~" in content:
                recommendations.append("Consider exact version pinning for production")
                score -= 5
        
        metrics["security_score"] = max(0, score)
        
        return ValidationResult(
            component="Security Posture",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    async def _validate_deployment(self) -> ValidationResult:
        """Validate deployment pipeline"""
        issues = []
        recommendations = []
        metrics = {}
        
        # Check deployment scripts
        required_files = [
            "deploy_local.sh",
            "Makefile.deploy",
            ".github/workflows/cns-deploy.yml",
            ".github/workflows/cns-ci.yml"
        ]
        
        missing_files = []
        for file in required_files:
            if not Path(file).exists():
                missing_files.append(file)
        
        if missing_files:
            issues.append(f"Missing deployment files: {', '.join(missing_files)}")
        
        # Test deployment script
        if Path("deploy_local.sh").exists():
            try:
                # Test status command
                result = await asyncio.create_subprocess_exec(
                    "./deploy_local.sh", "local", "status",
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                stdout, stderr = await result.communicate()
                
                if result.returncode == 0:
                    metrics["deployment_script_functional"] = True
                else:
                    issues.append("Deployment script status check failed")
                    
            except Exception as e:
                issues.append(f"Deployment script error: {e}")
        
        # Check for rollback capability
        if Path("deploy_local.sh").exists():
            content = Path("deploy_local.sh").read_text()
            if "rollback" not in content:
                issues.append("No rollback functionality found")
                recommendations.append("Implement automated rollback")
        
        # Calculate score
        score = 100.0
        score -= len(missing_files) * 20
        score -= len(issues) * 15
        score = max(0, score)
        
        return ValidationResult(
            component="Deployment Pipeline",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    async def _validate_monitoring(self) -> ValidationResult:
        """Validate monitoring and observability"""
        issues = []
        recommendations = []
        metrics = {}
        
        # Check monitoring components
        monitoring_files = [
            "cns_monitor.py",
            "cns_status.py",
            "validate_otel.py"
        ]
        
        for file in monitoring_files:
            if not Path(file).exists():
                issues.append(f"Missing monitoring component: {file}")
        
        # Test monitoring functionality
        if Path("cns_status.py").exists():
            try:
                result = await asyncio.create_subprocess_exec(
                    "uv", "run", "python", "cns_status.py", "--format", "json",
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE
                )
                stdout, stderr = await result.communicate()
                
                if result.returncode == 0:
                    try:
                        status_data = json.loads(stdout.decode())
                        metrics["health_score"] = status_data.get("health_score", 0)
                        
                        # Check monitoring coverage
                        expected_keys = ["timestamp", "health_score", "status", "system", "cns", "performance"]
                        missing_keys = [k for k in expected_keys if k not in status_data]
                        
                        if missing_keys:
                            issues.append(f"Incomplete monitoring data: missing {', '.join(missing_keys)}")
                            
                    except json.JSONDecodeError:
                        issues.append("CNS status output is not valid JSON")
                else:
                    issues.append("CNS status command failed")
                    
            except Exception as e:
                issues.append(f"Monitoring validation error: {e}")
        
        # Check for alerting
        alerting_patterns = ["alert", "threshold", "notify", "alarm"]
        has_alerting = False
        
        for file in Path(".").glob("*.py"):
            try:
                content = file.read_text().lower()
                if any(pattern in content for pattern in alerting_patterns):
                    has_alerting = True
                    break
            except:
                pass
        
        if not has_alerting:
            recommendations.append("Implement alerting for critical thresholds")
        
        # Calculate score
        score = 100.0
        score -= len(issues) * 15
        if not has_alerting:
            score -= 10
        score = max(0, score)
        
        return ValidationResult(
            component="Monitoring & Observability",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    async def _validate_neural_integration(self) -> ValidationResult:
        """Validate neural/AI integration"""
        issues = []
        recommendations = []
        metrics = {}
        
        try:
            # Run neural integration tests
            result = await asyncio.create_subprocess_exec(
                "uv", "run", "python", "neural_integration_test.py",
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            stdout, stderr = await result.communicate()
            output = stdout.decode()
            
            # Parse results
            if "Success Rate:" in output:
                rate_line = [l for l in output.split('\n') if 'Success Rate:' in l][0]
                success_rate = float(rate_line.split(':')[1].split('%')[0].strip())
                metrics["neural_success_rate"] = success_rate
                
                if success_rate < 100:
                    issues.append(f"Neural tests not fully passing: {success_rate}%")
            
            # Check throughput
            if "throughput_ips" in output:
                throughput_line = [l for l in output.split('\n') if 'throughput_ips' in l][0]
                throughput = float(throughput_line.split(':')[1].strip())
                metrics["neural_throughput_ips"] = throughput
                
                if throughput < 50:
                    issues.append(f"Neural inference too slow: {throughput:.1f} ips")
                    recommendations.append("Consider model optimization or quantization")
            
        except Exception as e:
            issues.append(f"Neural validation error: {e}")
        
        # Check DSPy integration
        if Path("generated_signatures.py").exists():
            content = Path("generated_signatures.py").read_text()
            signature_count = content.count("class") - 1  # Exclude imports
            metrics["dspy_signatures"] = signature_count
            
            if signature_count < 3:
                issues.append(f"Too few DSPy signatures: {signature_count}")
                recommendations.append("Generate more signatures from ontologies")
        
        # Calculate score
        score = metrics.get("neural_success_rate", 50)
        score -= len(issues) * 10
        score = max(0, score)
        
        return ValidationResult(
            component="Neural Integration",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    async def _validate_error_handling(self) -> ValidationResult:
        """Validate error handling and resilience"""
        issues = []
        recommendations = []
        metrics = {}
        
        # Analyze error handling patterns
        py_files = list(Path(".").glob("*.py"))[:20]
        
        total_functions = 0
        functions_with_try = 0
        bare_excepts = 0
        
        for py_file in py_files:
            try:
                content = py_file.read_text()
                lines = content.split('\n')
                
                for i, line in enumerate(lines):
                    if line.strip().startswith("def ") or line.strip().startswith("async def "):
                        total_functions += 1
                        
                        # Check if function has try/except
                        func_end = i
                        for j in range(i + 1, min(i + 50, len(lines))):
                            if lines[j].strip() and not lines[j].startswith(" ") and not lines[j].startswith("\t"):
                                func_end = j
                                break
                        
                        func_content = '\n'.join(lines[i:func_end])
                        if "try:" in func_content:
                            functions_with_try += 1
                        
                    if "except:" in line and "Exception" not in line:
                        bare_excepts += 1
                        
            except Exception:
                pass
        
        if total_functions > 0:
            error_handling_coverage = functions_with_try / total_functions
            metrics["error_handling_coverage"] = error_handling_coverage
            
            if error_handling_coverage < 0.5:
                issues.append(f"Low error handling coverage: {error_handling_coverage:.0%}")
                recommendations.append("Add try/except blocks to critical functions")
        
        if bare_excepts > 0:
            issues.append(f"Found {bare_excepts} bare except clauses")
            recommendations.append("Use specific exception types")
        
        # Check for logging
        has_logging = False
        for py_file in py_files[:10]:
            try:
                if "logging" in py_file.read_text() or "logger" in py_file.read_text():
                    has_logging = True
                    break
            except:
                pass
        
        if not has_logging:
            issues.append("No logging framework detected")
            recommendations.append("Implement structured logging")
        
        # Calculate score
        score = 100.0
        score -= (1 - metrics.get("error_handling_coverage", 0.5)) * 40
        score -= bare_excepts * 5
        score -= len(issues) * 10
        score = max(0, score)
        
        return ValidationResult(
            component="Error Handling",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    async def _validate_documentation(self) -> ValidationResult:
        """Validate documentation completeness"""
        issues = []
        recommendations = []
        metrics = {}
        
        # Check key documentation files
        doc_files = {
            "README.md": "Project overview",
            "deploy/README.md": "Deployment guide",
            "docs/": "Documentation directory"
        }
        
        missing_docs = []
        for file, desc in doc_files.items():
            if not Path(file).exists():
                missing_docs.append(f"{file} ({desc})")
        
        if missing_docs:
            issues.append(f"Missing documentation: {', '.join(missing_docs)}")
        
        # Check docstring coverage
        py_files = list(Path(".").glob("*.py"))[:10]
        total_classes = 0
        documented_classes = 0
        total_functions = 0
        documented_functions = 0
        
        for py_file in py_files:
            try:
                content = py_file.read_text()
                lines = content.split('\n')
                
                i = 0
                while i < len(lines):
                    line = lines[i].strip()
                    
                    if line.startswith("class "):
                        total_classes += 1
                        # Check for docstring
                        if i + 1 < len(lines) and '"""' in lines[i + 1]:
                            documented_classes += 1
                    
                    elif line.startswith("def ") or line.startswith("async def "):
                        total_functions += 1
                        # Check for docstring
                        if i + 1 < len(lines) and '"""' in lines[i + 1]:
                            documented_functions += 1
                    
                    i += 1
                    
            except Exception:
                pass
        
        if total_classes > 0:
            class_doc_coverage = documented_classes / total_classes
            metrics["class_documentation"] = class_doc_coverage
            if class_doc_coverage < 0.8:
                issues.append(f"Low class documentation: {class_doc_coverage:.0%}")
        
        if total_functions > 0:
            func_doc_coverage = documented_functions / total_functions
            metrics["function_documentation"] = func_doc_coverage
            if func_doc_coverage < 0.6:
                issues.append(f"Low function documentation: {func_doc_coverage:.0%}")
        
        # Calculate score
        score = 100.0
        score -= len(missing_docs) * 15
        score -= (1 - metrics.get("class_documentation", 0.8)) * 20
        score -= (1 - metrics.get("function_documentation", 0.6)) * 20
        score = max(0, score)
        
        return ValidationResult(
            component="Documentation",
            status="PASS" if score >= 80 else "WARN" if score >= 60 else "FAIL",
            score=score,
            metrics=metrics,
            issues=issues,
            recommendations=recommendations
        )
    
    def _generate_validation_report(self) -> Dict[str, Any]:
        """Generate comprehensive validation report"""
        # Calculate overall metrics
        total_validations = len(self.results)
        passed = len([r for r in self.results if r.status == "PASS"])
        warnings = len([r for r in self.results if r.status == "WARN"])
        failed = len([r for r in self.results if r.status == "FAIL"])
        
        # Calculate overall score
        overall_score = sum(r.score for r in self.results) / total_validations if total_validations > 0 else 0
        
        # Update pipeline health gauge
        self.pipeline_health_gauge.set(overall_score)
        
        # Print summary
        print("\n" + "=" * 80)
        print("🏁 CNS PIPELINE VALIDATION REPORT")
        print("=" * 80)
        print(f"Duration: {time.time() - self.start_time:.1f}s")
        print(f"Total Validations: {total_validations}")
        print(f"Passed: {passed} ({passed/total_validations*100:.1f}%)")
        print(f"Warnings: {warnings} ({warnings/total_validations*100:.1f}%)")
        print(f"Failed: {failed} ({failed/total_validations*100:.1f}%)")
        print(f"Overall Score: {overall_score:.1f}/100")
        print()
        
        # Component results
        print("Component Results:")
        print("-" * 80)
        for result in self.results:
            status_icon = {"PASS": "✅", "FAIL": "❌", "WARN": "⚠️ "}.get(result.status, "❓")
            print(f"{status_icon} {result.component:30} {result.score:5.1f}/100")
            
            # Show critical issues
            if result.status == "FAIL" and result.issues:
                for issue in result.issues[:2]:
                    print(f"   └─ {issue}")
        
        # Best practices compliance
        print("\n8T-8H-8M Compliance:")
        print("-" * 80)
        
        # Find performance metrics
        perf_result = next((r for r in self.results if r.component == "Performance Benchmarks"), None)
        if perf_result and perf_result.metrics:
            latency = perf_result.metrics.get("latency_cycles", "N/A")
            throughput = perf_result.metrics.get("throughput_ops_sec", "N/A")
            
            eight_tick = "✅ PASS" if isinstance(latency, (int, float)) and latency <= 8 else "❌ FAIL"
            eight_hour = "✅ PASS" if overall_score >= 80 else "❌ FAIL"
            eight_mb = "✅ PASS" if isinstance(throughput, (int, float)) and throughput >= 1000000 else "❌ FAIL"
            
            print(f"8-Tick (≤8 CPU cycles):    {eight_tick} ({latency} cycles)")
            print(f"8-Hour (Reliability):      {eight_hour} ({overall_score:.1f}/100 score)")
            print(f"8-MB/s (Throughput):       {eight_mb} ({throughput:.0f} ops/sec)")
        
        # Recommendations
        all_recommendations = []
        for result in self.results:
            all_recommendations.extend(result.recommendations)
        
        if all_recommendations:
            print("\n📋 Top Recommendations:")
            print("-" * 80)
            for i, rec in enumerate(all_recommendations[:5]):
                print(f"{i+1}. {rec}")
        
        # Mermaid diagram
        print("\n```mermaid")
        print("graph TD")
        print("    A[Pipeline Validation] --> B[Build System]")
        print("    A --> C[Performance]")
        print("    A --> D[OTEL Integration]")
        print("    A --> E[Code Quality]")
        print("    A --> F[Security]")
        print("    A --> G[Deployment]")
        print("    A --> H[Monitoring]")
        print("    A --> I[Neural Integration]")
        print("    A --> J[Error Handling]")
        print("    A --> K[Documentation]")
        
        for result in self.results:
            node = result.component[0]
            if result.status == "PASS":
                print(f"    class {node} pass")
            elif result.status == "WARN":
                print(f"    class {node} warn")
            else:
                print(f"    class {node} fail")
        
        print("    classDef pass fill:#90EE90")
        print("    classDef warn fill:#FFD700")
        print("    classDef fail fill:#FFB6C1")
        print("```")
        
        # Status chart
        print("\n```mermaid")
        print("pie title Validation Results")
        print(f'    "Passed" : {passed}')
        print(f'    "Warnings" : {warnings}')
        print(f'    "Failed" : {failed}')
        print("```")
        
        # Final verdict
        print("\n" + "=" * 80)
        if overall_score >= 90:
            print("🎉 EXCELLENT - Pipeline is production-ready!")
            verdict = "PRODUCTION_READY"
        elif overall_score >= 80:
            print("✅ GOOD - Pipeline meets minimum requirements")
            verdict = "ACCEPTABLE"
        elif overall_score >= 70:
            print("⚠️  WARNING - Pipeline needs improvements")
            verdict = "NEEDS_IMPROVEMENT"
        else:
            print("❌ CRITICAL - Pipeline has serious issues")
            verdict = "CRITICAL"
        
        print(f"Overall Health Score: {overall_score:.1f}/100")
        print("=" * 80)
        
        return {
            "summary": {
                "total_validations": total_validations,
                "passed": passed,
                "warnings": warnings,
                "failed": failed,
                "overall_score": overall_score,
                "verdict": verdict,
                "duration_seconds": time.time() - self.start_time,
                "timestamp": datetime.now().isoformat()
            },
            "results": [
                {
                    "component": r.component,
                    "status": r.status,
                    "score": r.score,
                    "metrics": r.metrics,
                    "issues": r.issues,
                    "recommendations": r.recommendations,
                    "timestamp": r.timestamp
                }
                for r in self.results
            ],
            "compliance": {
                "eight_tick": perf_result.metrics.get("latency_cycles", 999) <= 8 if perf_result else False,
                "eight_hour": overall_score >= 80,
                "eight_mb": perf_result.metrics.get("throughput_ops_sec", 0) >= 1000000 if perf_result else False
            },
            "recommendations": all_recommendations[:10]
        }


async def main():
    """Run pipeline validation"""
    validator = CNSPipelineValidator()
    
    try:
        report = await validator.validate_pipeline()
        
        # Save report
        report_file = f"pipeline-validation-{datetime.now().strftime('%Y%m%d-%H%M%S')}.json"
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"\n📄 Full report saved to: {report_file}")
        
    except KeyboardInterrupt:
        print("\n👋 Validation stopped by user")
    except Exception as e:
        print(f"\n💥 Validation failed with error: {e}")
        raise


if __name__ == "__main__":
    asyncio.run(main())