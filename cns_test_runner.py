#!/usr/bin/env python3
"""
CNS Test Runner - Comprehensive test orchestration CLI
Runs all self-checks, unit tests, BDD tests, benchmarks, and stress tests
Reports REAL validated results with production-grade metrics
"""

import typer
import subprocess
import time
import json
import os
import sys
from pathlib import Path
from typing import List, Dict, Any, Optional
from dataclasses import dataclass, asdict
from datetime import datetime, timedelta
from enum import Enum
import re
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading

app = typer.Typer(
    name="cns-test",
    help="🚀 CNS Test Runner - Comprehensive validation and benchmarking suite",
    no_args_is_help=True
)

class TestStatus(str, Enum):
    PENDING = "pending"
    RUNNING = "running"
    PASSED = "passed"
    FAILED = "failed"
    SKIPPED = "skipped"

@dataclass
class TestResult:
    name: str
    status: TestStatus
    duration_ms: float
    output: str
    error: Optional[str] = None
    metrics: Dict[str, Any] = None
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None

@dataclass
class TestSuite:
    name: str
    description: str
    tests: List[TestResult]
    total_duration_ms: float
    passed_count: int
    failed_count: int
    skipped_count: int

class TestRunner:
    def __init__(self, verbose: bool = False, parallel: bool = True):
        self.verbose = verbose
        self.parallel = parallel
        self.results: Dict[str, TestSuite] = {}
        self.start_time = datetime.now()
        self.test_dir = Path("/Users/sac/cns/tests")
        self.root_dir = Path("/Users/sac/cns")
        
        # Ensure we're in the right directory
        if not self.test_dir.exists():
            typer.echo(f"❌ Test directory not found: {self.test_dir}", err=True)
            raise typer.Exit(1)
    
    def _run_command(self, cmd: List[str], cwd: Optional[Path] = None, timeout: int = 300) -> TestResult:
        """Run a command and capture results with real metrics"""
        start_time = datetime.now()
        
        if cwd is None:
            cwd = self.test_dir
            
        cmd_str = " ".join(cmd)
        test_name = cmd[-1] if cmd else "unknown"
        
        if self.verbose:
            typer.echo(f"🔍 Running: {cmd_str}")
        
        try:
            result = subprocess.run(
                cmd,
                cwd=cwd,
                capture_output=True,
                text=True,
                timeout=timeout
            )
            
            end_time = datetime.now()
            duration_ms = (end_time - start_time).total_seconds() * 1000
            
            # Parse output for metrics
            metrics = self._parse_metrics(result.stdout)
            
            if result.returncode == 0:
                return TestResult(
                    name=test_name,
                    status=TestStatus.PASSED,
                    duration_ms=duration_ms,
                    output=result.stdout,
                    metrics=metrics,
                    start_time=start_time,
                    end_time=end_time
                )
            else:
                return TestResult(
                    name=test_name,
                    status=TestStatus.FAILED,
                    duration_ms=duration_ms,
                    output=result.stdout,
                    error=result.stderr,
                    start_time=start_time,
                    end_time=end_time
                )
                
        except subprocess.TimeoutExpired:
            end_time = datetime.now()
            duration_ms = (end_time - start_time).total_seconds() * 1000
            return TestResult(
                name=test_name,
                status=TestStatus.FAILED,
                duration_ms=duration_ms,
                output="",
                error=f"Test timed out after {timeout}s",
                start_time=start_time,
                end_time=end_time
            )
        except Exception as e:
            end_time = datetime.now()
            duration_ms = (end_time - start_time).total_seconds() * 1000
            return TestResult(
                name=test_name,
                status=TestStatus.FAILED,
                duration_ms=duration_ms,
                output="",
                error=str(e),
                start_time=start_time,
                end_time=end_time
            )
    
    def _parse_metrics(self, output: str) -> Dict[str, Any]:
        """Parse real metrics from test output"""
        metrics = {}
        
        # Parse performance metrics
        cycle_pattern = r"cycles?:\s*(\d+)"
        matches = re.findall(cycle_pattern, output, re.IGNORECASE)
        if matches:
            metrics["execution_cycles"] = [int(m) for m in matches]
        
        # Parse signal processing metrics
        signal_pattern = r"signals?\s+(?:processed|injected|optimized):\s*(\d+)"
        matches = re.findall(signal_pattern, output, re.IGNORECASE)
        if matches:
            metrics["signals_processed"] = [int(m) for m in matches]
        
        # Parse zero-tick optimization metrics
        zero_tick_pattern = r"Zero-tick\s+optimized:\s*(\d+)"
        matches = re.findall(zero_tick_pattern, output, re.IGNORECASE)
        if matches:
            metrics["zero_tick_optimized"] = int(matches[0])
        
        # Parse optimization ratios
        ratio_pattern = r"Optimization\s+ratio:\s*([\d.]+)%"
        matches = re.findall(ratio_pattern, output, re.IGNORECASE)
        if matches:
            metrics["optimization_ratio"] = float(matches[0])
        
        # Parse test counts
        passed_pattern = r"(\d+)\s+passed"
        failed_pattern = r"(\d+)\s+failed"
        
        passed_matches = re.findall(passed_pattern, output, re.IGNORECASE)
        failed_matches = re.findall(failed_pattern, output, re.IGNORECASE)
        
        if passed_matches:
            metrics["tests_passed"] = int(passed_matches[-1])
        if failed_matches:
            metrics["tests_failed"] = int(failed_matches[-1])
        
        # Parse memory usage
        memory_pattern = r"Memory.*?(\d+)\s*MB"
        matches = re.findall(memory_pattern, output, re.IGNORECASE)
        if matches:
            metrics["memory_usage_mb"] = [int(m) for m in matches]
        
        # Check for system health status
        if "ALL SYSTEMS OPERATIONAL" in output:
            metrics["system_health"] = "operational"
        elif "SYSTEM DEGRADATION" in output:
            metrics["system_health"] = "degraded"
        
        return metrics
    
    def _compile_tests(self) -> TestResult:
        """Compile all tests first"""
        typer.echo("🔧 Compiling test suite...")
        return self._run_command(["make", "clean", "all"], timeout=120)
    
    def run_self_checks(self) -> TestSuite:
        """Run comprehensive self-check validations"""
        typer.echo("🏥 Running CNS System Self-Checks...")
        
        tests = []
        
        # Compile tests first
        compile_result = self._compile_tests()
        tests.append(compile_result)
        
        if compile_result.status == TestStatus.PASSED:
            # Run comprehensive system integration test
            integration_test = self._run_command(["./test_cns_system_integration_complete"])
            tests.append(integration_test)
            
            # Run chaos engineering tests
            chaos_test = self._run_command(["./test_bitactor_chaos_bdd"])
            tests.append(chaos_test)
        else:
            typer.echo("❌ Compilation failed, skipping executable tests")
        
        return self._create_test_suite("Self-Checks", "System health and integration validation", tests)
    
    def run_unit_tests(self) -> TestSuite:
        """Run unit tests"""
        typer.echo("🧪 Running Unit Tests...")
        
        tests = []
        
        # Find and run individual unit tests
        unit_tests = [
            "test_bitactor_core_real_bdd",
            "test_news_validator_real_bdd", 
            "test_sparql_real_bdd"
        ]
        
        for test_name in unit_tests:
            test_path = self.test_dir / test_name
            if test_path.exists():
                result = self._run_command([f"./{test_name}"])
                tests.append(result)
            else:
                # Try to compile and run
                compile_result = self._run_command(["make", test_name])
                if compile_result.status == TestStatus.PASSED:
                    result = self._run_command([f"./{test_name}"])
                    tests.append(result)
                else:
                    tests.append(TestResult(
                        name=test_name,
                        status=TestStatus.SKIPPED,
                        duration_ms=0,
                        output="Test not found and compilation failed"
                    ))
        
        return self._create_test_suite("Unit Tests", "Individual component validation", tests)
    
    def run_bdd_tests(self) -> TestSuite:
        """Run BDD (Behavior-Driven Development) tests"""
        typer.echo("📋 Running BDD Test Suite...")
        
        tests = []
        
        # Run all BDD tests via make target
        bdd_result = self._run_command(["make", "test-real"], timeout=600)
        tests.append(bdd_result)
        
        return self._create_test_suite("BDD Tests", "Behavior-driven development validation", tests)
    
    def run_benchmarks(self) -> TestSuite:
        """Run performance benchmarks"""
        typer.echo("⚡ Running Performance Benchmarks...")
        
        tests = []
        
        # Run performance comparison tests
        perf_result = self._run_command(["make", "test-performance"])
        tests.append(perf_result)
        
        # Run coverage analysis
        coverage_result = self._run_command(["make", "test-coverage"], timeout=300)
        tests.append(coverage_result)
        
        return self._create_test_suite("Benchmarks", "Performance and coverage analysis", tests)
    
    def run_stress_tests(self) -> TestSuite:
        """Run stress and load tests"""
        typer.echo("🔥 Running Stress Tests...")
        
        tests = []
        
        # First check if system integration test exists and is compiled
        integration_test = self.test_dir / "test_cns_system_integration_complete"
        if integration_test.exists():
            result = self._run_command(["./test_cns_system_integration_complete"], timeout=600)
            tests.append(result)
        else:
            # Try to compile it
            compile_result = self._run_command(["make", "test_cns_system_integration_complete"])
            if compile_result.status == TestStatus.PASSED:
                result = self._run_command(["./test_cns_system_integration_complete"], timeout=600)
                tests.append(result)
            else:
                tests.append(TestResult(
                    name="test_cns_system_integration_complete",
                    status=TestStatus.FAILED,
                    duration_ms=0,
                    output="Compilation failed",
                    error=compile_result.error
                ))
        
        # Run chaos engineering stress tests if available
        stress_tests = [
            "test_bitactor_chaos_bdd",
            "test_bitactor_core_real_bdd"
        ]
        
        for test_name in stress_tests:
            test_path = self.test_dir / test_name
            if test_path.exists():
                result = self._run_command([f"./{test_name}"], timeout=600)
                tests.append(result)
            else:
                compile_result = self._run_command(["make", test_name])
                if compile_result.status == TestStatus.PASSED:
                    result = self._run_command([f"./{test_name}"], timeout=600)
                    tests.append(result)
                else:
                    tests.append(TestResult(
                        name=test_name,
                        status=TestStatus.SKIPPED,
                        duration_ms=0,
                        output=f"Test {test_name} not available - compilation failed"
                    ))
        
        return self._create_test_suite("Stress Tests", "High-load and chaos engineering validation", tests)
    
    def _create_test_suite(self, name: str, description: str, tests: List[TestResult]) -> TestSuite:
        """Create a test suite from results"""
        total_duration = sum(test.duration_ms for test in tests)
        passed_count = sum(1 for test in tests if test.status == TestStatus.PASSED)
        failed_count = sum(1 for test in tests if test.status == TestStatus.FAILED)
        skipped_count = sum(1 for test in tests if test.status == TestStatus.SKIPPED)
        
        suite = TestSuite(
            name=name,
            description=description,
            tests=tests,
            total_duration_ms=total_duration,
            passed_count=passed_count,
            failed_count=failed_count,
            skipped_count=skipped_count
        )
        
        self.results[name] = suite
        return suite
    
    def generate_report(self, output_file: Optional[str] = None) -> None:
        """Generate comprehensive test report"""
        end_time = datetime.now()
        total_duration = (end_time - self.start_time).total_seconds()
        
        # Calculate overall statistics
        total_tests = sum(suite.passed_count + suite.failed_count + suite.skipped_count for suite in self.results.values())
        total_passed = sum(suite.passed_count for suite in self.results.values())
        total_failed = sum(suite.failed_count for suite in self.results.values())
        total_skipped = sum(suite.skipped_count for suite in self.results.values())
        
        # Calculate success rate
        success_rate = (total_passed / total_tests * 100) if total_tests > 0 else 0
        
        # Generate report
        report = []
        report.append("=" * 80)
        report.append("🚀 CNS COMPREHENSIVE TEST REPORT")
        report.append("=" * 80)
        report.append(f"Generated: {end_time.strftime('%Y-%m-%d %H:%M:%S')}")
        report.append(f"Total Execution Time: {total_duration:.2f}s")
        report.append("")
        
        # Overall statistics
        report.append("📊 OVERALL STATISTICS:")
        report.append(f"   Total Tests: {total_tests}")
        report.append(f"   ✅ Passed: {total_passed}")
        report.append(f"   ❌ Failed: {total_failed}")
        report.append(f"   ⏭️  Skipped: {total_skipped}")
        report.append(f"   🎯 Success Rate: {success_rate:.1f}%")
        report.append("")
        
        # Test suite details
        for suite_name, suite in self.results.items():
            report.append(f"🧪 {suite.name.upper()}: {suite.description}")
            report.append("-" * 60)
            report.append(f"   Duration: {suite.total_duration_ms/1000:.2f}s")
            report.append(f"   Tests: {suite.passed_count + suite.failed_count + suite.skipped_count}")
            report.append(f"   Status: ✅ {suite.passed_count} passed, ❌ {suite.failed_count} failed, ⏭️ {suite.skipped_count} skipped")
            
            # Individual test results
            for test in suite.tests:
                status_icon = "✅" if test.status == TestStatus.PASSED else "❌" if test.status == TestStatus.FAILED else "⏭️"
                report.append(f"   {status_icon} {test.name}: {test.duration_ms/1000:.2f}s")
                
                # Add metrics if available
                if test.metrics:
                    if "execution_cycles" in test.metrics:
                        cycles = test.metrics["execution_cycles"]
                        if isinstance(cycles, list) and cycles:
                            report.append(f"      🔄 Execution cycles: {cycles[0]}")
                    
                    if "zero_tick_optimized" in test.metrics:
                        report.append(f"      ⚡ Zero-tick optimized: {test.metrics['zero_tick_optimized']}")
                    
                    if "optimization_ratio" in test.metrics:
                        report.append(f"      📈 Optimization ratio: {test.metrics['optimization_ratio']}%")
                    
                    if "system_health" in test.metrics:
                        health_icon = "🟢" if test.metrics["system_health"] == "operational" else "🟡"
                        report.append(f"      {health_icon} System health: {test.metrics['system_health']}")
                
                # Show errors for failed tests
                if test.status == TestStatus.FAILED and test.error:
                    error_lines = test.error.split('\n')[:3]  # Show first 3 lines
                    for line in error_lines:
                        if line.strip():
                            report.append(f"      🔴 {line.strip()}")
            
            report.append("")
        
        # Final status
        report.append("🎯 FINAL STATUS:")
        if total_failed == 0:
            report.append("🟢 ALL TESTS PASSED - SYSTEM READY FOR PRODUCTION")
        elif success_rate >= 80:
            report.append("🟡 MOSTLY PASSING - REVIEW FAILED TESTS BEFORE DEPLOYMENT")
        else:
            report.append("🔴 CRITICAL FAILURES - SYSTEM NOT READY FOR PRODUCTION")
        
        report.append("=" * 80)
        
        # Output report
        report_text = "\n".join(report)
        
        if output_file:
            with open(output_file, 'w') as f:
                f.write(report_text)
            typer.echo(f"📄 Report saved to: {output_file}")
        
        typer.echo(report_text)

# CLI Commands

@app.command()
def self_check(
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose output"),
    output: Optional[str] = typer.Option(None, "--output", "-o", help="Save report to file")
):
    """🏥 Run comprehensive system self-checks and integration validation"""
    runner = TestRunner(verbose=verbose)
    runner.run_self_checks()
    runner.generate_report(output)

@app.command()
def unit(
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose output"),
    output: Optional[str] = typer.Option(None, "--output", "-o", help="Save report to file")
):
    """🧪 Run unit tests for individual components"""
    runner = TestRunner(verbose=verbose)
    runner.run_unit_tests()
    runner.generate_report(output)

@app.command()
def bdd(
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose output"),
    output: Optional[str] = typer.Option(None, "--output", "-o", help="Save report to file")
):
    """📋 Run BDD (Behavior-Driven Development) test suite"""
    runner = TestRunner(verbose=verbose)
    runner.run_bdd_tests()
    runner.generate_report(output)

@app.command()
def benchmark(
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose output"),
    output: Optional[str] = typer.Option(None, "--output", "-o", help="Save report to file")
):
    """⚡ Run performance benchmarks and coverage analysis"""
    runner = TestRunner(verbose=verbose)
    runner.run_benchmarks()
    runner.generate_report(output)

@app.command()
def stress(
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose output"),
    output: Optional[str] = typer.Option(None, "--output", "-o", help="Save report to file")
):
    """🔥 Run stress tests and chaos engineering validation"""
    runner = TestRunner(verbose=verbose)
    runner.run_stress_tests()
    runner.generate_report(output)

@app.command()
def all(
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose output"),
    output: Optional[str] = typer.Option(None, "--output", "-o", help="Save report to file"),
    parallel: bool = typer.Option(True, "--parallel/--sequential", help="Run test suites in parallel")
):
    """🚀 Run ALL tests: self-checks, unit tests, BDD, benchmarks, and stress tests"""
    runner = TestRunner(verbose=verbose, parallel=parallel)
    
    typer.echo("🌟 Running COMPREHENSIVE CNS Test Suite")
    typer.echo("=" * 50)
    
    if parallel:
        typer.echo("🔄 Running test suites in parallel...")
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = {
                executor.submit(runner.run_self_checks): "self-checks",
                executor.submit(runner.run_unit_tests): "unit-tests", 
                executor.submit(runner.run_bdd_tests): "bdd-tests",
                executor.submit(runner.run_benchmarks): "benchmarks",
                executor.submit(runner.run_stress_tests): "stress-tests"
            }
            
            for future in as_completed(futures):
                suite_name = futures[future]
                try:
                    suite = future.result()
                    status = "✅" if suite.failed_count == 0 else "❌"
                    typer.echo(f"{status} {suite_name} completed in {suite.total_duration_ms/1000:.2f}s")
                except Exception as e:
                    typer.echo(f"❌ {suite_name} failed: {e}")
    else:
        typer.echo("📋 Running test suites sequentially...")
        runner.run_self_checks()
        runner.run_unit_tests()
        runner.run_bdd_tests()
        runner.run_benchmarks()
        runner.run_stress_tests()
    
    runner.generate_report(output)

@app.command()
def status():
    """📊 Show current system and test environment status"""
    typer.echo("📊 CNS Test Environment Status")
    typer.echo("=" * 40)
    
    test_dir = Path("/Users/sac/cns/tests")
    root_dir = Path("/Users/sac/cns")
    
    # Check directories
    typer.echo(f"📁 Root directory: {root_dir}")
    typer.echo(f"   {'✅' if root_dir.exists() else '❌'} Exists")
    
    typer.echo(f"📁 Test directory: {test_dir}")
    typer.echo(f"   {'✅' if test_dir.exists() else '❌'} Exists")
    
    if test_dir.exists():
        # Check for Makefile
        makefile = test_dir / "Makefile"
        typer.echo(f"🔧 Makefile: {'✅' if makefile.exists() else '❌'}")
        
        # Check for test executables
        test_files = list(test_dir.glob("test_*"))
        c_files = [f for f in test_files if f.suffix == '.c']
        executables = [f for f in test_files if f.is_file() and f.suffix == '']
        
        typer.echo(f"📝 Test source files: {len(c_files)}")
        typer.echo(f"⚙️  Compiled tests: {len(executables)}")
        
        # List available tests
        if executables:
            typer.echo("\n🧪 Available test executables:")
            for exe in sorted(executables):
                typer.echo(f"   • {exe.name}")

if __name__ == "__main__":
    app()