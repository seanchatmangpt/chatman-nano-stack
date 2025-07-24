#!/usr/bin/env python3
"""
Comprehensive unit tests for pipeline_validator.py - 80% line coverage
Tests pipeline validation, best practices, scoring
"""

import pytest
import asyncio
import json
import tempfile
import shutil
from pathlib import Path
from unittest.mock import Mock, patch, AsyncMock, MagicMock
from dataclasses import dataclass

from pipeline_validator import (
    ValidationResult, CNSPipelineValidator
)


class TestValidationResult:
    """Test ValidationResult dataclass"""
    
    def test_validation_result_creation(self):
        """Test ValidationResult instance creation"""
        result = ValidationResult(
            component="Test Component",
            status="PASS",
            score=85.0,
            metrics={"test_metric": 42},
            issues=["Minor issue"],
            recommendations=["Improve something"]
        )
        
        assert result.component == "Test Component"
        assert result.status == "PASS"
        assert result.score == 85.0
        assert result.metrics["test_metric"] == 42
        assert len(result.issues) == 1
        assert len(result.recommendations) == 1
        assert result.timestamp is not None  # Auto-generated
    
    def test_validation_result_defaults(self):
        """Test ValidationResult with default values"""
        result = ValidationResult(
            component="Test",
            status="FAIL",
            score=0.0
        )
        
        assert result.metrics == {}
        assert result.issues == []
        assert result.recommendations == []
        assert result.timestamp is not None


class TestCNSPipelineValidator:
    """Test CNSPipelineValidator class functionality"""
    
    def setup_method(self):
        """Setup for each test method"""
        with patch('pipeline_validator.PeriodicExportingMetricReader'):
            with patch('pipeline_validator.MeterProvider'):
                with patch('pipeline_validator.TracerProvider'):
                    with patch('pipeline_validator.BatchSpanProcessor'):
                        with patch('pipeline_validator.metrics.set_meter_provider'):
                            with patch('pipeline_validator.trace.set_tracer_provider'):
                                self.validator = CNSPipelineValidator()
    
    def test_pipeline_validator_initialization(self):
        """Test pipeline validator initialization"""
        assert hasattr(self.validator, 'results')
        assert isinstance(self.validator.results, list)
        assert len(self.validator.results) == 0
        assert hasattr(self.validator, 'start_time')
        assert hasattr(self.validator, 'best_practices')
        assert hasattr(self.validator, 'meter')
        assert hasattr(self.validator, 'tracer')
    
    def test_load_best_practices(self):
        """Test best practices configuration loading"""
        practices = self.validator._load_best_practices()
        
        assert 'performance' in practices
        assert 'reliability' in practices
        assert 'code_quality' in practices
        assert 'security' in practices
        assert 'deployment' in practices
        
        # Check specific values
        assert practices['performance']['latency_threshold_ms'] == 8
        assert practices['performance']['throughput_min_ops'] == 1000000
        assert practices['reliability']['error_rate_max'] == 0.0001
        assert practices['code_quality']['test_coverage_min'] == 0.80
    
    def test_setup_telemetry(self):
        """Test OpenTelemetry setup"""
        # The setup should create meters, tracers, and instruments
        assert self.validator.meter is not None
        assert self.validator.tracer is not None
        assert hasattr(self.validator, 'validation_histogram')
        assert hasattr(self.validator, 'component_score_gauge')
        assert hasattr(self.validator, 'best_practice_violations')
        assert hasattr(self.validator, 'pipeline_health_gauge')
    
    @pytest.mark.asyncio
    async def test_run_validation_success(self):
        """Test successful validation execution"""
        async def mock_validator_func():
            return ValidationResult(
                component="Test Component",
                status="PASS",
                score=90.0,
                metrics={"test_metric": 42},
                issues=[],
                recommendations=[]
            )
        
        with patch.object(self.validator.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            await self.validator._run_validation("Test Validation", mock_validator_func)
            
            assert len(self.validator.results) == 1
            result = self.validator.results[0]
            assert result.component == "Test Component"
            assert result.status == "PASS"
            assert result.score == 90.0
    
    @pytest.mark.asyncio
    async def test_run_validation_with_issues(self):
        """Test validation execution with issues"""
        async def mock_validator_func():
            return ValidationResult(
                component="Test Component",
                status="WARN",
                score=70.0,
                metrics={},
                issues=["Issue 1", "Issue 2", "Issue 3", "Issue 4"],  # More than 3 issues
                recommendations=["Fix something"]
            )
        
        with patch.object(self.validator.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            with patch('builtins.print') as mock_print:
                await self.validator._run_validation("Test Validation", mock_validator_func)
                
                # Should display first 3 issues + summary for more
                print_calls = [str(call) for call in mock_print.call_args_list]
                issue_output = any("Issue 1" in call for call in print_calls)
                more_issues = any("and 1 more issues" in call for call in print_calls)
                assert issue_output
                assert more_issues
    
    @pytest.mark.asyncio
    async def test_run_validation_exception(self):
        """Test validation execution with exception"""
        async def mock_validator_func():
            raise RuntimeError("Validation failed")
        
        with patch.object(self.validator.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            with patch('builtins.print'):
                await self.validator._run_validation("Test Validation", mock_validator_func)
                
                assert len(self.validator.results) == 1
                result = self.validator.results[0]
                assert result.component == "Test Validation"
                assert result.status == "FAIL"
                assert result.score == 0.0
                assert "Validation error: Validation failed" in result.issues
    
    @pytest.mark.asyncio
    async def test_validate_build_system_success(self):
        """Test build system validation success"""
        # Mock successful tool checks
        with patch('pipeline_validator.subprocess.run') as mock_subprocess:
            mock_subprocess.return_value.returncode = 0  # All tools found
            
            # Mock successful compilation
            mock_compile_process = Mock()
            mock_compile_process.communicate = AsyncMock(return_value=(b"Compilation successful", b""))
            mock_compile_process.returncode = 0
            
            with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_compile_process):
                # Mock Makefile check
                with patch('pipeline_validator.Path') as mock_path:
                    mock_makefile = Mock()
                    mock_makefile.exists.return_value = True
                    mock_makefile.read_text.return_value = "CFLAGS = -O3 -march=native"
                    
                    mock_test_build = Mock()
                    mock_test_build.__truediv__ = Mock(return_value=mock_makefile)
                    mock_path.return_value = mock_test_build
                    
                    # Mock cleanup
                    with patch('pipeline_validator.shutil.rmtree'):
                        result = await self.validator._validate_build_system()
                        
                        assert result.status == "PASS"
                        assert result.score >= 80
                        assert result.metrics["compilation_success"] is True
                        assert len(result.issues) == 0
    
    @pytest.mark.asyncio
    async def test_validate_build_system_missing_tools(self):
        """Test build system validation with missing tools"""
        # Mock missing tools
        def mock_subprocess_run(cmd, **kwargs):
            mock_result = Mock()
            if cmd[1] == "gcc":  # gcc missing
                mock_result.returncode = 1
            else:
                mock_result.returncode = 0
            return mock_result
        
        with patch('pipeline_validator.subprocess.run', side_effect=mock_subprocess_run):
            result = await self.validator._validate_build_system()
            
            assert result.status in ["WARN", "FAIL"]
            assert result.score < 100
            assert any("Missing build tools" in issue for issue in result.issues)
    
    @pytest.mark.asyncio
    async def test_validate_build_system_compilation_failure(self):
        """Test build system validation with compilation failure"""
        # Mock successful tool checks
        with patch('pipeline_validator.subprocess.run') as mock_subprocess:
            mock_subprocess.return_value.returncode = 0
            
            # Mock failed compilation
            mock_compile_process = Mock()
            mock_compile_process.communicate = AsyncMock(return_value=(b"", b"Compilation error"))
            mock_compile_process.returncode = 1
            
            with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_compile_process):
                result = await self.validator._validate_build_system()
                
                assert result.status in ["WARN", "FAIL"]
                assert result.metrics["compilation_success"] is False
                assert any("OWL compilation failed" in issue for issue in result.issues)
    
    @pytest.mark.asyncio
    async def test_validate_build_system_missing_optimization(self):
        """Test build system validation with missing optimization flags"""
        # Mock tools and compilation success
        with patch('pipeline_validator.subprocess.run') as mock_subprocess:
            mock_subprocess.return_value.returncode = 0
            
            mock_compile_process = Mock()
            mock_compile_process.communicate = AsyncMock(return_value=(b"Success", b""))
            mock_compile_process.returncode = 0
            
            with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_compile_process):
                # Mock Makefile without optimization flags
                with patch('pipeline_validator.Path') as mock_path:
                    mock_makefile = Mock()
                    mock_makefile.exists.return_value = True
                    mock_makefile.read_text.return_value = "CFLAGS = -Wall -g"  # No -O3
                    
                    mock_test_build = Mock()
                    mock_test_build.__truediv__ = Mock(return_value=mock_makefile)
                    mock_path.return_value = mock_test_build
                    
                    with patch('pipeline_validator.shutil.rmtree'):
                        result = await self.validator._validate_build_system()
                        
                        # Should have issues about missing optimization
                        optimization_issues = [issue for issue in result.issues if "-O3" in issue]
                        assert len(optimization_issues) > 0
                        
                        # Should have recommendations
                        optimization_recs = [rec for rec in result.recommendations if "-O3" in rec]
                        assert len(optimization_recs) > 0
    
    @pytest.mark.asyncio
    async def test_validate_performance_success(self):
        """Test performance validation success"""
        mock_benchmark_output = """
        🔬 CNS BENCHMARK REPORT
        Performance Score: 95.0/100
        Average operation latency: 2.5ns
        Throughput: 1500000 ops/sec
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_benchmark_output.encode(), b""))
        mock_process.returncode = 0
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_performance()
            
            assert result.status == "PASS"
            assert result.score >= 80
            assert result.metrics["performance_score"] == 95.0
            assert result.metrics["latency_ns"] == 2.5
            assert result.metrics["latency_cycles"] == 7.5  # 2.5ns * 3GHz
            assert result.metrics["throughput_ops_sec"] == 1500000
            assert len(result.issues) == 0
    
    @pytest.mark.asyncio
    async def test_validate_performance_exceeds_8_tick(self):
        """Test performance validation with latency exceeding 8-tick requirement"""
        mock_benchmark_output = """
        Performance Score: 80.0/100
        Average operation latency: 5.0ns
        Throughput: 800000 ops/sec
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_benchmark_output.encode(), b""))
        mock_process.returncode = 0
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_performance()
            
            # 5.0ns * 3GHz = 15 cycles > 8 tick requirement
            assert result.score < 100  # Should be penalized
            assert result.metrics["latency_cycles"] == 15.0
            
            # Should have issues about exceeding 8-tick requirement
            latency_issues = [issue for issue in result.issues if "exceeds 8-tick" in issue]
            assert len(latency_issues) > 0
    
    @pytest.mark.asyncio
    async def test_validate_performance_low_throughput(self):
        """Test performance validation with low throughput"""
        mock_benchmark_output = """
        Performance Score: 70.0/100
        Average operation latency: 1.0ns
        Throughput: 500000 ops/sec
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_benchmark_output.encode(), b""))
        mock_process.returncode = 0
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_performance()
            
            # Should have issues about low throughput
            throughput_issues = [issue for issue in result.issues if "below 1M ops/sec" in issue]
            assert len(throughput_issues) > 0
            
            # Should have recommendations
            optimization_recs = [rec for rec in result.recommendations if "SIMD" in rec]
            assert len(optimization_recs) > 0
    
    @pytest.mark.asyncio
    async def test_validate_otel_integration_success(self):
        """Test OTEL integration validation success"""
        mock_otel_output = """
        🏁 CNS OPENTELEMETRY VALIDATION REPORT
        Success Rate: 100.0%
        cns.health metrics found
        cns.monitor metrics found
        cns.neural metrics found
        cns.validator metrics found
        {"resource_metrics": [{"metric1": "value1"}]}
        {"resource_metrics": [{"metric2": "value2"}]}
        {"resource_metrics": [{"metric3": "value3"}]}
        {"resource_metrics": [{"metric4": "value4"}]}
        {"resource_metrics": [{"metric5": "value5"}]}
        {"name": "trace1"}
        {"name": "trace2"}
        {"name": "trace3"}
        {"name": "trace4"}
        {"name": "trace5"}
        {"name": "trace6"}
        {"name": "trace7"}
        {"name": "trace8"}
        {"name": "trace9"}
        {"name": "trace10"}
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_otel_output.encode(), b""))
        mock_process.returncode = 0
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_otel_integration()
            
            assert result.status == "PASS"
            assert result.score == 100.0
            assert result.metrics["otel_success_rate"] == 100.0
            assert result.metrics["metric_exports"] == 5
            assert result.metrics["trace_exports"] == 10
            assert len(result.issues) == 0
    
    @pytest.mark.asyncio
    async def test_validate_otel_integration_incomplete(self):
        """Test OTEL integration validation with incomplete integration"""
        mock_otel_output = """
        Success Rate: 60.0%
        cns.health metrics found
        cns.monitor metrics found
        {"resource_metrics": [{"metric1": "value1"}]}
        {"resource_metrics": [{"metric2": "value2"}]}
        {"name": "trace1"}
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_otel_output.encode(), b""))
        mock_process.returncode = 0
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_otel_integration()
            
            assert result.status in ["WARN", "FAIL"]
            assert result.score == 60.0
            
            # Should have issues about incomplete OTEL validation
            incomplete_issues = [issue for issue in result.issues if "OTEL validation incomplete" in issue]
            assert len(incomplete_issues) > 0
            
            # Should have issues about insufficient exports
            export_issues = [issue for issue in result.issues if "Insufficient OTEL metric exports" in issue]
            assert len(export_issues) > 0
            
            # Should have issues about missing components
            missing_issues = [issue for issue in result.issues if "Missing OTEL instrumentation" in issue]
            assert len(missing_issues) > 0
    
    @pytest.mark.asyncio
    async def test_validate_code_quality_success(self):
        """Test code quality validation success"""
        # Mock successful ruff check
        mock_ruff_process = Mock()
        mock_ruff_process.communicate = AsyncMock(return_value=(b"", b""))
        mock_ruff_process.returncode = 0
        
        # Mock Python files with type hints
        mock_py_files = []
        for i in range(5):
            mock_file = Mock()
            mock_file.read_text.return_value = f"def function_{i}(param: str) -> int: pass"
            mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_ruff_process):
            with patch('pipeline_validator.Path') as mock_path:
                mock_path.return_value.glob.return_value = mock_py_files
                
                result = await self.validator._validate_code_quality()
                
                assert result.status == "PASS"
                assert result.score >= 80
                assert result.metrics["ruff_issues"] == 0
                assert result.metrics["type_hint_coverage"] == 1.0
                assert len(result.issues) == 0
    
    @pytest.mark.asyncio
    async def test_validate_code_quality_many_ruff_issues(self):
        """Test code quality validation with many ruff issues"""
        # Mock ruff with many issues
        mock_ruff_output = "\n".join([f"issue_{i}" for i in range(15)])  # 15 issues
        mock_ruff_process = Mock()
        mock_ruff_process.communicate = AsyncMock(return_value=(mock_ruff_output.encode(), b""))
        mock_ruff_process.returncode = 1
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_ruff_process):
            with patch('pipeline_validator.Path') as mock_path:
                mock_path.return_value.glob.return_value = []
                
                result = await self.validator._validate_code_quality()
                
                assert result.status in ["WARN", "FAIL"]
                assert result.metrics["ruff_issues"] == 14  # 15 lines - 1
                
                # Should have issues about too many linting issues
                ruff_issues = [issue for issue in result.issues if "Too many linting issues" in issue]
                assert len(ruff_issues) > 0
                
                # Should have recommendations
                ruff_recs = [rec for rec in result.recommendations if "ruff check --fix" in rec]
                assert len(ruff_recs) > 0
    
    @pytest.mark.asyncio
    async def test_validate_code_quality_low_type_coverage(self):
        """Test code quality validation with low type hint coverage"""
        # Mock successful ruff
        mock_ruff_process = Mock()
        mock_ruff_process.communicate = AsyncMock(return_value=(b"", b""))
        mock_ruff_process.returncode = 0
        
        # Mock Python files mostly without type hints
        mock_py_files = []
        for i in range(10):
            mock_file = Mock()
            if i < 3:  # Only 3 out of 10 have type hints
                mock_file.read_text.return_value = f"def function_{i}(param: str) -> int: pass"
            else:
                mock_file.read_text.return_value = f"def function_{i}(param): pass"
            mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_ruff_process):
            with patch('pipeline_validator.Path') as mock_path:
                mock_path.return_value.glob.return_value = mock_py_files
                
                result = await self.validator._validate_code_quality()
                
                assert result.metrics["type_hint_coverage"] == 0.3  # 3/10
                
                # Should have issues about low type hint coverage
                type_issues = [issue for issue in result.issues if "Low type hint coverage" in issue]
                assert len(type_issues) > 0
    
    @pytest.mark.asyncio
    async def test_validate_code_quality_unsafe_c_patterns(self):
        """Test code quality validation detecting unsafe C patterns"""
        # Mock successful ruff
        mock_ruff_process = Mock()
        mock_ruff_process.communicate = AsyncMock(return_value=(b"", b""))
        mock_ruff_process.returncode = 0
        
        # Mock C files with unsafe patterns
        mock_c_files = []
        for i in range(3):
            mock_file = Mock()
            mock_file.read_text.return_value = f"strcpy(dest, src); strcat(buffer, input);"
            mock_c_files.append(mock_file)
        
        with patch('pipeline_validator.Path') as mock_path:
            mock_path.return_value.glob.side_effect = [[], mock_c_files]  # No .py files, some .c files
            
            with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_ruff_process):
                result = await self.validator._validate_code_quality()
                
                # Should detect unsafe C functions
                unsafe_issues = [issue for issue in result.issues if "unsafe C functions" in issue]
                assert len(unsafe_issues) > 0
                
                # Should have recommendations
                safe_recs = [rec for rec in result.recommendations if "safe variants" in rec]
                assert len(safe_recs) > 0
    
    @pytest.mark.asyncio
    async def test_validate_security_success(self):
        """Test security validation success"""
        # Mock Python files without secrets
        mock_py_files = []
        for i in range(5):
            mock_file = Mock()
            mock_file.name = f"file_{i}.py"
            mock_file.read_text.return_value = f"def function_{i}(): return 42"
            mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.Path') as mock_path:
            mock_path.return_value.glob.return_value = mock_py_files
            
            # Mock file permissions check
            with patch('pipeline_validator.os.stat') as mock_stat:
                mock_stat.return_value.st_mode = 0o750  # Secure permissions
                
                # Mock pyproject.toml with exact versions
                mock_pyproject = Mock()
                mock_pyproject.exists.return_value = True
                mock_pyproject.read_text.return_value = 'dependencies = ["package==1.0.0"]'
                mock_path.return_value = mock_pyproject
                
                result = await self.validator._validate_security()
                
                assert result.status == "PASS"
                assert result.score == 100.0
                assert len(result.issues) == 0
    
    @pytest.mark.asyncio 
    async def test_validate_security_hardcoded_secrets(self):
        """Test security validation detecting hardcoded secrets"""
        # Mock Python files with potential secrets
        mock_py_files = []
        mock_file = Mock()
        mock_file.name = "config.py"
        mock_file.read_text.return_value = '''
        api_key = "secret123456"
        password = "hardcoded_password"
        '''
        mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.Path') as mock_path:
            mock_path.return_value.glob.return_value = mock_py_files
            
            result = await self.validator._validate_security()
            
            assert result.status in ["WARN", "FAIL"]
            assert result.score < 100
            
            # Should detect hardcoded secrets
            secret_issues = [issue for issue in result.issues if "hardcoded secret" in issue]
            assert len(secret_issues) > 0
    
    @pytest.mark.asyncio
    async def test_validate_security_insecure_permissions(self):
        """Test security validation detecting insecure file permissions"""
        with patch('pipeline_validator.Path') as mock_path:
            mock_path.return_value.glob.return_value = []
            
            # Mock deploy script with insecure permissions
            deploy_script = Mock()
            deploy_script.exists.return_value = True
            
            def mock_path_constructor(filename):
                if filename == "deploy_local.sh":
                    return deploy_script
                return Mock(exists=Mock(return_value=False))
            
            mock_path.side_effect = mock_path_constructor
            
            with patch('pipeline_validator.os.stat') as mock_stat:
                mock_stat.return_value.st_mode = 0o777  # World writable
                
                result = await self.validator._validate_security()
                
                assert result.status in ["WARN", "FAIL"]
                assert result.score < 100
                
                # Should detect insecure permissions
                perm_issues = [issue for issue in result.issues if "Insecure permissions" in issue]
                assert len(perm_issues) > 0
                
                # Should have recommendations
                chmod_recs = [rec for rec in result.recommendations if "chmod 750" in rec]
                assert len(chmod_recs) > 0
    
    @pytest.mark.asyncio
    async def test_validate_deployment_success(self):
        """Test deployment validation success"""
        # Mock all required deployment files exist
        required_files = [
            "deploy_local.sh",
            "Makefile.deploy", 
            ".github/workflows/cns-deploy.yml",
            ".github/workflows/cns-ci.yml"
        ]
        
        def mock_path_exists(filename):
            mock_file = Mock()
            mock_file.exists.return_value = filename in required_files
            if filename == "deploy_local.sh":
                mock_file.read_text.return_value = "rollback() { echo 'Rolling back'; }"
            return mock_file
        
        with patch('pipeline_validator.Path', side_effect=mock_path_exists):
            # Mock successful deployment script test
            mock_process = Mock()
            mock_process.communicate = AsyncMock(return_value=(b"Status OK", b""))
            mock_process.returncode = 0
            
            with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
                result = await self.validator._validate_deployment()
                
                assert result.status == "PASS"
                assert result.score >= 80
                assert result.metrics["deployment_script_functional"] is True
                assert len(result.issues) == 0
    
    @pytest.mark.asyncio
    async def test_validate_deployment_missing_files(self):
        """Test deployment validation with missing required files"""
        # Mock missing deployment files
        def mock_path_exists(filename):
            mock_file = Mock()
            mock_file.exists.return_value = False  # All files missing
            return mock_file
        
        with patch('pipeline_validator.Path', side_effect=mock_path_exists):
            result = await self.validator._validate_deployment()
            
            assert result.status in ["WARN", "FAIL"]
            assert result.score < 100
            
            # Should have issues about missing files
            missing_issues = [issue for issue in result.issues if "Missing deployment files" in issue]
            assert len(missing_issues) > 0
    
    @pytest.mark.asyncio
    async def test_validate_deployment_no_rollback(self):
        """Test deployment validation without rollback functionality"""
        # Mock deploy script without rollback
        deploy_script = Mock()
        deploy_script.exists.return_value = True
        deploy_script.read_text.return_value = "deploy() { echo 'Deploying'; }"  # No rollback
        
        def mock_path_exists(filename):
            if filename == "deploy_local.sh":
                return deploy_script
            mock_file = Mock()
            mock_file.exists.return_value = True
            return mock_file
        
        with patch('pipeline_validator.Path', side_effect=mock_path_exists):
            result = await self.validator._validate_deployment()
            
            # Should have issues about missing rollback
            rollback_issues = [issue for issue in result.issues if "No rollback functionality" in issue]
            assert len(rollback_issues) > 0
            
            # Should have recommendations
            rollback_recs = [rec for rec in result.recommendations if "automated rollback" in rec]
            assert len(rollback_recs) > 0
    
    @pytest.mark.asyncio
    async def test_validate_monitoring_success(self):
        """Test monitoring validation success"""
        # Mock all monitoring files exist
        monitoring_files = ["cns_monitor.py", "cns_status.py", "validate_otel.py"]
        
        def mock_path_exists(filename):
            mock_file = Mock()
            mock_file.exists.return_value = filename in monitoring_files
            return mock_file
        
        # Mock successful CNS status
        mock_status_data = {
            "timestamp": "2023-01-01T12:00:00",
            "health_score": 95.0,
            "status": "OPTIMAL",
            "system": {"cpu": 50},
            "cns": {"version": "1.0"},
            "performance": {"latency": 5.0}
        }
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(json.dumps(mock_status_data).encode(), b""))
        mock_process.returncode = 0
        
        with patch('pipeline_validator.Path', side_effect=mock_path_exists):
            with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
                # Mock alerting detection
                mock_alert_file = Mock()
                mock_alert_file.read_text.return_value = "if threshold > limit: alert('High usage')"
                
                with patch('pipeline_validator.Path') as mock_path_glob:
                    mock_path_glob.return_value.glob.return_value = [mock_alert_file]
                    
                    result = await self.validator._validate_monitoring()
                    
                    assert result.status == "PASS"
                    assert result.score >= 80
                    assert result.metrics["health_score"] == 95.0
                    assert len(result.issues) == 0
    
    @pytest.mark.asyncio
    async def test_validate_monitoring_missing_components(self):
        """Test monitoring validation with missing components"""
        # Mock missing monitoring files
        def mock_path_exists(filename):
            mock_file = Mock()
            mock_file.exists.return_value = False
            return mock_file
        
        with patch('pipeline_validator.Path', side_effect=mock_path_exists):
            result = await self.validator._validate_monitoring()
            
            assert result.status in ["WARN", "FAIL"]
            assert result.score < 100
            
            # Should have issues about missing components
            missing_issues = [issue for issue in result.issues if "Missing monitoring component" in issue]
            assert len(missing_issues) > 0
    
    @pytest.mark.asyncio
    async def test_validate_monitoring_no_alerting(self):
        """Test monitoring validation without alerting"""
        # Mock monitoring files exist but no alerting
        monitoring_files = ["cns_monitor.py", "cns_status.py", "validate_otel.py"]
        
        def mock_path_exists(filename):
            mock_file = Mock()
            mock_file.exists.return_value = filename in monitoring_files
            return mock_file
        
        mock_status_data = {"timestamp": "2023-01-01", "health_score": 95.0, "status": "OK", "system": {}, "cns": {}, "performance": {}}
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(json.dumps(mock_status_data).encode(), b""))
        mock_process.returncode = 0
        
        with patch('pipeline_validator.Path', side_effect=mock_path_exists):
            with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
                # Mock no alerting patterns found
                mock_no_alert_file = Mock()
                mock_no_alert_file.read_text.return_value = "def monitor(): print('Monitoring')"
                
                with patch('pipeline_validator.Path') as mock_path_glob:
                    mock_path_glob.return_value.glob.return_value = [mock_no_alert_file]
                    
                    result = await self.validator._validate_monitoring()
                    
                    # Should have recommendations about alerting
                    alert_recs = [rec for rec in result.recommendations if "alerting" in rec]
                    assert len(alert_recs) > 0
    
    @pytest.mark.asyncio
    async def test_validate_neural_integration_success(self):
        """Test neural integration validation success"""
        mock_neural_output = """
        🧠 CNS NEURAL INTEGRATION REPORT
        Success Rate: 100.0%
        throughput_ips: 75.0
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_neural_output.encode(), b""))
        mock_process.returncode = 0
        
        # Mock DSPy signatures file
        mock_signatures_file = Mock()
        mock_signatures_file.exists.return_value = True
        mock_signatures_file.read_text.return_value = """
        class Signature1(dspy.Signature): pass
        class Signature2(dspy.Signature): pass
        class Signature3(dspy.Signature): pass
        class Signature4(dspy.Signature): pass
        """
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
            with patch('pipeline_validator.Path', return_value=mock_signatures_file):
                result = await self.validator._validate_neural_integration()
                
                assert result.status == "PASS"
                assert result.score == 100.0
                assert result.metrics["neural_success_rate"] == 100.0
                assert result.metrics["neural_throughput_ips"] == 75.0
                assert result.metrics["dspy_signatures"] == 3  # 4 classes - 1 for imports
                assert len(result.issues) == 0
    
    @pytest.mark.asyncio
    async def test_validate_neural_integration_low_performance(self):
        """Test neural integration validation with low performance"""
        mock_neural_output = """
        Success Rate: 85.0%
        throughput_ips: 30.0
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_neural_output.encode(), b""))
        mock_process.returncode = 0
        
        mock_signatures_file = Mock()
        mock_signatures_file.exists.return_value = True
        mock_signatures_file.read_text.return_value = "class Signature1(dspy.Signature): pass"
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
            with patch('pipeline_validator.Path', return_value=mock_signatures_file):
                result = await self.validator._validate_neural_integration()
                
                assert result.status in ["WARN", "FAIL"]
                assert result.score == 85.0
                
                # Should have issues about low performance
                perf_issues = [issue for issue in result.issues if "Neural inference too slow" in issue]
                assert len(perf_issues) > 0
                
                # Should have recommendations
                opt_recs = [rec for rec in result.recommendations if "optimization" in rec]
                assert len(opt_recs) > 0
                
                # Should have issues about few signatures
                sig_issues = [issue for issue in result.issues if "Too few DSPy signatures" in issue]
                assert len(sig_issues) > 0
    
    @pytest.mark.asyncio
    async def test_validate_error_handling_success(self):
        """Test error handling validation success"""
        # Mock Python files with good error handling
        mock_py_files = []
        for i in range(10):
            mock_file = Mock()
            content = f"""
def function_{i}():
    try:
        risky_operation()
    except SpecificException as e:
        logger.error(f"Error: {{e}}")
        raise
"""
            mock_file.read_text.return_value = content
            mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.Path') as mock_path:
            mock_path.return_value.glob.return_value = mock_py_files
            
            result = await self.validator._validate_error_handling()
            
            assert result.status == "PASS"
            assert result.score >= 80
            assert result.metrics["error_handling_coverage"] == 1.0  # All functions have try/except
            assert len(result.issues) == 0
    
    @pytest.mark.asyncio
    async def test_validate_error_handling_poor_coverage(self):
        """Test error handling validation with poor coverage"""
        # Mock Python files with poor error handling
        mock_py_files = []
        for i in range(10):
            mock_file = Mock()
            if i < 3:  # Only 3 out of 10 have error handling
                content = f"""
def function_{i}():
    try:
        operation()
    except:  # Bare except - also bad
        pass
"""
            else:
                content = f"def function_{i}(): risky_operation()"
            
            mock_file.read_text.return_value = content
            mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.Path') as mock_path:
            mock_path.return_value.glob.return_value = mock_py_files
            
            result = await self.validator._validate_error_handling()
            
            assert result.status in ["WARN", "FAIL"]
            assert result.metrics["error_handling_coverage"] == 0.3  # 3/10
            
            # Should have issues about low coverage
            coverage_issues = [issue for issue in result.issues if "Low error handling coverage" in issue]
            assert len(coverage_issues) > 0
            
            # Should have issues about bare except
            bare_issues = [issue for issue in result.issues if "bare except clauses" in issue]
            assert len(bare_issues) > 0
            
            # Should have recommendations
            error_recs = [rec for rec in result.recommendations if "try/except blocks" in rec]
            specific_recs = [rec for rec in result.recommendations if "specific exception types" in rec]
            assert len(error_recs) > 0
            assert len(specific_recs) > 0
    
    @pytest.mark.asyncio
    async def test_validate_error_handling_no_logging(self):
        """Test error handling validation without logging"""
        # Mock files without logging
        mock_py_files = []
        for i in range(5):
            mock_file = Mock()
            mock_file.read_text.return_value = f"def function_{i}(): pass"
            mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.Path') as mock_path:
            mock_path.return_value.glob.return_value = mock_py_files
            
            result = await self.validator._validate_error_handling()
            
            # Should have issues about no logging
            logging_issues = [issue for issue in result.issues if "No logging framework detected" in issue]
            assert len(logging_issues) > 0
            
            # Should have recommendations
            logging_recs = [rec for rec in result.recommendations if "structured logging" in rec]
            assert len(logging_recs) > 0
    
    @pytest.mark.asyncio
    async def test_validate_documentation_success(self):
        """Test documentation validation success"""
        # Mock documentation files exist
        doc_files = {
            "README.md": True,
            "deploy/README.md": True,
            "docs/": True
        }
        
        def mock_path_exists(filename):
            mock_file = Mock()
            mock_file.exists.return_value = doc_files.get(filename, False)
            return mock_file
        
        # Mock Python files with good docstring coverage
        mock_py_files = []
        for i in range(5):
            mock_file = Mock()
            content = f'''
class TestClass{i}:
    """Well documented class."""
    
    def method{i}(self):
        """Well documented method."""
        pass
'''
            mock_file.read_text.return_value = content
            mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.Path', side_effect=mock_path_exists):
            with patch('pipeline_validator.Path') as mock_path_glob:
                mock_path_glob.return_value.glob.return_value = mock_py_files
                
                result = await self.validator._validate_documentation()
                
                assert result.status == "PASS"
                assert result.score >= 80
                assert result.metrics["class_documentation"] == 1.0
                assert result.metrics["function_documentation"] == 1.0
                assert len(result.issues) == 0
    
    @pytest.mark.asyncio
    async def test_validate_documentation_missing_files(self):
        """Test documentation validation with missing files"""
        # Mock missing documentation files
        def mock_path_exists(filename):
            mock_file = Mock()
            mock_file.exists.return_value = False
            return mock_file
        
        with patch('pipeline_validator.Path', side_effect=mock_path_exists):
            with patch('pipeline_validator.Path') as mock_path_glob:
                mock_path_glob.return_value.glob.return_value = []
                
                result = await self.validator._validate_documentation()
                
                assert result.status in ["WARN", "FAIL"]
                assert result.score < 100
                
                # Should have issues about missing documentation
                missing_issues = [issue for issue in result.issues if "Missing documentation" in issue]
                assert len(missing_issues) > 0
    
    @pytest.mark.asyncio
    async def test_validate_documentation_poor_docstring_coverage(self):
        """Test documentation validation with poor docstring coverage"""
        # Mock files exist
        def mock_path_exists(filename):
            mock_file = Mock()
            mock_file.exists.return_value = True
            return mock_file
        
        # Mock Python files with poor docstring coverage
        mock_py_files = []
        for i in range(10):
            mock_file = Mock()
            if i < 3:  # Only 3/10 classes have docstrings
                content = f'''
class TestClass{i}:
    """Documented class."""
    
    def method{i}(self):
        pass  # No docstring
'''
            else:
                content = f'''
class TestClass{i}:  # No docstring
    
    def method{i}(self):  # No docstring
        pass
'''
            mock_file.read_text.return_value = content
            mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.Path', side_effect=mock_path_exists):
            with patch('pipeline_validator.Path') as mock_path_glob:
                mock_path_glob.return_value.glob.return_value = mock_py_files
                
                result = await self.validator._validate_documentation()
                
                assert result.metrics["class_documentation"] == 0.3  # 3/10
                assert result.metrics["function_documentation"] == 0.0  # 0/10
                
                # Should have issues about low documentation
                class_issues = [issue for issue in result.issues if "Low class documentation" in issue]
                func_issues = [issue for issue in result.issues if "Low function documentation" in issue]
                assert len(class_issues) > 0
                assert len(func_issues) > 0
    
    def test_generate_validation_report_all_pass(self):
        """Test validation report generation with all validations passing"""
        # Setup mock validation results
        self.validator.results = [
            ValidationResult("Test 1", "PASS", 90.0, {"metric": 1}, [], []),
            ValidationResult("Test 2", "PASS", 95.0, {"metric": 2}, [], [])
        ]
        
        with patch('builtins.print') as mock_print:
            report = self.validator._generate_validation_report()
            
            assert "summary" in report
            assert "results" in report
            assert "compliance" in report
            assert "recommendations" in report
            
            summary = report["summary"]
            assert summary["total_validations"] == 2
            assert summary["passed"] == 2
            assert summary["warnings"] == 0
            assert summary["failed"] == 0
            assert summary["overall_score"] == 92.5  # (90+95)/2
            assert summary["verdict"] == "EXCELLENT"
            
            # Check 8T-8H-8M compliance
            compliance = report["compliance"]
            assert "eight_tick" in compliance
            assert "eight_hour" in compliance
            assert "eight_mb" in compliance
    
    def test_generate_validation_report_mixed_results(self):
        """Test validation report generation with mixed results"""
        # Setup mixed validation results
        self.validator.results = [
            ValidationResult("Build System", "PASS", 90.0, {}, [], []),
            ValidationResult("Performance Benchmarks", "WARN", 75.0, {"latency_cycles": 12}, [], ["Optimize hot paths"]),
            ValidationResult("Code Quality", "FAIL", 40.0, {}, ["Too many issues"], ["Run linter"])
        ]
        
        with patch('builtins.print') as mock_print:
            report = self.validator._generate_validation_report()
            
            summary = report["summary"]
            assert summary["total_validations"] == 3
            assert summary["passed"] == 1
            assert summary["warnings"] == 1
            assert summary["failed"] == 1
            assert summary["overall_score"] == 68.33  # (90+75+40)/3
            assert summary["verdict"] == "NEEDS_IMPROVEMENT"
            
            # Check compliance - should fail 8-tick due to high latency
            compliance = report["compliance"]
            assert compliance["eight_tick"] is False  # 12 > 8 cycles
            assert compliance["eight_hour"] is False  # <80% score
    
    def test_generate_validation_report_performance_metrics(self):
        """Test validation report with performance metrics"""
        # Setup results with performance data
        perf_result = ValidationResult(
            "Performance Benchmarks", 
            "PASS", 
            95.0,
            {
                "latency_cycles": 6.0,
                "throughput_ops_sec": 2000000
            },
            [], []
        )
        self.validator.results = [perf_result]
        
        with patch('builtins.print') as mock_print:
            report = self.validator._generate_validation_report()
            
            # Should show compliance details
            print_calls = [str(call) for call in mock_print.call_args_list]
            
            # Check 8T-8H-8M compliance display
            tick_compliance = any("8-Tick" in call and "6.0 cycles" in call for call in print_calls)
            hour_compliance = any("8-Hour" in call and "95.0/100" in call for call in print_calls)
            mb_compliance = any("8-MB/s" in call and "2000000 ops/sec" in call for call in print_calls)
            
            assert tick_compliance
            assert hour_compliance  
            assert mb_compliance
    
    def test_generate_validation_report_recommendations(self):
        """Test validation report with recommendations"""
        self.validator.results = [
            ValidationResult("Test 1", "WARN", 70.0, {}, ["Issue 1"], ["Recommendation 1", "Recommendation 2"]),
            ValidationResult("Test 2", "FAIL", 50.0, {}, ["Issue 2"], ["Recommendation 3"])
        ]
        
        with patch('builtins.print') as mock_print:
            report = self.validator._generate_validation_report()
            
            # Should have top 5 recommendations
            assert len(report["recommendations"]) <= 10
            assert "Recommendation 1" in report["recommendations"]
            assert "Recommendation 2" in report["recommendations"]
            assert "Recommendation 3" in report["recommendations"]
            
            # Should display recommendations in report
            print_calls = [str(call) for call in mock_print.call_args_list]
            rec_section = any("📋 Top Recommendations:" in call for call in print_calls)
            assert rec_section
    
    def test_generate_validation_report_mermaid_diagrams(self):
        """Test that validation report includes Mermaid diagrams"""
        self.validator.results = [
            ValidationResult("Build System", "PASS", 90.0, {}, [], []),
            ValidationResult("Performance", "FAIL", 40.0, {}, [], [])
        ]
        
        with patch('builtins.print') as mock_print:
            self.validator._generate_validation_report()
            
            print_calls = [str(call) for call in mock_print.call_args_list]
            
            # Check for Mermaid diagrams
            mermaid_graph = any("```mermaid" in call and "graph TD" in call for call in print_calls)
            mermaid_pie = any("```mermaid" in call and "pie title" in call for call in print_calls)
            
            assert mermaid_graph
            assert mermaid_pie
    
    @pytest.mark.asyncio
    async def test_validate_pipeline_comprehensive(self):
        """Test comprehensive pipeline validation"""
        # Mock all validation methods to return success
        mock_validation_result = ValidationResult(
            component="Mock Component",
            status="PASS",
            score=90.0,
            metrics={"test_metric": 100},
            issues=[],
            recommendations=[]
        )
        
        validation_methods = [
            '_validate_build_system',
            '_validate_performance', 
            '_validate_otel_integration',
            '_validate_code_quality',
            '_validate_security',
            '_validate_deployment',
            '_validate_monitoring',
            '_validate_neural_integration',
            '_validate_error_handling',
            '_validate_documentation'
        ]
        
        patches = []
        for method_name in validation_methods:
            patch_obj = patch.object(self.validator, method_name, return_value=mock_validation_result)
            patches.append(patch_obj)
        
        with patch.object(self.validator.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            # Apply all patches
            for patch_obj in patches:
                patch_obj.start()
            
            try:
                with patch('builtins.print'):
                    report = await self.validator.validate_pipeline()
                    
                    assert "summary" in report 
                    assert len(self.validator.results) == 10  # All 10 validations ran
                    
                    summary = report["summary"]
                    assert summary["total_validations"] == 10
                    assert summary["overall_score"] == 90.0
                    assert summary["verdict"] == "EXCELLENT"
                    
            finally:
                # Stop all patches
                for patch_obj in patches:
                    patch_obj.stop()


class TestAsyncMainFunction:
    """Test main function and async execution"""
    
    @pytest.mark.asyncio
    async def test_main_function_success(self):
        """Test main function successful execution"""
        with patch('pipeline_validator.CNSPipelineValidator') as mock_validator_class:
            mock_validator = Mock()
            mock_validator.validate_pipeline = AsyncMock(return_value={"summary": {"verdict": "EXCELLENT"}})
            mock_validator_class.return_value = mock_validator
            
            from pipeline_validator import main
            
            with patch('builtins.open', mock_open()) as mock_file:
                with patch('pipeline_validator.json.dump') as mock_json_dump:
                    with patch('pipeline_validator.datetime') as mock_datetime:
                        mock_datetime.now.return_value.strftime.return_value = "20230101-120000"
                        
                        await main()
                        
                        mock_validator.validate_pipeline.assert_called_once()
                        mock_json_dump.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_main_function_keyboard_interrupt(self):
        """Test main function with KeyboardInterrupt"""
        with patch('pipeline_validator.CNSPipelineValidator') as mock_validator_class:
            mock_validator = Mock()
            mock_validator.validate_pipeline = AsyncMock(side_effect=KeyboardInterrupt())
            mock_validator_class.return_value = mock_validator
            
            from pipeline_validator import main
            
            with patch('builtins.print') as mock_print:
                await main()
                
                # Should print keyboard interrupt message
                print_calls = [str(call) for call in mock_print.call_args_list]
                interrupt_message = any("stopped by user" in call for call in print_calls)
                assert interrupt_message
    
    @pytest.mark.asyncio
    async def test_main_function_exception(self):
        """Test main function with general exception"""
        with patch('pipeline_validator.CNSPipelineValidator') as mock_validator_class:
            mock_validator = Mock()
            mock_validator.validate_pipeline = AsyncMock(side_effect=RuntimeError("Test error"))
            mock_validator_class.return_value = mock_validator
            
            from pipeline_validator import main
            
            with patch('builtins.print') as mock_print:
                with pytest.raises(RuntimeError):
                    await main()
                
                # Should print error message
                print_calls = [str(call) for call in mock_print.call_args_list]
                error_message = any("failed with error" in call for call in print_calls)
                assert error_message


class TestPerformanceAndEdgeCases:
    """Test performance characteristics and edge cases"""
    
    def setup_method(self):
        """Setup for each test method"""
        with patch('pipeline_validator.PeriodicExportingMetricReader'):
            with patch('pipeline_validator.MeterProvider'):
                with patch('pipeline_validator.TracerProvider'):
                    with patch('pipeline_validator.BatchSpanProcessor'):
                        with patch('pipeline_validator.metrics.set_meter_provider'):
                            with patch('pipeline_validator.trace.set_tracer_provider'):
                                self.validator = CNSPipelineValidator()
    
    @pytest.mark.asyncio
    async def test_large_codebase_analysis(self):
        """Test performance with large codebase"""
        # Mock large number of Python files
        mock_py_files = []
        for i in range(1000):  # 1000 files
            mock_file = Mock()
            mock_file.read_text.return_value = f"def function_{i}(): pass"
            mock_py_files.append(mock_file)
        
        with patch('pipeline_validator.Path') as mock_path:
            mock_path.return_value.glob.return_value = mock_py_files
            
            import time
            start_time = time.time()
            result = await self.validator._validate_code_quality()
            end_time = time.time()
            
            # Should complete in reasonable time
            execution_time = end_time - start_time
            assert execution_time < 5.0  # Less than 5 seconds
            
            assert "success" in result.__dict__
    
    @pytest.mark.asyncio
    async def test_subprocess_timeout_handling(self):
        """Test subprocess timeout handling"""
        # Mock subprocess that hangs
        mock_process = Mock()
        mock_process.communicate = AsyncMock(side_effect=asyncio.TimeoutError())
        
        with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
            # Should handle timeout gracefully
            result = await self.validator._validate_build_system()
            
            assert result.status == "FAIL"
            assert result.score == 0
    
    @pytest.mark.asyncio
    async def test_file_system_errors(self):
        """Test handling of file system errors"""
        # Mock file system operations that raise exceptions
        with patch('pipeline_validator.Path') as mock_path:
            mock_path.side_effect = PermissionError("Permission denied")
            
            result = await self.validator._validate_security()
            
            # Should handle file system errors gracefully
            assert result.status == "FAIL"
            assert len(result.issues) > 0
    
    @pytest.mark.asyncio 
    async def test_malformed_output_parsing(self):
        """Test parsing of malformed subprocess outputs"""
        malformed_outputs = [
            b"",  # Empty output
            b"Invalid JSON {not valid",  # Malformed JSON
            b"No structured data at all",  # No parseable data
            b"\x00\x01\x02\x03",  # Binary data
            "Unicode: 测试 🎉".encode('utf-8'),  # Unicode
            b"Performance Score: not_a_number/100",  # Invalid numbers
        ]
        
        for output in malformed_outputs:
            mock_process = Mock()
            mock_process.communicate = AsyncMock(return_value=(output, b""))
            mock_process.returncode = 0
            
            with patch('pipeline_validator.asyncio.create_subprocess_exec', return_value=mock_process):
                result = await self.validator._validate_performance()
                
                # Should handle malformed output gracefully
                assert hasattr(result, 'status')
                assert hasattr(result, 'score')
    
    def test_best_practices_values(self):
        """Test that best practices contain reasonable values"""
        practices = self.validator.best_practices
        
        # Performance thresholds should be challenging but achievable
        assert practices['performance']['latency_threshold_ms'] <= 10
        assert practices['performance']['throughput_min_ops'] >= 100000
        
        # Reliability requirements should be high
        assert practices['reliability']['error_rate_max'] <= 0.001
        assert practices['reliability']['availability_min'] >= 0.99
        
        # Code quality standards should be reasonable
        assert 0.5 <= practices['code_quality']['test_coverage_min'] <= 1.0
        assert practices['code_quality']['cyclomatic_complexity_max'] >= 5
    
    @pytest.mark.asyncio
    async def test_concurrent_validations(self):
        """Test running validations concurrently"""
        # Create tasks for multiple validations
        tasks = [
            self.validator._validate_security(),
            self.validator._validate_documentation(),
            self.validator._validate_error_handling()
        ]
        
        # Run concurrently
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # All should complete
        assert len(results) == 3
        for result in results:
            if isinstance(result, Exception):
                # If there's an exception, it should be handled gracefully
                assert isinstance(result, (RuntimeError, ValueError, OSError, PermissionError))
            else:
                # If successful, should be ValidationResult
                assert isinstance(result, ValidationResult)


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])