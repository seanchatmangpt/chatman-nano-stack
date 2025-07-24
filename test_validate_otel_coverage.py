#!/usr/bin/env python3
"""
Comprehensive unit tests for validate_otel.py - 80% line coverage
Tests OTEL integration, metrics validation, tracing
"""

import pytest
import asyncio
import json
import tempfile
import shutil
from pathlib import Path
from unittest.mock import Mock, patch, AsyncMock, MagicMock

from validate_otel import OTELValidator


class TestOTELValidator:
    """Test OTELValidator class functionality"""
    
    def setup_method(self):
        """Setup for each test method"""
        with patch('validate_otel.PeriodicExportingMetricReader'):
            with patch('validate_otel.MeterProvider'):
                with patch('validate_otel.TracerProvider'):
                    with patch('validate_otel.BatchSpanProcessor'):
                        with patch('validate_otel.metrics.set_meter_provider'):
                            with patch('validate_otel.trace.set_tracer_provider'):
                                self.validator = OTELValidator()
    
    def test_otel_validator_initialization(self):
        """Test OTELValidator initialization"""
        assert hasattr(self.validator, 'validation_results')
        assert isinstance(self.validator.validation_results, list)
        assert len(self.validator.validation_results) == 0
        assert hasattr(self.validator, 'start_time')
        assert hasattr(self.validator, 'meter')
        assert hasattr(self.validator, 'tracer')
    
    def test_setup_telemetry(self):
        """Test OpenTelemetry setup"""
        # The setup should create meters, tracers, and instruments
        assert self.validator.meter is not None
        assert self.validator.tracer is not None
        assert hasattr(self.validator, 'validation_counter')
        assert hasattr(self.validator, 'validation_duration')
        assert hasattr(self.validator, 'success_rate_gauge')
    
    @pytest.mark.asyncio
    async def test_run_validation_test_success(self):
        """Test successful validation test execution"""
        async def mock_validator_func():
            return {
                "success": True,
                "metrics": {"test_metric": 42},
                "issues": []
            }
        
        with patch.object(self.validator.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            await self.validator._run_validation_test("Test Validation", mock_validator_func)
            
            assert len(self.validator.validation_results) == 1
            result = self.validator.validation_results[0]
            assert result["name"] == "Test Validation"
            assert result["success"] is True
            assert result["metrics"]["test_metric"] == 42
            assert result["issues"] == []
            assert "duration_ms" in result
            assert "timestamp" in result
    
    @pytest.mark.asyncio
    async def test_run_validation_test_failure(self):
        """Test failed validation test execution"""
        async def mock_validator_func():
            return {
                "success": False,
                "metrics": {},
                "issues": ["Validation error occurred"]
            }
        
        with patch.object(self.validator.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            await self.validator._run_validation_test("Failed Validation", mock_validator_func)
            
            assert len(self.validator.validation_results) == 1
            result = self.validator.validation_results[0]
            assert result["name"] == "Failed Validation"
            assert result["success"] is False
            assert "Validation error occurred" in result["issues"]
    
    @pytest.mark.asyncio
    async def test_run_validation_test_exception(self):
        """Test validation test with exception"""
        async def mock_validator_func():
            raise ValueError("Test validation exception")
        
        with patch.object(self.validator.tracer, 'start_as_current_span') as mock_span:
            mock_span.return_value.__enter__ = Mock()
            mock_span.return_value.__exit__ = Mock()
            mock_span_obj = Mock()
            mock_span.return_value = mock_span_obj
            
            await self.validator._run_validation_test("Exception Validation", mock_validator_func)
            
            assert len(self.validator.validation_results) == 1
            result = self.validator.validation_results[0]
            assert result["name"] == "Exception Validation"
            assert result["success"] is False
            assert "error" in result
            assert "Test validation exception" in result["error"]
    
    @pytest.mark.asyncio
    async def test_validate_cns_status_success(self):
        """Test CNS status validation success"""
        mock_status_data = {
            "timestamp": "2023-01-01T12:00:00",
            "health_score": 95.0,
            "status": "OPTIMAL",
            "system": {"cpu_percent": 50.0},
            "cns": {"version": "1.0.0"},
            "performance": {
                "avg_latency_ms": 5.0,
                "throughput_ops_sec": 1000.0
            }
        }
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(json.dumps(mock_status_data).encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_cns_status()
            
            assert result["success"] is True
            assert result["metrics"]["health_score"] == 95.0
            assert result["metrics"]["status"] == "OPTIMAL"
            assert len(result["issues"]) == 0
    
    @pytest.mark.asyncio
    async def test_validate_cns_status_missing_fields(self):
        """Test CNS status validation with missing required fields"""
        incomplete_status_data = {
            "timestamp": "2023-01-01T12:00:00",
            "health_score": 95.0
            # Missing required fields: status, system, cns, performance
        }
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(json.dumps(incomplete_status_data).encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_cns_status()
            
            assert result["success"] is False
            assert len(result["issues"]) > 0
            
            # Check that specific missing fields are reported
            missing_fields = [issue for issue in result["issues"] if "Missing required field" in issue]
            assert len(missing_fields) > 0
    
    @pytest.mark.asyncio
    async def test_validate_cns_status_invalid_json(self):
        """Test CNS status validation with invalid JSON"""
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(b"Invalid JSON response", b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_cns_status()
            
            assert result["success"] is False
            assert any("not valid JSON" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_validate_cns_status_command_failure(self):
        """Test CNS status validation with command failure"""
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(b"", b"Command failed"))
        mock_process.returncode = 1
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_cns_status()
            
            assert result["success"] is False
            assert any("failed with code 1" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_validate_cns_status_missing_performance_metrics(self):
        """Test CNS status validation with missing OTEL performance metrics"""
        status_data_missing_perf = {
            "timestamp": "2023-01-01T12:00:00",
            "health_score": 95.0,
            "status": "OPTIMAL",
            "system": {"cpu_percent": 50.0},
            "cns": {"version": "1.0.0"},
            "performance": {
                # Missing avg_latency_ms and throughput_ops_sec
            }
        }
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(json.dumps(status_data_missing_perf).encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_cns_status()
            
            assert result["success"] is False
            performance_issues = [issue for issue in result["issues"] if "OTEL performance metric" in issue]
            assert len(performance_issues) == 2  # Missing both metrics
    
    @pytest.mark.asyncio 
    async def test_validate_owl_compiler_success(self):
        """Test OWL compiler validation success"""
        # Mock successful compilation
        mock_compile_process = Mock()
        mock_compile_process.communicate = AsyncMock(return_value=(b"Compilation successful", b""))
        mock_compile_process.returncode = 0
        
        # Mock successful C compilation
        mock_gcc_process = Mock()
        mock_gcc_process.wait = AsyncMock(return_value=None)
        mock_gcc_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec') as mock_subprocess:
            mock_subprocess.side_effect = [mock_compile_process, mock_gcc_process]
            
            # Mock file system operations
            with patch('validate_otel.Path') as mock_path:
                mock_validation_dir = Mock()
                mock_validation_dir.exists.return_value = True
                
                # Mock expected output files
                expected_files = ["realtime_core.c", "realtime_core.h", "realtime_core.json", "Makefile"]
                file_mocks = {}
                for filename in expected_files:
                    file_mock = Mock()
                    file_mock.exists.return_value = True
                    file_mock.stat.return_value.st_size = 1024  # 1KB file
                    file_mocks[filename] = file_mock
                
                def mock_truediv(filename):
                    return file_mocks.get(filename, Mock(exists=Mock(return_value=False)))
                
                mock_validation_dir.__truediv__ = mock_truediv
                mock_path.return_value = mock_validation_dir
                
                # Mock shutil.rmtree
                with patch('validate_otel.shutil.rmtree'):
                    result = await self.validator._validate_owl_compiler()
                    
                    assert result["success"] is True
                    assert result["metrics"]["compilation_time_s"] > 0
                    assert result["metrics"]["return_code"] == 0
                    assert result["metrics"]["c_compilation_success"] is True
                    
                    # Check that all expected files are accounted for
                    for filename in expected_files:
                        size_key = f"{filename}_size"
                        assert size_key in result["metrics"]
                        assert result["metrics"][size_key] == 1024
    
    @pytest.mark.asyncio
    async def test_validate_owl_compiler_compilation_failure(self):
        """Test OWL compiler validation with compilation failure"""
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(b"", b"Compilation error"))
        mock_process.returncode = 1
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_owl_compiler()
            
            assert result["success"] is False
            assert result["metrics"]["return_code"] == 1
            assert any("compiler failed with code 1" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_validate_owl_compiler_missing_output_files(self):
        """Test OWL compiler validation with missing output files"""
        # Mock successful compilation
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(b"Compilation successful", b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            with patch('validate_otel.Path') as mock_path:
                mock_validation_dir = Mock()
                mock_validation_dir.exists.return_value = True
                
                # Mock missing files
                def mock_truediv(filename):
                    file_mock = Mock()
                    file_mock.exists.return_value = False  # All files missing
                    return file_mock
                
                mock_validation_dir.__truediv__ = mock_truediv
                mock_path.return_value = mock_validation_dir
                
                with patch('validate_otel.shutil.rmtree'):
                    result = await self.validator._validate_owl_compiler()
                    
                    assert result["success"] is False
                    missing_file_issues = [issue for issue in result["issues"] if "Missing expected output file" in issue]
                    assert len(missing_file_issues) == 4  # All 4 expected files missing
    
    @pytest.mark.asyncio
    async def test_validate_owl_compiler_c_compilation_failure(self):
        """Test OWL compiler validation with C compilation failure"""
        # Mock successful OWL compilation
        mock_owl_process = Mock()
        mock_owl_process.communicate = AsyncMock(return_value=(b"OWL compilation successful", b""))
        mock_owl_process.returncode = 0
        
        # Mock failed C compilation
        mock_gcc_process = Mock()
        mock_gcc_process.wait = AsyncMock(return_value=None)
        mock_gcc_process.returncode = 1  # Compilation failed
        
        with patch('validate_otel.asyncio.create_subprocess_exec') as mock_subprocess:
            mock_subprocess.side_effect = [mock_owl_process, mock_gcc_process]
            
            with patch('validate_otel.Path') as mock_path:
                mock_validation_dir = Mock()
                mock_validation_dir.exists.return_value = True
                
                # Mock C file exists
                mock_c_file = Mock()
                mock_c_file.exists.return_value = True
                mock_c_file.stat.return_value.st_size = 1024
                
                def mock_truediv(filename):
                    if filename == "realtime_core.c":
                        return mock_c_file
                    file_mock = Mock()
                    file_mock.exists.return_value = True
                    file_mock.stat.return_value.st_size = 512
                    return file_mock
                
                mock_validation_dir.__truediv__ = mock_truediv
                mock_path.return_value = mock_validation_dir
                
                with patch('validate_otel.shutil.rmtree'):
                    result = await self.validator._validate_owl_compiler()
                    
                    assert result["success"] is False
                    assert any("Generated C code does not compile" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_validate_benchmark_system_success(self):
        """Test benchmark system validation success"""
        mock_benchmark_output = """
        🔬 CNS BENCHMARK REPORT
        ======================
        Performance Score: 85.0/100
        
        ```mermaid
        graph TD
        A[Test] --> B[Result]
        ```
        
        ```mermaid
        pie title Results
        "Passed" : 3
        "Failed" : 1
        ```
        
        ```mermaid
        timeline
        title Timeline
        Test1 : Pass
        ```
        
        📊 OTEL Metrics:
        - benchmark_duration_ms: 250.0
        - performance_score: 85.0
        - test_results_total: 4
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_benchmark_output.encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_benchmark_system()
            
            assert result["success"] is True
            assert result["metrics"]["benchmark_time_s"] > 0
            assert result["metrics"]["return_code"] == 0
            assert result["metrics"]["performance_score"] == 85.0
            assert result["metrics"]["mermaid_diagrams_count"] == 3
            assert len(result["issues"]) == 0
    
    @pytest.mark.asyncio
    async def test_validate_benchmark_system_missing_components(self):
        """Test benchmark system validation with missing components"""
        mock_output_missing_components = """
        Some benchmark output without expected components
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_output_missing_components.encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_benchmark_system()
            
            assert result["success"] is False
            
            # Check for specific missing components
            otel_missing = any("Missing OTEL metrics section" in issue for issue in result["issues"])
            mermaid_missing = any("Expected at least 3 Mermaid diagrams" in issue for issue in result["issues"])
            score_missing = any("Performance score not found" in issue for issue in result["issues"])
            
            assert otel_missing
            assert mermaid_missing
            assert score_missing
    
    @pytest.mark.asyncio
    async def test_validate_benchmark_system_low_performance(self):
        """Test benchmark system validation with low performance score"""
        mock_output_low_score = """
        Performance Score: 65.0/100
        
        ```mermaid
        graph TD
        ```
        ```mermaid  
        pie title Results
        ```
        ```mermaid
        timeline
        ```
        
        📊 OTEL Metrics:
        - benchmark_duration_ms: 500.0
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_output_low_score.encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_benchmark_system()
            
            assert result["success"] is False
            assert result["metrics"]["performance_score"] == 65.0
            assert any("Performance score too low: 65.0/100" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_validate_performance_monitor_success(self):
        """Test performance monitor validation success"""
        mock_monitor_output = """
        🔬 CNS PERFORMANCE MONITOR
        =========================
        system_cpu_percent: 45.2
        system_memory_percent: 62.1
        cns_health_score: 95.0
        compilation_duration_seconds: 2.5
        
        {"resource_metrics": [{"metric": "test"}]}
        cns.monitor metric data
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_monitor_output.encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            with patch('validate_otel.asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = (mock_monitor_output.encode(), b"")
                
                result = await self.validator._validate_performance_monitor()
                
                assert result["success"] is True
                assert result["metrics"]["monitor_time_s"] > 0
                assert result["metrics"]["return_code"] == 0
                assert result["metrics"]["otel_exports_count"] == 1
                assert len(result["issues"]) == 0
    
    @pytest.mark.asyncio
    async def test_validate_performance_monitor_timeout(self):
        """Test performance monitor validation with timeout"""
        with patch('validate_otel.asyncio.create_subprocess_exec') as mock_subprocess:
            mock_process = Mock()
            mock_subprocess.return_value = mock_process
            
            with patch('validate_otel.asyncio.wait_for', side_effect=asyncio.TimeoutError()):
                result = await self.validator._validate_performance_monitor()
                
                assert result["success"] is False
                assert any("timed out" in issue for issue in result["issues"])
                
                # Verify that process was terminated
                mock_process.terminate.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_validate_performance_monitor_missing_features(self):
        """Test performance monitor validation with missing expected features"""
        mock_output_missing_features = """
        Basic monitor output without expected features
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_output_missing_features.encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            with patch('validate_otel.asyncio.wait_for') as mock_wait_for:
                mock_wait_for.return_value = (mock_output_missing_features.encode(), b"")
                
                result = await self.validator._validate_performance_monitor()
                
                assert result["success"] is False
                
                # Should find multiple missing features
                missing_features = [issue for issue in result["issues"] if "Missing expected feature" in issue]
                assert len(missing_features) > 0
    
    @pytest.mark.asyncio
    async def test_validate_generated_code_success(self):
        """Test generated code validation success"""
        mock_binary_path = Mock()
        mock_binary_path.exists.return_value = True
        mock_binary_path.stat.return_value.st_size = 2048000  # 2MB
        
        # Mock self-test output
        mock_self_test_output = """
        📊 Test Results: 4/4 passed
        🎉 All tests PASSED - System is OPTIMAL
        """
        
        mock_self_test_process = Mock()
        mock_self_test_process.communicate = AsyncMock(return_value=(mock_self_test_output.encode(), b""))
        mock_self_test_process.returncode = 0
        
        # Mock help and production mode processes
        mock_mode_process = Mock()
        mock_mode_process.wait = AsyncMock(return_value=None)
        mock_mode_process.returncode = 0
        
        with patch('validate_otel.Path', return_value=mock_binary_path):
            with patch('validate_otel.asyncio.create_subprocess_exec') as mock_subprocess:
                mock_subprocess.side_effect = [mock_self_test_process, mock_mode_process, mock_mode_process]
                
                result = await self.validator._validate_generated_code()
                
                assert result["success"] is True
                assert result["metrics"]["binary_size"] == 2048000
                assert result["metrics"]["self_test_return_code"] == 0
                assert result["metrics"]["tests_passed"] == 4
                assert result["metrics"]["tests_total"] == 4
                assert result["metrics"]["--help_return_code"] == 0
                assert result["metrics"]["--deploy-production_return_code"] == 0
    
    @pytest.mark.asyncio
    async def test_validate_generated_code_binary_not_found(self):
        """Test generated code validation when binary not found"""
        mock_binary_path = Mock()
        mock_binary_path.exists.return_value = False
        
        with patch('validate_otel.Path', return_value=mock_binary_path):
            result = await self.validator._validate_generated_code()
            
            assert result["success"] is False
            assert any("Generated binary not found" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_validate_generated_code_self_test_failure(self):
        """Test generated code validation with self-test failure"""
        mock_binary_path = Mock()
        mock_binary_path.exists.return_value = True
        mock_binary_path.stat.return_value.st_size = 1024000
        
        # Mock failed self-test
        mock_output_failed = """
        📊 Test Results: 1/4 passed
        ❌ Some tests FAILED - System needs attention
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_output_failed.encode(), b""))
        mock_process.returncode = 1
        
        with patch('validate_otel.Path', return_value=mock_binary_path):
            with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
                result = await self.validator._validate_generated_code()
                
                assert result["success"] is False
                assert result["metrics"]["tests_passed"] == 1
                assert result["metrics"]["tests_total"] == 4
                assert any("Too few tests passed: 1/4" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_validate_generated_code_unparseable_results(self):
        """Test generated code validation with unparseable test results"""
        mock_binary_path = Mock()
        mock_binary_path.exists.return_value = True
        mock_binary_path.stat.return_value.st_size = 1024000
        
        mock_output_unparseable = """
        Some output without parseable test results
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(mock_output_unparseable.encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.Path', return_value=mock_binary_path):
            with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
                result = await self.validator._validate_generated_code()
                
                assert result["success"] is False
                assert any("Test results not found" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_validate_otel_integration_success(self):
        """Test OTEL integration validation success"""
        # Mock OTEL components
        mock_meter_provider = Mock()
        mock_tracer_provider = Mock()
        mock_meter = Mock()
        mock_counter = Mock()
        mock_tracer = Mock()
        mock_span = Mock()
        mock_span.__enter__ = Mock(return_value=mock_span)
        mock_span.__exit__ = Mock()
        
        with patch('validate_otel.otel_metrics.get_meter_provider', return_value=mock_meter_provider):
            with patch('validate_otel.otel_trace.get_tracer_provider', return_value=mock_tracer_provider):
                with patch('validate_otel.otel_metrics.get_meter', return_value=mock_meter):
                    with patch.object(mock_meter, 'create_counter', return_value=mock_counter):
                        with patch('validate_otel.otel_trace.get_tracer', return_value=mock_tracer):
                            with patch.object(mock_tracer, 'start_as_current_span', return_value=mock_span):
                                result = await self.validator._validate_otel_integration()
                                
                                assert result["success"] is True
                                assert result["metrics"]["meter_provider_configured"] is True
                                assert result["metrics"]["tracer_provider_configured"] is True
                                assert result["metrics"]["test_metric_created"] is True
                                assert result["metrics"]["test_span_created"] is True
                                assert len(result["issues"]) == 0
    
    @pytest.mark.asyncio
    async def test_validate_otel_integration_missing_providers(self):
        """Test OTEL integration validation with missing providers"""
        with patch('validate_otel.otel_metrics.get_meter_provider', return_value=None):
            with patch('validate_otel.otel_trace.get_tracer_provider', return_value=None):
                result = await self.validator._validate_otel_integration()
                
                assert result["success"] is False
                assert any("meter provider not configured" in issue for issue in result["issues"])
                assert any("tracer provider not configured" in issue for issue in result["issues"])
    
    def test_generate_validation_report_all_pass(self):
        """Test validation report generation with all validations passing"""
        # Setup mock validation results
        self.validator.validation_results = [
            {
                "name": "Test 1",
                "success": True,
                "duration_ms": 100.0,
                "metrics": {"accuracy": 95.0},
                "issues": []
            },
            {
                "name": "Test 2",
                "success": True,
                "duration_ms": 150.0,
                "metrics": {"throughput": 50.0},
                "issues": []
            }
        ]
        
        with patch('builtins.print') as mock_print:
            report = self.validator._generate_validation_report()
            
            assert "validation_summary" in report
            assert "validation_results" in report
            assert "timestamp" in report
            
            summary = report["validation_summary"]
            assert summary["total_validations"] == 2
            assert summary["successful_validations"] == 2
            assert summary["success_rate"] == 100.0
            assert summary["final_status"] == "OPTIMAL"
            
            # Check that print was called with success message
            print_calls = [str(call) for call in mock_print.call_args_list]
            success_message = any("ALL VALIDATIONS PASSED" in call for call in print_calls)
            assert success_message
    
    def test_generate_validation_report_partial_pass(self):
        """Test validation report generation with partial pass"""
        # Setup mixed validation results
        self.validator.validation_results = [
            {
                "name": "Test 1",
                "success": True,
                "duration_ms": 100.0,
                "metrics": {},
                "issues": []
            },
            {
                "name": "Test 2",
                "success": False,
                "duration_ms": 200.0,
                "metrics": {},
                "issues": ["Validation failure"],
                "error": "Validation error"
            }
        ]
        
        with patch('builtins.print') as mock_print:
            report = self.validator._generate_validation_report()
            
            summary = report["validation_summary"]
            assert summary["total_validations"] == 2
            assert summary["successful_validations"] == 1
            assert summary["success_rate"] == 50.0
            assert summary["final_status"] == "CRITICAL"  # Below 80%
            
            # Check that print was called with critical message
            print_calls = [str(call) for call in mock_print.call_args_list]
            critical_message = any("VALIDATION FAILURES" in call for call in print_calls)
            assert critical_message
    
    def test_generate_validation_report_mostly_pass(self):
        """Test validation report generation with mostly passing validations"""
        # Setup mostly passing validation results (80%)
        self.validator.validation_results = [
            {"name": "Test 1", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []},
            {"name": "Test 2", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []},
            {"name": "Test 3", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []},
            {"name": "Test 4", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []},
            {"name": "Test 5", "success": False, "duration_ms": 100.0, "metrics": {}, "issues": ["Failure"]}
        ]
        
        with patch('builtins.print') as mock_print:
            report = self.validator._generate_validation_report()
            
            summary = report["validation_summary"]
            assert summary["success_rate"] == 80.0  # 4/5 = 80%
            assert summary["final_status"] == "ACCEPTABLE"
            
            # Check that print was called with acceptable message
            print_calls = [str(call) for call in mock_print.call_args_list]
            acceptable_message = any("MOST VALIDATIONS PASSED" in call for call in print_calls)
            assert acceptable_message
    
    def test_generate_validation_report_mermaid_diagrams(self):
        """Test that validation report includes Mermaid diagrams"""
        self.validator.validation_results = [
            {"name": "Test Validation", "success": True, "duration_ms": 100.0, "metrics": {}, "issues": []}
        ]
        
        with patch('builtins.print') as mock_print:
            self.validator._generate_validation_report()
            
            # Check that Mermaid diagrams were printed
            print_calls = [str(call) for call in mock_print.call_args_list]
            
            mermaid_graph = any("```mermaid" in call and "graph TD" in call for call in print_calls)
            mermaid_pie = any("```mermaid" in call and "pie title" in call for call in print_calls)
            mermaid_timeline = any("```mermaid" in call and "timeline" in call for call in print_calls)
            
            assert mermaid_graph, "Should contain mermaid graph diagram"
            assert mermaid_pie, "Should contain mermaid pie chart"
            assert mermaid_timeline, "Should contain mermaid timeline"
    
    @pytest.mark.asyncio
    async def test_run_comprehensive_validation(self):
        """Test comprehensive validation runner"""
        # Mock all validation methods to return success
        mock_validation_result = {
            "success": True,
            "metrics": {"test_metric": 100},
            "issues": []
        }
        
        validation_methods = [
            '_validate_cns_status',
            '_validate_owl_compiler',
            '_validate_benchmark_system',
            '_validate_performance_monitor',
            '_validate_generated_code',
            '_validate_otel_integration'
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
                    report = await self.validator.run_comprehensive_validation()
                    
                    assert "validation_summary" in report
                    assert len(self.validator.validation_results) == 6  # All 6 validations ran
                    
                    # Verify span attributes were set
                    mock_span_obj.set_attributes.assert_called()
                    
            finally:
                # Stop all patches
                for patch_obj in patches:
                    patch_obj.stop()


class TestAsyncMainFunction:
    """Test main function and async execution"""
    
    @pytest.mark.asyncio
    async def test_main_function_success(self):
        """Test main function successful execution"""
        with patch('validate_otel.OTELValidator') as mock_validator_class:
            mock_validator = Mock()
            mock_validator.run_comprehensive_validation = AsyncMock(return_value={"status": "success"})
            mock_validator_class.return_value = mock_validator
            
            from validate_otel import main
            
            # Should complete without exception
            await main()
            
            mock_validator.run_comprehensive_validation.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_main_function_keyboard_interrupt(self):
        """Test main function with KeyboardInterrupt"""
        with patch('validate_otel.OTELValidator') as mock_validator_class:
            mock_validator = Mock()
            mock_validator.run_comprehensive_validation = AsyncMock(side_effect=KeyboardInterrupt())
            mock_validator_class.return_value = mock_validator
            
            from validate_otel import main
            
            with patch('builtins.print') as mock_print:
                await main()
                
                # Should print keyboard interrupt message
                print_calls = [str(call) for call in mock_print.call_args_list]
                interrupt_message = any("stopped by user" in call for call in print_calls)
                assert interrupt_message
    
    @pytest.mark.asyncio
    async def test_main_function_exception(self):
        """Test main function with general exception"""
        with patch('validate_otel.OTELValidator') as mock_validator_class:
            mock_validator = Mock()
            mock_validator.run_comprehensive_validation = AsyncMock(side_effect=RuntimeError("Test error"))
            mock_validator_class.return_value = mock_validator
            
            from validate_otel import main
            
            with patch('builtins.print') as mock_print:
                with pytest.raises(RuntimeError):
                    await main()
                
                # Should print error message
                print_calls = [str(call) for call in mock_print.call_args_list]
                error_message = any("failed with error" in call for call in print_calls)
                assert error_message


class TestErrorHandlingAndEdgeCases:
    """Test error handling scenarios and edge cases"""
    
    def setup_method(self):
        """Setup for each test method"""
        with patch('validate_otel.PeriodicExportingMetricReader'):
            with patch('validate_otel.MeterProvider'):
                with patch('validate_otel.TracerProvider'):
                    with patch('validate_otel.BatchSpanProcessor'):
                        with patch('validate_otel.metrics.set_meter_provider'):
                            with patch('validate_otel.trace.set_tracer_provider'):
                                self.validator = OTELValidator()
    
    @pytest.mark.asyncio
    async def test_validation_with_subprocess_exception(self):
        """Test validation with subprocess exceptions"""
        with patch('validate_otel.asyncio.create_subprocess_exec', side_effect=OSError("Process creation failed")):
            result = await self.validator._validate_cns_status()
            
            assert result["success"] is False
            assert any("CNS status validation error" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_validation_with_json_decode_edge_cases(self):
        """Test JSON decoding with various edge cases"""
        edge_case_jsons = [
            b'{"incomplete": ',  # Incomplete JSON
            b'{"invalid": "json"',  # Missing closing brace
            b'null',  # Valid JSON but null
            b'[]',  # Valid JSON but array instead of object
            b'{"nested": {"very": {"deep": {"object": "value"}}}}',  # Very nested
            b'""',  # Empty string
            b'{',  # Just opening brace
        ]
        
        for json_bytes in edge_case_jsons:
            mock_process = Mock()
            mock_process.communicate = AsyncMock(return_value=(json_bytes, b""))
            mock_process.returncode = 0
            
            with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
                result = await self.validator._validate_cns_status()
                
                # Should handle all cases gracefully
                assert result["success"] is False
                if json_bytes not in [b'{"nested": {"very": {"deep": {"object": "value"}}}}']:
                    # The nested JSON should still fail validation due to missing fields
                    assert any("not valid JSON" in issue or "Missing required field" in issue 
                             for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_file_system_operations_errors(self):
        """Test file system operation errors during validation"""
        # Mock compilation success but file system errors
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(b"Compilation successful", b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            with patch('validate_otel.Path') as mock_path:
                # Mock Path operations that raise exceptions
                mock_path.side_effect = PermissionError("Permission denied")
                
                result = await self.validator._validate_owl_compiler()
                
                assert result["success"] is False
                assert any("OWL compiler validation error" in issue for issue in result["issues"])
    
    @pytest.mark.asyncio
    async def test_concurrent_validations(self):
        """Test running multiple validations concurrently"""
        # Create multiple validation tasks
        tasks = []
        for i in range(3):
            validator = OTELValidator()
            task = validator._validate_otel_integration()
            tasks.append(task)
        
        # Run concurrently
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # All should complete (successfully or with handled exceptions)
        assert len(results) == 3
        for result in results:
            if isinstance(result, Exception):
                # If there's an exception, it should be handled gracefully
                assert isinstance(result, (RuntimeError, ValueError, OSError))
            else:
                # If successful, should have expected structure
                assert "success" in result
    
    def test_telemetry_setup_failure_handling(self):
        """Test handling of telemetry setup failures"""
        with patch('validate_otel.PeriodicExportingMetricReader', side_effect=Exception("Telemetry setup failed")):
            # Should handle telemetry setup failure gracefully
            try:
                validator = OTELValidator()
                # If it gets created, it should have some fallback state
                assert hasattr(validator, 'validation_results')
            except Exception as e:
                # If it fails, should be a reasonable exception
                assert "Telemetry setup failed" in str(e)
    
    @pytest.mark.asyncio
    async def test_large_output_handling(self):
        """Test handling of very large subprocess outputs"""
        # Create very large output (1MB)
        large_output = "Test output line\n" * 50000  # ~1MB
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(large_output.encode(), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_benchmark_system()
            
            # Should handle large output without memory issues
            assert "success" in result
            # Even if validation fails due to missing components,
            # it should not fail due to memory/size issues
    
    @pytest.mark.asyncio
    async def test_validation_with_unicode_output(self):
        """Test validation with Unicode characters in output"""
        unicode_output = """
        🔬 CNS BENCHMARK REPORT
        ================================
        Performance Score: 90.5/100 ✅
        
        Status: OPTIMAL 🎉
        
        📊 OTEL Metrics:
        - benchmark_duration_ms: 250.0
        - performance_score: 90.5
        
        ```mermaid
        graph TD
            A[测试] --> B[结果]
        ```
        
        ```mermaid
        pie title Résultats
        ```
        
        ```mermaid
        timeline
        ```
        """
        
        mock_process = Mock()
        mock_process.communicate = AsyncMock(return_value=(unicode_output.encode('utf-8'), b""))
        mock_process.returncode = 0
        
        with patch('validate_otel.asyncio.create_subprocess_exec', return_value=mock_process):
            result = await self.validator._validate_benchmark_system()
            
            # Should handle Unicode characters properly
            assert result["success"] is True
            assert result["metrics"]["performance_score"] == 90.5


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])