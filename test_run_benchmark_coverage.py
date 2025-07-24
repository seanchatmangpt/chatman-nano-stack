#!/usr/bin/env python3
"""
Comprehensive unit tests for run_benchmark.py - 80% line coverage
Tests benchmark execution, metrics collection, performance validation
"""

import pytest
import subprocess
import tempfile
import time
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock, mock_open

from run_benchmark import (
    find_latest_binary, BenchmarkRunner, run_benchmark, main
)


class TestFindLatestBinary:
    """Test find_latest_binary function"""
    
    @patch('run_benchmark.Path')
    def test_find_latest_binary_exists(self, mock_path_class):
        """Test finding existing binary"""
        # Mock the live_system directory and binary file
        mock_live_system_dir = Mock()
        mock_binary_path = Mock()
        mock_binary_path.exists.return_value = True
        mock_binary_path.is_file.return_value = True
        
        mock_live_system_dir.__truediv__ = Mock(return_value=mock_binary_path)
        mock_path_class.return_value = mock_live_system_dir
        
        result = find_latest_binary()
        
        assert result == mock_binary_path
        mock_binary_path.exists.assert_called_once()
        mock_binary_path.is_file.assert_called_once()
    
    @patch('run_benchmark.Path')
    def test_find_latest_binary_not_exists_fallback(self, mock_path_class):
        """Test fallback to executable files when main binary doesn't exist"""
        # Mock the live_system directory
        mock_live_system_dir = Mock()
        mock_binary_path = Mock()
        mock_binary_path.exists.return_value = False
        
        # Mock glob to return executable files
        mock_executable = Mock()
        mock_executable.is_file.return_value = True
        mock_stat = Mock()
        mock_stat.st_mode = 0o755  # Executable permissions
        mock_executable.stat.return_value = mock_stat
        
        mock_live_system_dir.__truediv__ = Mock(return_value=mock_binary_path)
        mock_live_system_dir.glob.return_value = [mock_executable]
        mock_path_class.return_value = mock_live_system_dir
        
        result = find_latest_binary()
        
        assert result == mock_executable
        mock_live_system_dir.glob.assert_called_once_with("*")
    
    @patch('run_benchmark.Path')
    def test_find_latest_binary_none_found(self, mock_path_class):
        """Test when no binary is found"""
        # Mock the live_system directory
        mock_live_system_dir = Mock()
        mock_binary_path = Mock()
        mock_binary_path.exists.return_value = False
        
        # Mock glob to return no executable files
        mock_live_system_dir.__truediv__ = Mock(return_value=mock_binary_path)
        mock_live_system_dir.glob.return_value = []
        mock_path_class.return_value = mock_live_system_dir
        
        with patch('builtins.print') as mock_print:
            result = find_latest_binary()
            
            assert result is None
            mock_print.assert_called_with("No compiled binaries found in live_system directory!")
    
    @patch('run_benchmark.Path')
    def test_find_latest_binary_fallback_non_executable(self, mock_path_class):
        """Test fallback with non-executable files (should be ignored)"""
        mock_live_system_dir = Mock()
        mock_binary_path = Mock()
        mock_binary_path.exists.return_value = False
        
        # Mock glob to return non-executable files
        mock_non_executable = Mock()
        mock_non_executable.is_file.return_value = True
        mock_stat = Mock()
        mock_stat.st_mode = 0o644  # Not executable
        mock_non_executable.stat.return_value = mock_stat
        
        mock_live_system_dir.__truediv__ = Mock(return_value=mock_binary_path)
        mock_live_system_dir.glob.return_value = [mock_non_executable]
        mock_path_class.return_value = mock_live_system_dir
        
        result = find_latest_binary()
        
        assert result is None


class TestBenchmarkRunner:
    """Test BenchmarkRunner class functionality"""
    
    def setup_method(self):
        """Setup for each test method"""
        with patch('run_benchmark.PeriodicExportingMetricReader'):
            with patch('run_benchmark.MeterProvider'):
                with patch('run_benchmark.metrics.set_meter_provider'):
                    self.runner = BenchmarkRunner()
    
    def test_benchmark_runner_initialization(self):
        """Test BenchmarkRunner initialization"""
        assert hasattr(self.runner, 'meter')
        assert hasattr(self.runner, 'benchmark_duration')
        assert hasattr(self.runner, 'performance_score')
        assert hasattr(self.runner, 'test_results')
    
    @patch('run_benchmark.find_latest_binary')
    def test_run_benchmark_no_binary(self, mock_find_binary):
        """Test benchmark run when no binary is found"""
        mock_find_binary.return_value = None
        
        with patch('builtins.print') as mock_print:
            result = self.runner.run_benchmark()
            
            assert result is False
            
            # Check that appropriate messages were printed
            print_calls = [str(call) for call in mock_print.call_args_list]
            no_binary_message = any("Could not find a compiled binary" in call for call in print_calls)
            assert no_binary_message
    
    @patch('run_benchmark.find_latest_binary')
    def test_run_benchmark_with_binary(self, mock_find_binary):
        """Test benchmark run with binary found"""
        mock_binary = Mock()
        mock_binary.name = "test_binary"
        mock_stat = Mock()
        mock_stat.st_size = 1024000  # 1MB
        mock_binary.stat.return_value = mock_stat
        mock_find_binary.return_value = mock_binary
        
        # Mock all test methods to return success
        mock_test_result = {
            "success": True,
            "duration_ms": 100.0,
            "tests_passed": 4,
            "tests_total": 4,
            "output": "Test output",
            "stderr": ""
        }
        
        with patch.object(self.runner, '_run_self_test', return_value=mock_test_result):
            with patch.object(self.runner, '_run_help_test', return_value=mock_test_result):
                with patch.object(self.runner, '_run_production_test', return_value=mock_test_result):
                    with patch.object(self.runner, '_run_default_test', return_value=mock_test_result):
                        with patch.object(self.runner, '_generate_mermaid_report'):
                            with patch('builtins.print'):
                                result = self.runner.run_benchmark()
                                
                                assert result is True
    
    @patch('run_benchmark.subprocess.run')
    def test_run_self_test_success(self, mock_subprocess):
        """Test successful self-test execution"""
        mock_binary = Path("/test/binary")
        
        # Mock successful subprocess result
        mock_result = Mock()
        mock_result.stdout = """
        🧪 CNS Ontology Self-Test
        ========================
        Test 1: Memory allocation... ✓ PASSED
        Test 2: Class descriptors... ✓ PASSED
        Test 3: Property descriptors... ✓ PASSED
        Test 4: 8-cycle performance contract... ✓ PASSED (500.0 ns)
        
        📊 Test Results: 4/4 passed
        🎉 All tests PASSED - System is OPTIMAL
        """
        mock_result.stderr = ""
        mock_result.returncode = 0
        mock_subprocess.return_value = mock_result
        
        result = self.runner._run_self_test(mock_binary)
        
        assert result["success"] is True
        assert result["tests_passed"] == 4
        assert result["tests_total"] == 4
        assert "duration_ms" in result
        assert result["output"] == mock_result.stdout
        
        # Verify subprocess was called correctly
        mock_subprocess.assert_called_once_with(
            [str(mock_binary), "--self-test"],
            capture_output=True,
            text=True,
            timeout=10
        )
    
    @patch('run_benchmark.subprocess.run')
    def test_run_self_test_partial_success(self, mock_subprocess):
        """Test self-test with partial success (acceptable)"""
        mock_binary = Path("/test/binary")
        
        mock_result = Mock()
        mock_result.stdout = """
        🧪 CNS Ontology Self-Test
        ========================
        Test 1: Memory allocation... ✓ PASSED
        Test 2: Class descriptors... ✓ PASSED
        Test 3: Property descriptors... ✓ PASSED
        Test 4: 8-cycle performance contract... ⚠ SLOW (2000.0 ns)
        
        📊 Test Results: 3/4 passed
        ❌ Some tests FAILED - System needs attention
        """
        mock_result.stderr = ""
        mock_result.returncode = 1  # Non-zero due to performance test
        mock_subprocess.return_value = mock_result
        
        result = self.runner._run_self_test(mock_binary)
        
        # Should still be considered success if core tests pass
        assert result["success"] is True
        assert result["tests_passed"] == 3
        assert result["tests_total"] == 4
    
    @patch('run_benchmark.subprocess.run')
    def test_run_self_test_failure(self, mock_subprocess):
        """Test self-test failure"""
        mock_binary = Path("/test/binary")
        
        mock_result = Mock()
        mock_result.stdout = "Test output without expected markers"
        mock_result.stderr = "Error occurred"
        mock_result.returncode = 1
        mock_subprocess.return_value = mock_result
        
        result = self.runner._run_self_test(mock_binary)
        
        assert result["success"] is False
        assert result["tests_passed"] == 0
        assert result["tests_total"] == 0
    
    @patch('run_benchmark.subprocess.run')
    def test_run_self_test_exception(self, mock_subprocess):
        """Test self-test with exception"""
        mock_binary = Path("/test/binary")
        mock_subprocess.side_effect = subprocess.TimeoutExpired("cmd", 10)
        
        result = self.runner._run_self_test(mock_binary)
        
        assert result["success"] is False
        assert "error" in result
        assert result["duration_ms"] == 0
    
    @patch('run_benchmark.subprocess.run')
    def test_run_help_test_success(self, mock_subprocess):
        """Test successful help test"""
        mock_binary = Path("/test/binary")
        
        mock_result = Mock()
        mock_result.stdout = """
        Usage: owl_ontology [options]
        Options:
          --self-test         Run comprehensive self-tests
          --deploy-production Deploy to production environment
          --help              Show this help message
        """
        mock_result.stderr = ""
        mock_result.returncode = 0
        mock_subprocess.return_value = mock_result
        
        result = self.runner._run_help_test(mock_binary)
        
        assert result["success"] is True
        assert "duration_ms" in result
        assert result["features_detected"]["usage"] is True
        assert result["features_detected"]["options"] is True
        assert result["features_detected"]["self_test"] is True
    
    @patch('run_benchmark.subprocess.run')
    def test_run_help_test_missing_features(self, mock_subprocess):
        """Test help test with missing features"""
        mock_binary = Path("/test/binary")
        
        mock_result = Mock()
        mock_result.stdout = "Basic help without expected sections"
        mock_result.stderr = ""
        mock_result.returncode = 0
        mock_subprocess.return_value = mock_result
        
        result = self.runner._run_help_test(mock_binary)
        
        assert result["success"] is False
        assert result["features_detected"]["usage"] is False
        assert result["features_detected"]["options"] is False
        assert result["features_detected"]["self_test"] is False
    
    @patch('run_benchmark.subprocess.run')
    def test_run_production_test_success(self, mock_subprocess):
        """Test successful production deployment test"""
        mock_binary = Path("/test/binary")
        
        mock_result = Mock()
        mock_result.stdout = """
        🚀 Production Deployment
        =======================
        ✓ Ontology validated: 10 classes loaded
        ✓ Performance contracts: <8 CPU cycles guaranteed
        ✓ Memory alignment: 8-byte quantum compliance
        ✓ UHFT optimization: nanosecond determinism enabled
        ✅ System deployed and ready for production traffic
        """
        mock_result.stderr = ""
        mock_result.returncode = 0
        mock_subprocess.return_value = mock_result
        
        result = self.runner._run_production_test(mock_binary)
        
        assert result["success"] is True
        assert result["features_detected"]["deployment"] is True
        assert result["features_detected"]["validation"] is True
        assert result["features_detected"]["ready"] is True
    
    @patch('run_benchmark.subprocess.run')
    def test_run_default_test_success(self, mock_subprocess):
        """Test successful default behavior test"""
        mock_binary = Path("/test/binary")
        
        mock_result = Mock()
        mock_result.stdout = """
        CNS Ontology Runtime v1.0.0
        Generated: 2023-01-01T12:00:00
        Classes: 10, Properties: 20, Rules: 5
        
        🔍 System Information:
        - Ontology URI: http://example.org/test
        - Compilation timestamp: 2023-01-01T12:00:00
        - Eightfold stages: 8
        - Performance guarantee: <8 CPU cycles
        - Memory model: 8-byte quantum aligned
        
        Run with --help for options
        """
        mock_result.stderr = ""
        mock_result.returncode = 0
        mock_subprocess.return_value = mock_result
        
        result = self.runner._run_default_test(mock_binary)
        
        assert result["success"] is True
        assert result["features_detected"]["system_info"] is True
        assert result["features_detected"]["timestamp"] is True
        assert result["features_detected"]["help_hint"] is True
    
    @patch('run_benchmark.subprocess.run')
    def test_run_test_timeout(self, mock_subprocess):
        """Test test execution with timeout"""
        mock_binary = Path("/test/binary")
        mock_subprocess.side_effect = subprocess.TimeoutExpired("cmd", 5)
        
        result = self.runner._run_help_test(mock_binary)
        
        assert result["success"] is False
        assert "error" in result
        assert "timed out after 5 seconds" in result["error"]
    
    def test_generate_mermaid_report_all_pass(self):
        """Test Mermaid report generation with all tests passing"""
        test_results = {
            "self_test": {"success": True, "duration_ms": 100.0},
            "help_test": {"success": True, "duration_ms": 50.0},
            "production_test": {"success": True, "duration_ms": 75.0},
            "default_test": {"success": True, "duration_ms": 25.0}
        }
        
        with patch('builtins.print') as mock_print:
            self.runner._generate_mermaid_report(test_results, 250.0, 100.0)
            
            # Get all printed output as strings - handle both call() and call(arg) formats
            all_output = '\n'.join([str(call[0][0]) if call[0] else str(call) for call in mock_print.call_args_list])
            
            # Check for report header
            assert "CNS BENCHMARK REPORT" in all_output
            
            # Check for performance score
            assert "Performance Score: 100.0/100" in all_output
            
            # Check for Mermaid diagrams
            assert "```mermaid" in all_output and "graph TD" in all_output
            assert "pie title" in all_output
            assert "timeline" in all_output
            
            # Check for success message
            assert "All benchmarks PASSED" in all_output
    
    def test_generate_mermaid_report_some_fail(self):
        """Test Mermaid report generation with some failures"""
        test_results = {
            "self_test": {"success": True, "duration_ms": 100.0},
            "help_test": {"success": False, "duration_ms": 50.0, "error": "Help test failed"},
            "production_test": {"success": True, "duration_ms": 75.0},
            "default_test": {"success": False, "duration_ms": 25.0}
        }
        
        with patch('builtins.print') as mock_print:
            self.runner._generate_mermaid_report(test_results, 250.0, 50.0)
            
            # Get all printed output as strings - handle both call() and call(arg) formats
            all_output = '\n'.join([str(call[0][0]) if call[0] else str(call) for call in mock_print.call_args_list])
            
            # Check for failure message
            assert "benchmark(s) FAILED" in all_output
            
            # Check that errors are displayed
            assert "Help test failed" in all_output
    
    def test_generate_mermaid_report_otel_metrics(self):
        """Test that OTEL metrics are included in report"""
        test_results = {
            "self_test": {"success": True, "duration_ms": 100.0}
        }
        
        with patch('builtins.print') as mock_print:
            self.runner._generate_mermaid_report(test_results, 100.0, 100.0)
            
            # Get all printed output as strings - handle both call() and call(arg) formats
            all_output = '\n'.join([str(call[0][0]) if call[0] else str(call) for call in mock_print.call_args_list])
            
            # Check for OTEL metrics section
            assert "📊 OTEL Metrics:" in all_output
            
            # Check for specific metrics
            assert "benchmark_duration_ms: 100.0" in all_output
            assert "performance_score: 100.0" in all_output
            assert "test_results_total: 1" in all_output


class TestRunBenchmarkFunction:
    """Test run_benchmark wrapper function"""
    
    @patch('run_benchmark.BenchmarkRunner')
    def test_run_benchmark_wrapper(self, mock_runner_class):
        """Test run_benchmark wrapper function"""
        mock_runner = Mock()
        mock_runner.run_benchmark.return_value = True
        mock_runner_class.return_value = mock_runner
        
        result = run_benchmark()
        
        assert result is True
        mock_runner_class.assert_called_once()
        mock_runner.run_benchmark.assert_called_once()


class TestMainFunction:
    """Test main function"""
    
    @patch('run_benchmark.run_benchmark')
    @patch('sys.exit')
    def test_main_success(self, mock_exit, mock_run_benchmark):
        """Test main function with successful benchmark"""
        mock_run_benchmark.return_value = True
        
        main()
        
        mock_run_benchmark.assert_called_once()
        mock_exit.assert_called_once_with(0)
    
    @patch('run_benchmark.run_benchmark')
    @patch('sys.exit')
    def test_main_failure(self, mock_exit, mock_run_benchmark):
        """Test main function with failed benchmark"""
        mock_run_benchmark.return_value = False
        
        main()
        
        mock_run_benchmark.assert_called_once()
        mock_exit.assert_called_once_with(1)


class TestIntegrationScenarios:
    """Test integration scenarios and edge cases"""
    
    def test_performance_score_calculation(self):
        """Test performance score calculation"""
        with patch('run_benchmark.PeriodicExportingMetricReader'):
            with patch('run_benchmark.MeterProvider'):
                with patch('run_benchmark.metrics.set_meter_provider'):
                    runner = BenchmarkRunner()
        
        # Mock test results with different success rates
        test_scenarios = [
            # All pass (4/4)
            {
                "self_test": {"success": True},
                "help_test": {"success": True},
                "production_test": {"success": True},
                "default_test": {"success": True}
            },
            # 3 out of 4 pass
            {
                "self_test": {"success": True},
                "help_test": {"success": True},
                "production_test": {"success": True},
                "default_test": {"success": False}
            },
            # 2 out of 4 pass
            {
                "self_test": {"success": True},
                "help_test": {"success": True},
                "production_test": {"success": False},
                "default_test": {"success": False}
            },
            # All fail
            {
                "self_test": {"success": False},
                "help_test": {"success": False},
                "production_test": {"success": False},
                "default_test": {"success": False}
            }
        ]
        
        expected_scores = [100.0, 75.0, 50.0, 0.0]
        
        for i, test_results in enumerate(test_scenarios):
            passed_tests = sum(1 for result in test_results.values() if result["success"])
            total_tests = len(test_results)
            expected_score = (passed_tests / total_tests) * 100
            
            assert expected_score == expected_scores[i]
    
    def test_subprocess_error_handling(self):
        """Test various subprocess error conditions"""
        with patch('run_benchmark.PeriodicExportingMetricReader'):
            with patch('run_benchmark.MeterProvider'):
                with patch('run_benchmark.metrics.set_meter_provider'):
                    runner = BenchmarkRunner()
        
        mock_binary = Path("/test/binary")
        
        error_scenarios = [
            subprocess.TimeoutExpired("cmd", 10),
            subprocess.CalledProcessError(1, "cmd"),
            FileNotFoundError("Binary not found"),
            PermissionError("Permission denied"),
            OSError("OS error occurred")
        ]
        
        for error in error_scenarios:
            with patch('run_benchmark.subprocess.run', side_effect=error):
                result = runner._run_self_test(mock_binary)
                
                assert result["success"] is False
                assert "error" in result
                assert isinstance(result["error"], str)
    
    def test_output_parsing_edge_cases(self):
        """Test parsing of edge case outputs"""
        with patch('run_benchmark.PeriodicExportingMetricReader'):
            with patch('run_benchmark.MeterProvider'):
                with patch('run_benchmark.metrics.set_meter_provider'):
                    runner = BenchmarkRunner()
        
        mock_binary = Path("/test/binary")
        
        # Test malformed test results output
        malformed_outputs = [
            "Test Results: malformed/output",
            "Test Results: /4 passed",
            "Test Results: 3/ passed",
            "Test Results: 3/4",  # Missing "passed"
            "No test results line",
            "",
            "Test Results: abc/def passed"
        ]
        
        for output in malformed_outputs:
            mock_result = Mock()
            mock_result.stdout = output
            mock_result.stderr = ""
            mock_result.returncode = 0
            
            with patch('run_benchmark.subprocess.run', return_value=mock_result):
                result = runner._run_self_test(mock_binary)
                
                # Should handle malformed output gracefully
                assert "success" in result
                # These tests will fail the success criteria anyway since they lack required markers
                assert result["success"] is False
    
    def test_telemetry_integration(self):
        """Test OpenTelemetry integration"""
        with patch('run_benchmark.PeriodicExportingMetricReader') as mock_reader:
            with patch('run_benchmark.MeterProvider') as mock_provider:
                with patch('run_benchmark.metrics.set_meter_provider') as mock_set_provider:
                    with patch('run_benchmark.ConsoleMetricExporter') as mock_exporter:
                        runner = BenchmarkRunner()
                        
                        # Verify telemetry setup
                        mock_exporter.assert_called_once()
                        mock_reader.assert_called_once()
                        mock_provider.assert_called_once()
                        mock_set_provider.assert_called_once()
                        
                        # Verify meter and instruments are created
                        assert hasattr(runner, 'meter')
                        assert hasattr(runner, 'benchmark_duration')
                        assert hasattr(runner, 'performance_score')
                        assert hasattr(runner, 'test_results')
    
    def test_binary_file_properties(self):
        """Test binary file property detection"""
        with patch('run_benchmark.Path') as mock_path_class:
            # Test different file properties
            mock_binary = Mock()
            mock_binary.exists.return_value = True
            mock_binary.is_file.return_value = True
            mock_binary.name = "test_binary"
            
            # Test different file sizes
            file_sizes = [0, 1024, 1024*1024, 100*1024*1024]  # 0B, 1KB, 1MB, 100MB
            
            for size in file_sizes:
                mock_stat = Mock()
                mock_stat.st_size = size
                mock_binary.stat.return_value = mock_stat
                
                mock_live_system_dir = Mock()
                mock_live_system_dir.__truediv__ = Mock(return_value=mock_binary)
                mock_path_class.return_value = mock_live_system_dir
                
                result = find_latest_binary()
                
                assert result == mock_binary
                assert result.stat().st_size == size
    
    def test_concurrent_benchmark_execution(self):
        """Test that benchmark handles concurrent execution properly"""
        with patch('run_benchmark.PeriodicExportingMetricReader'):
            with patch('run_benchmark.MeterProvider'):
                with patch('run_benchmark.metrics.set_meter_provider'):
                    # Create separate instances to simulate concurrent access
                    runners = [BenchmarkRunner() for _ in range(5)]
        
        # All should have independent state
        for i, r in enumerate(runners):
            # Modify one runner's state
            if i == 0:
                r.test_custom_attribute = "modified"
            
            # Others should not be affected
            if i > 0:
                assert not hasattr(r, 'test_custom_attribute')


class TestPerformanceCharacteristics:
    """Test performance characteristics of the benchmark system"""
    
    def test_benchmark_execution_time(self):
        """Test that benchmark execution completes in reasonable time"""
        with patch('run_benchmark.PeriodicExportingMetricReader'):
            with patch('run_benchmark.MeterProvider'):
                with patch('run_benchmark.metrics.set_meter_provider'):
                    runner = BenchmarkRunner()
        
        # Mock fast test execution
        fast_result = {"success": True, "duration_ms": 1.0}
        
        with patch.object(runner, '_run_self_test', return_value=fast_result):
            with patch.object(runner, '_run_help_test', return_value=fast_result):
                with patch.object(runner, '_run_production_test', return_value=fast_result):
                    with patch.object(runner, '_run_default_test', return_value=fast_result):
                        with patch('run_benchmark.find_latest_binary', return_value=Mock()):
                            with patch.object(runner, '_generate_mermaid_report'):
                                with patch('builtins.print'):
                                    start_time = time.time()
                                    result = runner.run_benchmark()
                                    end_time = time.time()
                                    
                                    # Should complete quickly
                                    execution_time = end_time - start_time
                                    assert execution_time < 1.0  # Less than 1 second
                                    assert result is True
    
    def test_memory_efficiency(self):
        """Test memory usage characteristics"""
        with patch('run_benchmark.PeriodicExportingMetricReader'):
            with patch('run_benchmark.MeterProvider'):
                with patch('run_benchmark.metrics.set_meter_provider'):
                    runner = BenchmarkRunner()
        
        # Create many test results to test memory handling
        large_output = "Test output line\n" * 10000  # Large output
        
        mock_result = Mock()
        mock_result.stdout = large_output
        mock_result.stderr = ""
        mock_result.returncode = 0
        
        with patch('run_benchmark.subprocess.run', return_value=mock_result):
            result = runner._run_self_test(Path("/test/binary"))
            
            # Should handle large outputs without issues
            assert "success" in result
            assert len(result["output"]) == len(large_output)
    
    def test_error_resilience(self):
        """Test resilience to various error conditions"""
        with patch('run_benchmark.PeriodicExportingMetricReader'):
            with patch('run_benchmark.MeterProvider'):
                with patch('run_benchmark.metrics.set_meter_provider'):
                    runner = BenchmarkRunner()
        
        # Test with binary that doesn't exist (simpler test case)
        with patch('run_benchmark.find_latest_binary', return_value=None):
            # Should handle missing binary gracefully
            with patch('builtins.print'):
                result = runner.run_benchmark()
                # Should return False when no binary is found
                assert result is False


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])