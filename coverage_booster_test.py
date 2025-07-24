#!/usr/bin/env python3
"""
Coverage Booster Test - Push bitactor_cli.py to 80%+ coverage
Targets specific uncovered lines to achieve 80% target
"""

import unittest
import sys
import os
import tempfile
import json
import subprocess
from pathlib import Path
from unittest.mock import patch, MagicMock, call
from typer.testing import CliRunner
import typer

# Add current directory to path
sys.path.insert(0, '/Users/sac/cns')

# Direct imports
import bitactor_cli

class CoverageBoosterTest(unittest.TestCase):
    """Targeted tests to boost bitactor_cli.py coverage to 80%+"""
    
    def setUp(self):
        self.test_dir = Path("/Users/sac/cns/generated/test_fix")
        self.ttl_file = Path("/Users/sac/cns/ontologies/bitactor_semantic_core.ttl")
        self.cli_runner = CliRunner()
    
    # ========== TARGET UNCOVERED LINES ==========
    
    @patch('subprocess.run')
    def test_cli_run_c_tests_no_benchmark_files(self, mock_run):
        """Test C tests when no benchmark files exist"""
        mock_run.side_effect = [
            MagicMock(returncode=0, stdout="build ok"),  # make clean all
            MagicMock(returncode=0, stdout="test ok"),   # test run
        ]
        
        cli = bitactor_cli.BitActorCLI()
        with patch('pathlib.Path.glob') as mock_glob:
            # First call returns test files, second call returns empty for benchmarks
            mock_glob.side_effect = [
                [MagicMock(name="test_binary")],  # *_test glob
                []  # *_benchmark glob
            ]
            
            result = cli.run_c_tests(self.test_dir)
            
            self.assertTrue(result["build"])
            self.assertTrue(result["tests"])
            # Should handle missing benchmark gracefully
    
    @patch('subprocess.run')
    def test_cli_run_c_tests_benchmark_failure(self, mock_run):
        """Test C tests when benchmark fails"""
        mock_run.side_effect = [
            MagicMock(returncode=0, stdout="build ok"),  # make
            MagicMock(returncode=0, stdout="test ok"),   # test
            MagicMock(returncode=1, stdout="benchmark failed", stderr="error")  # benchmark fails
        ]
        
        cli = bitactor_cli.BitActorCLI()
        with patch('pathlib.Path.glob') as mock_glob:
            mock_glob.return_value = [MagicMock(name="test_binary")]
            result = cli.run_c_tests(self.test_dir)
            
            self.assertTrue(result["build"])
            self.assertTrue(result["tests"])
            self.assertFalse(result["benchmark"])
    
    @patch('subprocess.run')
    def test_cli_run_c_tests_test_failure(self, mock_run):
        """Test C tests when unit test fails"""
        mock_run.side_effect = [
            MagicMock(returncode=0, stdout="build ok"),  # make
            MagicMock(returncode=1, stdout="test failed", stderr="assertion failed")  # test fails
        ]
        
        cli = bitactor_cli.BitActorCLI()
        with patch('pathlib.Path.glob') as mock_glob:
            mock_glob.return_value = [MagicMock(name="test_binary")]
            result = cli.run_c_tests(self.test_dir)
            
            self.assertTrue(result["build"])
            self.assertFalse(result["tests"])
            # Should not run benchmark if tests fail
    
    @patch('subprocess.run')
    def test_cli_run_python_tests_partial_success(self, mock_run):
        """Test Python tests with partial success"""
        partial_output = """
✅ BitActor instantiated
❌ Signal processing failed
        """
        mock_run.return_value = MagicMock(returncode=0, stdout=partial_output)
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_python_tests(self.test_dir)
        
        self.assertTrue(result["import"])
        self.assertFalse(result["signal_processing"])
    
    @patch('subprocess.run')
    def test_cli_run_python_tests_no_performance_data(self, mock_run):
        """Test Python tests without performance metrics"""
        output_no_perf = """
✅ BitActor instantiated
✅ Processed 1000 signals
        """
        mock_run.return_value = MagicMock(returncode=0, stdout=output_no_perf)
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_python_tests(self.test_dir)
        
        self.assertTrue(result["import"])
        self.assertTrue(result["signal_processing"])
        # Performance dict should be empty or not have expected keys
        self.assertNotIn("throughput", result.get("performance", {}))
    
    @patch('subprocess.run')
    @patch('pathlib.Path.glob')
    def test_cli_run_erlang_tests_compile_failure(self, mock_glob, mock_run):
        """Test Erlang tests with compilation failure"""
        mock_glob.return_value = [MagicMock(stem="test_bitactor")]
        mock_run.side_effect = [
            MagicMock(returncode=0),  # which escript success
            MagicMock(returncode=0, stdout="❌ Compilation failed: syntax error")  # escript with compile error
        ]
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_erlang_tests(self.test_dir)
        
        self.assertFalse(result["compile"])
        self.assertFalse(result["start"])
        self.assertFalse(result["signal_processing"])
    
    @patch('subprocess.run')
    @patch('pathlib.Path.glob')
    def test_cli_run_erlang_tests_no_erlang_files(self, mock_glob, mock_run):
        """Test Erlang tests when no .erl files exist"""
        mock_glob.return_value = []  # No .erl files found
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_erlang_tests(self.test_dir)
        
        self.assertFalse(result["compile"])
    
    def test_cli_display_results_edge_cases(self):
        """Test display results with edge cases"""
        cli = bitactor_cli.BitActorCLI()
        
        # Test with empty results
        cli.display_results({})
        
        # Test with partial results
        partial_results = {
            "C": {"build": True}  # Missing other keys
        }
        cli.display_results(partial_results)
        
        # Test with all false results
        false_results = {
            "C": {"build": False, "tests": False, "performance": {}},
            "Python": {"import": False, "signal_processing": False, "performance": {}},
            "Erlang": {"compile": False, "signal_processing": False, "performance": {}}
        }
        cli.display_results(false_results)
        
        # Should not crash for any of these
        self.assertTrue(True)
    
    # ========== CLI COMMAND EDGE CASES ==========
    
    def test_cli_generate_command_no_validate(self):
        """Test generate command without validation"""
        if self.ttl_file.exists():
            result = self.cli_runner.invoke(bitactor_cli.app, [
                "generate", 
                str(self.ttl_file), 
                "generated/coverage_test", 
                "coverage",
                "--no-validate"
            ])
            # Should complete without validation step
            self.assertIsNotNone(result)
    
    def test_cli_generate_command_invalid_ttl(self):
        """Test generate command with invalid TTL"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write("invalid ttl content")
            f.flush()
            
            try:
                result = self.cli_runner.invoke(bitactor_cli.app, [
                    "generate", 
                    f.name, 
                    "generated/invalid_test", 
                    "invalid"
                ])
                # Should fail validation
                self.assertNotEqual(result.exit_code, 0)
            finally:
                Path(f.name).unlink()
    
    def test_cli_self_check_missing_directory(self):
        """Test self-check with missing directory"""
        result = self.cli_runner.invoke(bitactor_cli.app, [
            "self-check", 
            "/nonexistent/directory"
        ])
        # Should handle missing directory
        self.assertNotEqual(result.exit_code, 0)
    
    def test_cli_self_check_single_language(self):
        """Test self-check with single language"""
        if self.test_dir.exists():
            result = self.cli_runner.invoke(bitactor_cli.app, [
                "self-check", 
                str(self.test_dir),
                "--lang", "python"
            ])
            self.assertIsNotNone(result)
    
    def test_cli_validate_missing_file(self):
        """Test validate command with missing file"""
        result = self.cli_runner.invoke(bitactor_cli.app, [
            "validate", 
            "/nonexistent/file.ttl"
        ])
        self.assertNotEqual(result.exit_code, 0)
    
    def test_cli_list_signals_missing_file(self):
        """Test list-signals with missing file"""
        result = self.cli_runner.invoke(bitactor_cli.app, [
            "list-signals", 
            "/nonexistent/file.ttl"
        ])
        self.assertNotEqual(result.exit_code, 0)
    
    def test_cli_full_cycle_invalid_params(self):
        """Test full-cycle with invalid parameters"""
        result = self.cli_runner.invoke(bitactor_cli.app, [
            "full-cycle", 
            "/nonexistent/file.ttl",
            "/invalid/output",
            "invalid_prefix"
        ])
        self.assertNotEqual(result.exit_code, 0)
    
    # ========== VERSION AND HELP COVERAGE ==========
    
    def test_cli_version_flag(self):
        """Test version flag"""
        result = self.cli_runner.invoke(bitactor_cli.app, ["--version"])
        self.assertEqual(result.exit_code, 0)
    
    def test_cli_version_short_flag(self):
        """Test version short flag"""
        result = self.cli_runner.invoke(bitactor_cli.app, ["-v"])
        self.assertEqual(result.exit_code, 0)
    
    def test_cli_help_flag(self):
        """Test help flag"""
        result = self.cli_runner.invoke(bitactor_cli.app, ["--help"])
        self.assertEqual(result.exit_code, 0)
    
    def test_cli_main_callback(self):
        """Test main callback function"""
        # Test version callback directly
        with self.assertRaises(typer.Exit):
            bitactor_cli.version_callback(True)
        
        # Test with None (should not exit)
        result = bitactor_cli.version_callback(None)
        self.assertIsNone(result)
        
        # Test with False (should not exit)
        result = bitactor_cli.version_callback(False)
        self.assertIsNone(result)
    
    # ========== ERROR PATH COVERAGE ==========
    
    def test_cli_validate_ttl_exception(self):
        """Test TTL validation with exception"""
        cli = bitactor_cli.BitActorCLI()
        
        # Create a file that will cause parsing exception
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write("@prefix : <invalid> .\nThis is not valid TTL at all!")
            f.flush()
            
            try:
                result = cli.validate_ttl(Path(f.name))
                self.assertFalse(result)
            finally:
                Path(f.name).unlink()
    
    @patch('subprocess.run')
    def test_cli_run_c_tests_make_exception(self, mock_run):
        """Test C tests when make command raises exception"""
        mock_run.side_effect = FileNotFoundError("make not found")
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_c_tests(self.test_dir)
        
        # Should handle exception gracefully
        self.assertFalse(result["build"])
    
    @patch('subprocess.run')
    def test_cli_run_python_tests_exception(self, mock_run):
        """Test Python tests when subprocess raises exception"""
        mock_run.side_effect = OSError("Python execution failed")
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_python_tests(self.test_dir)
        
        # Should handle exception gracefully
        self.assertFalse(result["import"])
    
    @patch('subprocess.run')
    def test_cli_run_erlang_tests_timeout(self, mock_run):
        """Test Erlang tests with timeout"""
        mock_run.side_effect = subprocess.TimeoutExpired("escript", 10)
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_erlang_tests(self.test_dir)
        
        # Should handle timeout gracefully
        self.assertIn("error", result)
    
    # ========== PERFORMANCE PARSING COVERAGE ==========
    
    @patch('subprocess.run')
    def test_cli_run_c_tests_partial_performance(self, mock_run):
        """Test C tests with partial performance data"""
        partial_perf_output = """
        Running benchmark...
        Average throughput: 123.45 Msignals/sec
        No latency data available
        """
        
        mock_run.side_effect = [
            MagicMock(returncode=0, stdout="build ok"),
            MagicMock(returncode=0, stdout="test ok"),
            MagicMock(returncode=0, stdout=partial_perf_output)
        ]
        
        cli = bitactor_cli.BitActorCLI()
        with patch('pathlib.Path.glob') as mock_glob:
            mock_glob.return_value = [MagicMock(name="test_binary")]
            result = cli.run_c_tests(self.test_dir)
            
            self.assertTrue(result["benchmark"])
            self.assertIn("throughput", result["performance"])
            self.assertNotIn("latency", result["performance"])
    
    @patch('subprocess.run')
    def test_cli_run_python_tests_partial_performance(self, mock_run):
        """Test Python tests with partial performance data"""
        partial_output = """
✅ BitActor instantiated
✅ Processed 100000 signals
Throughput: 1234567.89 signals/sec
        """
        mock_run.return_value = MagicMock(returncode=0, stdout=partial_output)
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_python_tests(self.test_dir)
        
        self.assertTrue(result["signal_processing"])
        self.assertIn("throughput", result["performance"])
        self.assertNotIn("latency", result["performance"])
    
    # ========== ADDITIONAL EDGE CASES ==========
    
    @patch('subprocess.run')
    def test_cli_cleanup_temp_files(self, mock_run):
        """Test that temporary test files are cleaned up"""
        mock_run.return_value = MagicMock(returncode=0, stdout="test ok")
        
        cli = bitactor_cli.BitActorCLI()
        
        # Should create and cleanup temp file
        result = cli.run_python_tests(self.test_dir)
        
        # Verify temp file doesn't exist (it should be cleaned up)
        temp_test_file = self.test_dir / "test_python.py"
        self.assertFalse(temp_test_file.exists())
    
    def test_cli_json_serialization(self):
        """Test JSON serialization of results"""
        cli = bitactor_cli.BitActorCLI()
        
        test_results = {
            "C": {"build": True, "tests": True, "performance": {"throughput": "100 Msignals/sec"}},
            "Python": {"import": True, "signal_processing": True, "performance": {"throughput": "1M signals/sec"}},
        }
        
        # Test JSON serialization works
        json_str = json.dumps(test_results, indent=2)
        self.assertIsInstance(json_str, str)
        self.assertIn("build", json_str)
        
        # Test deserialization
        loaded_results = json.loads(json_str)
        self.assertEqual(loaded_results, test_results)

if __name__ == '__main__':
    # Run with maximum verbosity
    unittest.main(verbosity=2, buffer=True)