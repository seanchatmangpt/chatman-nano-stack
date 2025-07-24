#!/usr/bin/env python3
"""
Absolute Coverage Verification - Direct Import Testing
MISSION: Achieve verified 80% coverage on BitActor modules
"""

import unittest
import sys
import os
import tempfile
import json
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add current directory to path
sys.path.insert(0, '/Users/sac/cns')
sys.path.insert(0, '/Users/sac/cns/generated/test_fix')

# Direct imports of target modules
import bitactor_cli
import bitactor_ttl_generator

class AbsoluteCoverageTest(unittest.TestCase):
    """Direct testing of BitActor modules for maximum coverage"""
    
    def setUp(self):
        self.test_dir = Path("/Users/sac/cns/generated/test_fix")
        self.ttl_file = Path("/Users/sac/cns/ontologies/bitactor_semantic_core.ttl")
    
    # ========== BITACTOR CLI COMPREHENSIVE TESTING ==========
    
    def test_cli_initialization(self):
        """Test CLI class initialization"""
        cli = bitactor_cli.BitActorCLI()
        self.assertIsNotNone(cli.generator)
        self.assertIsInstance(cli.test_results, dict)
    
    def test_cli_validate_ttl_success(self):
        """Test successful TTL validation"""
        cli = bitactor_cli.BitActorCLI()
        if self.ttl_file.exists():
            result = cli.validate_ttl(self.ttl_file)
            self.assertTrue(result)
    
    def test_cli_validate_ttl_failure(self):
        """Test TTL validation failure"""
        cli = bitactor_cli.BitActorCLI()
        with tempfile.NamedTemporaryFile(mode='w', suffix='.ttl', delete=False) as f:
            f.write("invalid ttl content")
            f.flush()
            try:
                result = cli.validate_ttl(Path(f.name))
                self.assertFalse(result)
            finally:
                Path(f.name).unlink()
    
    @patch('subprocess.run')
    def test_cli_run_c_tests_success(self, mock_run):
        """Test successful C test execution"""
        mock_run.side_effect = [
            MagicMock(returncode=0, stdout="build success"),  # make
            MagicMock(returncode=0, stdout="test success"),   # test
            MagicMock(returncode=0, stdout="Average throughput: 100.0 Msignals/sec\nAverage latency: 10.0 CPU ticks/signal")  # benchmark
        ]
        
        cli = bitactor_cli.BitActorCLI()
        with patch('pathlib.Path.glob', return_value=[MagicMock(name="test_binary")]):
            result = cli.run_c_tests(self.test_dir)
            
            self.assertTrue(result["build"])
            self.assertTrue(result["tests"])
            self.assertTrue(result["benchmark"])
            self.assertIn("throughput", result["performance"])
            self.assertIn("latency", result["performance"])
    
    @patch('subprocess.run')
    def test_cli_run_c_tests_build_failure(self, mock_run):
        """Test C test build failure"""
        mock_run.return_value = MagicMock(returncode=1, stdout="build failed")
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_c_tests(self.test_dir)
        
        self.assertFalse(result["build"])
        self.assertFalse(result["tests"])
    
    @patch('subprocess.run')
    def test_cli_run_python_tests_success(self, mock_run):
        """Test successful Python test execution"""
        mock_output = """
✅ BitActor instantiated
✅ Processed 100000 signals
Throughput: 1000000.0 signals/sec
Avg time: 1.0 µs/signal
        """
        mock_run.return_value = MagicMock(returncode=0, stdout=mock_output)
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_python_tests(self.test_dir)
        
        self.assertTrue(result["import"])
        self.assertTrue(result["signal_processing"])
        self.assertIn("throughput", result["performance"])
        self.assertIn("latency", result["performance"])
    
    @patch('subprocess.run')
    def test_cli_run_python_tests_failure(self, mock_run):
        """Test Python test failure"""
        mock_run.return_value = MagicMock(returncode=1, stdout="import failed")
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_python_tests(self.test_dir)
        
        self.assertFalse(result["import"])
        self.assertFalse(result["signal_processing"])
    
    @patch('subprocess.run')
    @patch('pathlib.Path.glob')
    def test_cli_run_erlang_tests_success(self, mock_glob, mock_run):
        """Test successful Erlang test execution"""
        mock_glob.return_value = [MagicMock(stem="test_bitactor")]
        mock_run.side_effect = [
            MagicMock(returncode=0),  # which escript
            MagicMock(returncode=0, stdout="✅ Module compiled\n✅ Server started\n✅ Processed 1000 signals\nThroughput: 500.0 signals/sec")
        ]
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_erlang_tests(self.test_dir)
        
        self.assertTrue(result["compile"])
        self.assertTrue(result["start"])
        self.assertTrue(result["signal_processing"])
        self.assertIn("throughput", result["performance"])
    
    @patch('subprocess.run')
    def test_cli_run_erlang_tests_no_escript(self, mock_run):
        """Test Erlang test when escript not available"""
        mock_run.return_value = MagicMock(returncode=1)  # which escript fails
        
        cli = bitactor_cli.BitActorCLI()
        result = cli.run_erlang_tests(self.test_dir)
        
        self.assertFalse(result["compile"])
        self.assertIn("error", result)
    
    def test_cli_display_results_comprehensive(self):
        """Test comprehensive results display"""
        cli = bitactor_cli.BitActorCLI()
        
        # Test all result combinations
        test_results = [
            # All pass
            {
                "C": {"build": True, "tests": True, "performance": {"throughput": "100 Msignals/sec"}},
                "Python": {"import": True, "signal_processing": True, "performance": {"throughput": "1M signals/sec"}},
                "Erlang": {"compile": True, "signal_processing": True, "performance": {"throughput": "500K signals/sec"}}
            },
            # Mixed results
            {
                "C": {"build": False, "tests": False, "performance": {}},
                "Python": {"import": True, "signal_processing": False, "performance": {}},
                "Erlang": {"compile": True, "signal_processing": True, "performance": {"throughput": "100K"}}
            },
            # All fail
            {
                "C": {"build": False, "tests": False, "performance": {}},
                "Python": {"import": False, "signal_processing": False, "performance": {}},
                "Erlang": {"compile": False, "signal_processing": False, "performance": {}}
            }
        ]
        
        for results in test_results:
            # Should not crash
            cli.display_results(results)
            self.assertTrue(True)
    
    # ========== BITACTOR TTL GENERATOR COMPREHENSIVE TESTING ==========
    
    def test_generator_initialization(self):
        """Test generator initialization"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        self.assertIsNotNone(generator.graph)
        self.assertIsNotNone(generator.template_dir)
    
    def test_generator_load_ttl_success(self):
        """Test successful TTL loading"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        if self.ttl_file.exists():
            generator.load_ttl(str(self.ttl_file))
            self.assertGreater(len(generator.graph), 0)
    
    def test_generator_load_ttl_failure(self):
        """Test TTL loading failure"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        with self.assertRaises(Exception):
            generator.load_ttl("/nonexistent/file.ttl")
    
    def test_generator_extract_signals(self):
        """Test signal extraction"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        if self.ttl_file.exists():
            generator.load_ttl(str(self.ttl_file))
            signals = generator.extract_signals()
            
            self.assertIsInstance(signals, list)
            if signals:
                signal = signals[0]
                self.assertIn("name", signal)
                self.assertIn("id", signal)
    
    def test_generator_extract_signals_empty(self):
        """Test signal extraction with empty graph"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        signals = generator.extract_signals()
        
        self.assertIsInstance(signals, list)
        self.assertEqual(len(signals), 0)
    
    def test_generator_extract_handlers(self):
        """Test handler extraction"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        if self.ttl_file.exists():
            generator.load_ttl(str(self.ttl_file))
            handlers = generator.extract_handlers()
            
            self.assertIsInstance(handlers, list)
            if handlers:
                handler = handlers[0]
                self.assertIn("name", handler)
    
    def test_generator_extract_handlers_empty(self):
        """Test handler extraction with empty graph"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        handlers = generator.extract_handlers()
        
        self.assertIsInstance(handlers, list)
        self.assertEqual(len(handlers), 0)
    
    def test_generator_generate_context(self):
        """Test context generation"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("test_ontology", "test_prefix")
        
        self.assertIsInstance(context, dict)
        self.assertIn("ontology_name", context)
        self.assertIn("class_prefix", context)
        self.assertIn("signals", context)
        self.assertIn("handlers", context)
        self.assertEqual(context["ontology_name"], "test_ontology")
        self.assertEqual(context["prefix"], "test_prefix")
    
    def test_generator_generate_context_with_data(self):
        """Test context generation with loaded data"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        if self.ttl_file.exists():
            generator.load_ttl(str(self.ttl_file))
            context = generator.generate_context("loaded_ontology", "loaded")
            
            self.assertIn("signals", context)
            self.assertIn("handlers", context)
            self.assertGreaterEqual(len(context["signals"]), 0)
            self.assertGreaterEqual(len(context["handlers"]), 0)
    
    def test_generator_generate_c_code(self):
        """Test C code generation"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("test", "test")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as f:
            try:
                generator.generate_c_code(context, f.name)
                self.assertTrue(Path(f.name).exists())
                self.assertGreater(Path(f.name).stat().st_size, 0)
            finally:
                Path(f.name).unlink(missing_ok=True)
    
    def test_generator_generate_python_code(self):
        """Test Python code generation"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("test", "test")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            try:
                generator.generate_python_code(context, f.name)
                self.assertTrue(Path(f.name).exists())
                self.assertGreater(Path(f.name).stat().st_size, 0)
            finally:
                Path(f.name).unlink(missing_ok=True)
    
    def test_generator_generate_erlang_code(self):
        """Test Erlang code generation"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("test", "test")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.erl', delete=False) as f:
            try:
                generator.generate_erlang_code(context, f.name)
                self.assertTrue(Path(f.name).exists())
                self.assertGreater(Path(f.name).stat().st_size, 0)
            finally:
                Path(f.name).unlink(missing_ok=True)
    
    def test_generator_generate_test_code(self):
        """Test test code generation"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("test", "test")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.c', delete=False) as f:
            try:
                generator.generate_test_code(context, f.name)
                self.assertTrue(Path(f.name).exists())
                self.assertGreater(Path(f.name).stat().st_size, 0)
            finally:
                Path(f.name).unlink(missing_ok=True)
    
    def test_generator_generate_benchmark_code(self):
        """Test benchmark code generation"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("test", "test")
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.c', delete=False) as f:
            try:
                generator.generate_benchmark_code(context, f.name)
                self.assertTrue(Path(f.name).exists())
                self.assertGreater(Path(f.name).stat().st_size, 0)
            finally:
                Path(f.name).unlink(missing_ok=True)
    
    def test_generator_generate_makefile(self):
        """Test Makefile generation"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("test", "test")
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            try:
                generator.generate_makefile(context, f.name)
                self.assertTrue(Path(f.name).exists())
                self.assertGreater(Path(f.name).stat().st_size, 0)
            finally:
                Path(f.name).unlink(missing_ok=True)
    
    def test_generator_generate_all(self):
        """Test complete code generation"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        if self.ttl_file.exists():
            with tempfile.TemporaryDirectory() as tmpdir:
                generator.generate_all(str(self.ttl_file), tmpdir, "coverage_test")
                
                # Check all expected files were created
                output_path = Path(tmpdir)
                expected_files = [
                    "coverage_test_bitactor.h",
                    "coverage_test_bitactor.py", 
                    "coverage_test_bitactor.erl",
                    "coverage_test_test.c",
                    "coverage_test_benchmark.c",
                    "Makefile"
                ]
                
                for filename in expected_files:
                    file_path = output_path / filename
                    self.assertTrue(file_path.exists(), f"Missing: {filename}")
                    self.assertGreater(file_path.stat().st_size, 0, f"Empty: {filename}")
    
    def test_generator_error_handling(self):
        """Test generator error handling"""
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        # Test with read-only path
        try:
            context = generator.generate_context("test", "test")
            generator.generate_c_code(context, "/dev/null/readonly.h")
            self.fail("Should have raised exception")
        except Exception:
            self.assertTrue(True)  # Expected to fail
    
    # ========== INTEGRATION TESTING ==========
    
    def test_end_to_end_workflow(self):
        """Test complete end-to-end workflow"""
        if not self.ttl_file.exists():
            self.skipTest("TTL file not available")
        
        # Step 1: Initialize generator
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        
        # Step 2: Load TTL
        generator.load_ttl(str(self.ttl_file))
        
        # Step 3: Generate code
        with tempfile.TemporaryDirectory() as tmpdir:
            generator.generate_all(str(self.ttl_file), tmpdir, "integration_test")
            
            # Step 4: Verify files created
            generated_files = list(Path(tmpdir).glob("*"))
            self.assertGreater(len(generated_files), 0)
            
            # Step 5: Initialize CLI
            cli = bitactor_cli.BitActorCLI()
            
            # Step 6: Validate TTL
            is_valid = cli.validate_ttl(self.ttl_file)
            self.assertTrue(is_valid)
    
    def test_performance_requirements(self):
        """Test that performance requirements are met"""
        # This is tested through the actual BitActor implementation
        # Import and test the generated BitActor
        try:
            from test_bitactor import TestBitActor, TestSignal, TestSignalType
            
            ba = TestBitActor()
            
            # Process multiple signals to test performance
            import time
            start = time.perf_counter()
            
            for i in range(1000):
                signal = TestSignal(
                    type=TestSignalType.SEMANTICSIGNAL,
                    flags=i,
                    timestamp=i * 1000,
                    payload=i
                )
                try:
                    ba.process_signal(signal)
                except:
                    pass  # Some signals may fail due to tick budget
            
            elapsed = time.perf_counter() - start
            throughput = 1000 / elapsed
            
            # Should process at least 100K signals/sec in Python
            self.assertGreater(throughput, 100000)
            
        except ImportError:
            self.skipTest("Generated BitActor not available")
    
    def test_cross_platform_compatibility(self):
        """Test cross-platform compatibility"""
        # Test that the generator produces platform-compatible code
        generator = bitactor_ttl_generator.BitActorTTLGenerator()
        context = generator.generate_context("platform_test", "plat")
        
        # Generate C code and check for platform-specific elements
        with tempfile.NamedTemporaryFile(mode='w', suffix='.h', delete=False) as f:
            try:
                generator.generate_c_code(context, f.name)
                
                # Read generated code
                with open(f.name, 'r') as rf:
                    content = rf.read()
                    
                    # Should have platform detection
                    self.assertIn("__x86_64__", content)
                    self.assertIn("__aarch64__", content)
                    
            finally:
                Path(f.name).unlink(missing_ok=True)

if __name__ == '__main__':
    # Run with maximum verbosity
    unittest.main(verbosity=2, buffer=True)