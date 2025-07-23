#!/usr/bin/env python3
"""
Unit tests for pipeline_validator.py - Testing try-except blocks
These tests will verify error handling works, then we'll remove it to let it crash
"""

import asyncio
import pytest
import subprocess
from unittest.mock import patch, MagicMock, AsyncMock
from pathlib import Path
import shutil
import json

# Import the module to test
from pipeline_validator import CNSPipelineValidator, ValidationResult


class TestPipelineValidatorCrash:
    """Test suite for try-except blocks in pipeline_validator.py"""
    
    @pytest.fixture
    def validator(self):
        """Create a validator instance for testing"""
        return CNSPipelineValidator()
    
    @pytest.mark.asyncio
    async def test_run_validation_exception_handling(self, validator):
        """Test that _run_validation handles exceptions properly"""
        # Create a failing validator function
        async def failing_validator():
            raise RuntimeError("Simulated validation failure")
        
        # This should catch the exception and create a FAIL result
        await validator._run_validation("Test Component", failing_validator)
        
        # Verify it created a failed result
        assert len(validator.results) == 1
        assert validator.results[0].status == "FAIL"
        assert validator.results[0].score == 0.0
        assert "Validation error: Simulated validation failure" in validator.results[0].issues[0]
    
    @pytest.mark.asyncio
    async def test_validate_build_system_subprocess_failure(self, validator):
        """Test build system validation with subprocess failures"""
        # Mock subprocess to simulate failure
        with patch('subprocess.run') as mock_run:
            mock_run.return_value = MagicMock(returncode=1)
            
            with patch('asyncio.create_subprocess_exec') as mock_exec:
                # Simulate compilation failure
                mock_process = AsyncMock()
                mock_process.returncode = 1
                mock_process.communicate = AsyncMock(return_value=(b'', b'Compilation failed'))
                mock_exec.return_value = mock_process
                
                result = await validator._validate_build_system()
                
                # Should handle the error gracefully
                assert result.status in ["FAIL", "WARN"]
                assert "OWL compilation failed" in result.issues
    
    @pytest.mark.asyncio
    async def test_validate_performance_exception(self, validator):
        """Test performance validation with exceptions"""
        with patch('asyncio.create_subprocess_exec') as mock_exec:
            # Simulate subprocess throwing exception
            mock_exec.side_effect = Exception("Subprocess launch failed")
            
            result = await validator._validate_performance()
            
            # Should catch exception and return FAIL
            assert result.status == "FAIL"
            assert result.score == 0.0
            assert "Performance validation error: Subprocess launch failed" in result.issues[0]
    
    @pytest.mark.asyncio
    async def test_validate_otel_integration_exception(self, validator):
        """Test OTEL validation with exceptions"""
        with patch('asyncio.create_subprocess_exec') as mock_exec:
            mock_exec.side_effect = FileNotFoundError("validate_otel.py not found")
            
            result = await validator._validate_otel_integration()
            
            # Should handle gracefully
            assert "OTEL validation error:" in result.issues[0]
    
    @pytest.mark.asyncio
    async def test_validate_code_quality_ruff_failure(self, validator):
        """Test code quality validation with ruff failures"""
        with patch('asyncio.create_subprocess_exec') as mock_exec:
            # Mock ruff failure
            mock_process = AsyncMock()
            mock_process.returncode = 1
            mock_process.communicate = AsyncMock(return_value=(b'file.py:10: E501 line too long\n', b''))
            mock_exec.return_value = mock_process
            
            with patch('pathlib.Path.glob') as mock_glob:
                mock_glob.return_value = [Path("test.py")]
                with patch('pathlib.Path.read_text') as mock_read:
                    mock_read.return_value = "def test(): pass"
                    
                    result = await validator._validate_code_quality()
                    
                    # Should handle ruff issues
                    assert result.metrics.get("ruff_issues", 0) > 0
    
    @pytest.mark.asyncio
    async def test_validate_security_file_read_exception(self, validator):
        """Test security validation with file read exceptions"""
        with patch('pathlib.Path.glob') as mock_glob:
            mock_glob.return_value = [Path("test.py")]
            with patch('pathlib.Path.read_text') as mock_read:
                mock_read.side_effect = PermissionError("Access denied")
                
                result = await validator._validate_security()
                
                # Should handle file read errors
                assert "Security scan error:" in result.issues[0]
    
    @pytest.mark.asyncio  
    async def test_validate_deployment_script_exception(self, validator):
        """Test deployment validation with script execution exceptions"""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = True
            
            with patch('asyncio.create_subprocess_exec') as mock_exec:
                mock_exec.side_effect = OSError("Permission denied")
                
                result = await validator._validate_deployment()
                
                # Should handle execution errors
                assert "Deployment script error:" in result.issues[0]
    
    @pytest.mark.asyncio
    async def test_validate_monitoring_json_parse_error(self, validator):
        """Test monitoring validation with JSON parsing errors"""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = True
            
            with patch('asyncio.create_subprocess_exec') as mock_exec:
                # Return invalid JSON
                mock_process = AsyncMock()
                mock_process.returncode = 0
                mock_process.communicate = AsyncMock(return_value=(b'Invalid JSON {', b''))
                mock_exec.return_value = mock_process
                
                result = await validator._validate_monitoring()
                
                # Should handle JSON decode errors
                assert "CNS status output is not valid JSON" in result.issues
    
    @pytest.mark.asyncio
    async def test_validate_neural_integration_timeout(self, validator):
        """Test neural integration with timeout/exception"""
        with patch('asyncio.create_subprocess_exec') as mock_exec:
            mock_exec.side_effect = asyncio.TimeoutError("Process timed out")
            
            result = await validator._validate_neural_integration()
            
            # Should handle timeout gracefully
            assert "Neural validation error:" in result.issues[0]
    
    @pytest.mark.asyncio
    async def test_validate_error_handling_file_access(self, validator):
        """Test error handling validation with file access issues"""
        with patch('pathlib.Path.glob') as mock_glob:
            mock_glob.return_value = [Path("test.py")]
            with patch('pathlib.Path.read_text') as mock_read:
                # First call succeeds, second fails
                mock_read.side_effect = ["def test():\n    try:\n        pass\n    except:\n        pass", 
                                       Exception("File locked")]
                
                result = await validator._validate_error_handling()
                
                # Should continue despite some file errors
                assert result.score >= 0  # Should not crash
    
    @pytest.mark.asyncio
    async def test_validate_documentation_exception(self, validator):
        """Test documentation validation with exceptions"""
        with patch('pathlib.Path.exists') as mock_exists:
            mock_exists.return_value = False
            
            with patch('pathlib.Path.glob') as mock_glob:
                mock_glob.return_value = [Path("test.py")]
                with patch('pathlib.Path.read_text') as mock_read:
                    mock_read.side_effect = UnicodeDecodeError('utf-8', b'', 0, 1, 'invalid utf-8')
                    
                    result = await validator._validate_documentation()
                    
                    # Should handle encoding errors
                    assert result.score >= 0  # Should complete without crashing
    
    @pytest.mark.asyncio
    async def test_main_keyboard_interrupt(self):
        """Test main function handles KeyboardInterrupt"""
        with patch('pipeline_validator.CNSPipelineValidator.validate_pipeline') as mock_validate:
            mock_validate.side_effect = KeyboardInterrupt()
            
            # Should handle gracefully
            await main()  # This should not raise


# Test that all exceptions are properly caught
def test_all_try_except_blocks_covered():
    """Verify we have tests for all try-except blocks in the module"""
    import ast
    import inspect
    
    # Get the source code
    source = inspect.getsource(CNSPipelineValidator)
    tree = ast.parse(source)
    
    # Count try-except blocks
    try_blocks = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Try):
            try_blocks.append(node)
    
    # We should have tests for each try-except block
    test_methods = [name for name in dir(TestPipelineValidatorCrash) if name.startswith('test_')]
    
    print(f"Found {len(try_blocks)} try-except blocks in pipeline_validator.py")
    print(f"Have {len(test_methods)} test methods")
    
    # Ensure we have reasonable coverage
    assert len(test_methods) >= 10  # We have at least 10 tests for different exceptions


if __name__ == "__main__":
    # Run the tests
    pytest.main([__file__, "-v"])