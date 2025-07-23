#!/usr/bin/env python3
"""
Test script to verify crash behavior after removing try-except blocks
"""

import asyncio
import pytest
import subprocess
from unittest.mock import patch, MagicMock, AsyncMock
from pathlib import Path

from pipeline_validator_no_error_handling import CNSPipelineValidator


class TestCrashBehavior:
    """Test that code crashes without error handling"""
    
    @pytest.fixture
    def validator(self):
        """Create validator instance"""
        return CNSPipelineValidator()
    
    @pytest.mark.asyncio
    async def test_run_validation_crashes_on_exception(self, validator):
        """Test that _run_validation crashes without try-except"""
        # Create a failing validator function
        async def failing_validator():
            raise RuntimeError("Simulated validation failure")
        
        # This should NOT catch the exception - it should crash
        with pytest.raises(RuntimeError, match="Simulated validation failure"):
            await validator._run_validation("Test Component", failing_validator)
    
    @pytest.mark.asyncio
    async def test_validate_build_system_crashes_on_subprocess_failure(self, validator):
        """Test build system crashes on subprocess errors"""
        with patch('asyncio.create_subprocess_exec') as mock_exec:
            # Simulate subprocess throwing exception
            mock_exec.side_effect = FileNotFoundError("uv not found")
            
            # Should crash with FileNotFoundError
            with pytest.raises(FileNotFoundError, match="uv not found"):
                await validator._validate_build_system()
    
    @pytest.mark.asyncio
    async def test_validate_performance_crashes_on_parse_error(self, validator):
        """Test performance validation crashes on parse errors"""
        with patch('asyncio.create_subprocess_exec') as mock_exec:
            # Return output that will cause parsing to fail
            mock_process = AsyncMock()
            mock_process.returncode = 0
            mock_process.communicate = AsyncMock(return_value=(b'Invalid output', b''))
            mock_exec.return_value = mock_process
            
            # Should crash when trying to parse non-existent performance score
            with pytest.raises(IndexError):
                await validator._validate_performance()
    
    @pytest.mark.asyncio
    async def test_validate_otel_crashes_on_float_conversion(self, validator):
        """Test OTEL validation crashes on invalid float conversion"""
        with patch('asyncio.create_subprocess_exec') as mock_exec:
            # Return output with invalid success rate
            mock_process = AsyncMock()
            mock_process.returncode = 0
            mock_process.communicate = AsyncMock(return_value=(b'Success Rate: not_a_number%', b''))
            mock_exec.return_value = mock_process
            
            # Should crash with ValueError when converting to float
            with pytest.raises(ValueError):
                await validator._validate_otel_integration()
    
    @pytest.mark.asyncio
    async def test_main_crashes_on_validation_error(self):
        """Test main function crashes without error handling"""
        with patch('pipeline_validator_no_error_handling.CNSPipelineValidator.validate_pipeline') as mock_validate:
            mock_validate.side_effect = RuntimeError("Validation system error")
            
            # Should crash with RuntimeError
            with pytest.raises(RuntimeError, match="Validation system error"):
                from pipeline_validator_no_error_handling import main
                await main()
    
    def test_no_try_except_blocks_remain(self):
        """Verify no try-except blocks remain in the modified code"""
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
        
        # Should have NO try-except blocks
        assert len(try_blocks) == 0, f"Found {len(try_blocks)} try-except blocks, expected 0"
        
        print("✅ Confirmed: No try-except blocks remain in the code")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])