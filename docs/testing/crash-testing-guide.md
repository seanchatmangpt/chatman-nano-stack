# Crash Testing Guide

## Overview

This guide documents the crash testing infrastructure created for the CNS codebase to verify "let it crash" behavior.

## Test Infrastructure

### Python Test Files

#### 1. Unit Tests for Error Handling
- `test_pipeline_validator_crash.py` - Tests for pipeline validator error handling
- `test_validate_otel_crash.py` - Tests for OTEL validation errors
- `test_cns_monitor_crash.py` - Tests for performance monitor errors

#### 2. Crash Behavior Verification
- `test_crash_behavior.py` - Verifies that code without error handling actually crashes

### C Test Files
- `test_c_crash.c` - Demonstrates error handling vs crash behavior in C

## Running the Tests

### Python Tests

```bash
# Add pytest-asyncio if not already installed
uv add pytest-asyncio --dev

# Run individual test suites
uv run pytest test_pipeline_validator_crash.py -v
uv run pytest test_validate_otel_crash.py -v
uv run pytest test_cns_monitor_crash.py -v

# Run crash behavior verification
uv run pytest test_crash_behavior.py -v
```

### C Tests

```bash
# Compile and run C crash test
gcc -o test_c_crash test_c_crash.c
./test_c_crash
```

## Test Patterns

### 1. Exception Handling Tests

Tests verify that error handling works correctly before removal:

```python
@pytest.mark.asyncio
async def test_subprocess_failure(self, validator):
    """Test handling of subprocess failures"""
    with patch('asyncio.create_subprocess_exec') as mock_exec:
        mock_exec.side_effect = FileNotFoundError("Command not found")
        
        result = await validator._validate_something()
        
        assert result.status == "FAIL"
        assert "error" in result.issues[0]
```

### 2. Crash Verification Tests

Tests verify that code crashes without error handling:

```python
@pytest.mark.asyncio
async def test_crashes_without_handling(self, validator):
    """Test that code crashes without try-except"""
    async def failing_operation():
        raise RuntimeError("Expected failure")
    
    # Should NOT catch the exception
    with pytest.raises(RuntimeError, match="Expected failure"):
        await validator._run_operation(failing_operation)
```

### 3. AST Verification

Verify no try-except blocks remain:

```python
def test_no_try_except_blocks_remain(self):
    """Verify no try-except blocks in modified code"""
    import ast
    import inspect
    
    source = inspect.getsource(ModifiedClass)
    tree = ast.parse(source)
    
    try_blocks = [node for node in ast.walk(tree) 
                  if isinstance(node, ast.Try)]
    
    assert len(try_blocks) == 0
```

## Common Error Patterns Tested

### Python
1. **Subprocess Failures**: `FileNotFoundError`, `OSError`
2. **Parsing Errors**: `json.JSONDecodeError`, `ValueError`
3. **File I/O**: `PermissionError`, `IOError`
4. **Timeouts**: `asyncio.TimeoutError`
5. **General Exceptions**: `RuntimeError`, `Exception`

### C
1. **NULL Pointers**: Dereferencing NULL
2. **Memory Allocation**: `malloc` failures
3. **File Operations**: `fopen` failures
4. **Array Bounds**: Buffer overflows

## Best Practices

1. **Test Both Versions**: Always test with and without error handling
2. **Use Mocks**: Mock external dependencies to trigger failures
3. **Verify Crashes**: Use `pytest.raises` to verify exceptions propagate
4. **Check Side Effects**: Ensure partial operations don't corrupt state
5. **Document Expected Failures**: Clear comments on what should crash

## Integration with CI/CD

```yaml
# Example GitHub Actions configuration
test-crash-behavior:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    - name: Install dependencies
      run: |
        pip install uv
        uv sync
        uv add pytest-asyncio --dev
    - name: Run crash tests
      run: |
        uv run pytest test_*_crash.py -v
        gcc -o test_c_crash test_c_crash.c && ./test_c_crash
```

## Monitoring Crashes in Production

1. **Enable Core Dumps**: 
   ```bash
   ulimit -c unlimited
   ```

2. **Use Process Supervisors**:
   ```ini
   [program:cns-validator]
   command=/usr/bin/python pipeline_validator_no_error_handling.py
   autorestart=true
   autorestart_unexpected_exitcodes=0,1,2
   ```

3. **Log Crash Patterns**:
   ```python
   # Wrapper script for production
   import subprocess
   import time
   
   while True:
       start = time.time()
       result = subprocess.run(['python', 'app.py'])
       if result.returncode != 0:
           print(f"Crashed with code {result.returncode} after {time.time()-start}s")
       time.sleep(1)  # Prevent tight crash loops
   ```

## Conclusion

The crash testing infrastructure ensures that:
- Error handling works correctly before removal
- Code actually crashes without error handling
- No silent failures occur
- Testing is repeatable and automated

This supports the "let it crash" philosophy while maintaining confidence in system behavior.