# Testing Documentation

## Overview

The CNS testing infrastructure includes comprehensive test suites for verifying both functional behavior and crash behavior in line with our "let it crash" philosophy.

## Test Categories

### 1. Unit Tests
Standard unit tests for component functionality

### 2. Crash Tests
- [Crash Testing Guide](./crash-testing-guide.md) - Comprehensive guide to crash testing
- Verify error handling behavior
- Confirm crash-on-error after removal

### 3. Performance Tests
- Benchmark tests for latency requirements
- 8-tick compliance verification

## Quick Start

```bash
# Install test dependencies
uv add pytest-asyncio --dev

# Run all tests
uv run pytest

# Run crash tests specifically
uv run pytest test_*_crash.py -v
```

## Test Files

### Python Tests
- `test_pipeline_validator_crash.py` - Pipeline validator error handling
- `test_validate_otel_crash.py` - OTEL validation errors
- `test_cns_monitor_crash.py` - Performance monitor errors
- `test_crash_behavior.py` - Crash behavior verification

### C Tests
- `test_c_crash.c` - C error handling demonstration

## Philosophy

Tests are designed to:
1. Verify functionality with error handling
2. Confirm crash behavior without error handling
3. Ensure no silent failures
4. Support the "let it crash" philosophy

See [Error Handling Philosophy](../error-handling-philosophy.md) for more details.