#!/bin/bash
# BitActor Test Runner Script

set -e  # Exit on error

echo "🚀 BitActor Test Suite Runner"
echo "============================"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test results
PASSED=0
FAILED=0

# Function to run a test
run_test() {
    local test_name=$1
    local test_binary=$2
    
    echo -n "Running $test_name... "
    
    if ./$test_binary > ${test_binary}.log 2>&1; then
        echo -e "${GREEN}✅ PASS${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ FAIL${NC}"
        echo "  See ${test_binary}.log for details"
        ((FAILED++))
    fi
}

# Build tests
echo "Building tests..."
make clean > /dev/null 2>&1
make all

echo ""
echo "Running tests..."
echo ""

# Run all tests
run_test "Core Tests" "test_bitactor_core"
run_test "Compiler Tests" "test_compiler"
run_test "Telemetry Tests" "test_telemetry"
run_test "Memory Tests" "test_memory"
run_test "Performance Tests" "test_performance"

echo ""
echo "Running specialized checks..."
echo ""

# Check for branches
echo -n "Checking for conditional branches... "
if make check-branches > branch_check.log 2>&1; then
    echo -e "${GREEN}✅ No branches found${NC}"
else
    echo -e "${YELLOW}⚠️  See branch_check.log${NC}"
fi

# Memory analysis (if valgrind available)
if command -v valgrind &> /dev/null; then
    echo -n "Running memory analysis... "
    if valgrind --tool=massif --pages-as-heap=yes ./test_bitactor_core > valgrind.log 2>&1; then
        if grep -q "heap" massif.out.*; then
            echo -e "${RED}❌ Heap allocation detected${NC}"
            ((FAILED++))
        else
            echo -e "${GREEN}✅ No heap allocation${NC}"
            ((PASSED++))
        fi
    else
        echo -e "${YELLOW}⚠️  Valgrind failed${NC}"
    fi
else
    echo -e "${YELLOW}⚠️  Valgrind not available, skipping memory analysis${NC}"
fi

# Performance gate check
echo -n "Checking P99.999 latency... "
if ./test_performance | grep -q "P99.999=[0-8]"; then
    echo -e "${GREEN}✅ Within 8 ticks${NC}"
    ((PASSED++))
else
    echo -e "${RED}❌ Exceeds 8 ticks${NC}"
    ((FAILED++))
fi

# Summary
echo ""
echo "=============================="
echo "Test Summary:"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✅ All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}❌ $FAILED tests failed${NC}"
    exit 1
fi