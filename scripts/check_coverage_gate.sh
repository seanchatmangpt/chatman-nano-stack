#!/bin/bash
# Coverage Gate Check for CNS
# Validates coverage meets minimum thresholds

set -euo pipefail

CNS_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COVERAGE_DATA="${CNS_ROOT}/coverage/cns_coverage.profdata"

# Coverage thresholds
LINE_THRESHOLD=95
FUNCTION_THRESHOLD=98
REGION_THRESHOLD=90

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

if [ ! -f "$COVERAGE_DATA" ]; then
    echo -e "${RED}❌ Coverage data not found. Run 'make coverage-test' first.${NC}"
    exit 1
fi

echo "🚦 CNS Coverage Gate Check"
echo "========================="

# Get coverage summary
REPORT=$(llvm-cov report ./cns -instr-profile="$COVERAGE_DATA" 2>/dev/null | tail -n 1)

if [ -z "$REPORT" ]; then
    echo -e "${RED}❌ Failed to generate coverage report${NC}"
    exit 1
fi

# Parse coverage percentages
LINE_COV=$(echo "$REPORT" | awk '{print $4}' | sed 's/%//')
FUNCTION_COV=$(echo "$REPORT" | awk '{print $3}' | sed 's/%//')
REGION_COV=$(echo "$REPORT" | awk '{print $5}' | sed 's/%//')

echo "Coverage Results:"
echo "  Line Coverage:     ${LINE_COV}%"
echo "  Function Coverage: ${FUNCTION_COV}%"
echo "  Region Coverage:   ${REGION_COV}%"
echo ""

# Check thresholds
FAILURES=0

if (( $(echo "$LINE_COV < $LINE_THRESHOLD" | bc -l) )); then
    echo -e "${RED}❌ Line coverage ${LINE_COV}% below threshold ${LINE_THRESHOLD}%${NC}"
    FAILURES=$((FAILURES + 1))
else
    echo -e "${GREEN}✅ Line coverage ${LINE_COV}% meets threshold${NC}"
fi

if (( $(echo "$FUNCTION_COV < $FUNCTION_THRESHOLD" | bc -l) )); then
    echo -e "${RED}❌ Function coverage ${FUNCTION_COV}% below threshold ${FUNCTION_THRESHOLD}%${NC}"
    FAILURES=$((FAILURES + 1))
else
    echo -e "${GREEN}✅ Function coverage ${FUNCTION_COV}% meets threshold${NC}"
fi

if (( $(echo "$REGION_COV < $REGION_THRESHOLD" | bc -l) )); then
    echo -e "${RED}❌ Region coverage ${REGION_COV}% below threshold ${REGION_THRESHOLD}%${NC}"
    FAILURES=$((FAILURES + 1))
else
    echo -e "${GREEN}✅ Region coverage ${REGION_COV}% meets threshold${NC}"
fi

echo ""

if [ $FAILURES -eq 0 ]; then
    echo -e "${GREEN}🎉 All coverage gates passed!${NC}"
    exit 0
else
    echo -e "${RED}💥 $FAILURES coverage gate(s) failed${NC}"
    echo ""
    echo "To see uncovered lines:"
    echo "  make coverage-report"
    echo "  open coverage/reports/index.html"
    exit 1
fi
