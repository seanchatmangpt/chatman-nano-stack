#!/bin/bash
# Per-file Coverage Analysis for CNS
# Provides detailed breakdown of coverage by source file

set -euo pipefail

CNS_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COVERAGE_DATA="${CNS_ROOT}/coverage/cns_coverage.profdata"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

if [ ! -f "$COVERAGE_DATA" ]; then
    echo -e "${RED}❌ Coverage data not found. Run 'make coverage-test' first.${NC}"
    exit 1
fi

echo -e "${BLUE}📊 CNS Per-File Coverage Analysis${NC}"
echo "=================================="

# Find all source files
SOURCE_FILES=$(find src -name "*.c" | sort)

echo ""
printf "%-40s %-12s %-12s %-12s %-8s\n" "File" "Lines" "Functions" "Regions" "Status"
echo "$(printf '%.0s-' {1..90})"

TOTAL_GOOD=0
TOTAL_NEEDS_WORK=0
TOTAL_CRITICAL=0

for file in $SOURCE_FILES; do
    # Get coverage for this specific file
    REPORT=$(llvm-cov report ./cns -instr-profile="$COVERAGE_DATA" "$file" 2>/dev/null | tail -n 1 || echo "")
    
    if [ -z "$REPORT" ]; then
        # File not covered at all
        printf "%-40s %-12s %-12s %-12s " "$(basename "$file")" "0%" "0%" "0%"
        echo -e "${RED}UNCOVERED${NC}"
        TOTAL_CRITICAL=$((TOTAL_CRITICAL + 1))
    else
        LINE_COV=$(echo "$REPORT" | awk '{print $4}' | sed 's/%//')
        FUNC_COV=$(echo "$REPORT" | awk '{print $3}' | sed 's/%//')
        REGION_COV=$(echo "$REPORT" | awk '{print $5}' | sed 's/%//')
        
        printf "%-40s %-12s %-12s %-12s " "$(basename "$file")" "${LINE_COV}%" "${FUNC_COV}%" "${REGION_COV}%"
        
        # Status based on line coverage
        if (( $(echo "$LINE_COV >= 95" | bc -l) )); then
            echo -e "${GREEN}EXCELLENT${NC}"
            TOTAL_GOOD=$((TOTAL_GOOD + 1))
        elif (( $(echo "$LINE_COV >= 80" | bc -l) )); then
            echo -e "${YELLOW}NEEDS WORK${NC}"
            TOTAL_NEEDS_WORK=$((TOTAL_NEEDS_WORK + 1))
        else
            echo -e "${RED}CRITICAL${NC}"
            TOTAL_CRITICAL=$((TOTAL_CRITICAL + 1))
        fi
    fi
done

echo ""
echo "Summary:"
echo -e "  ${GREEN}Excellent (≥95%):${NC}  $TOTAL_GOOD files"
echo -e "  ${YELLOW}Needs Work (80-94%):${NC} $TOTAL_NEEDS_WORK files"
echo -e "  ${RED}Critical (<80%):${NC}   $TOTAL_CRITICAL files"

# Show top 5 files needing attention
echo ""
echo "🎯 Top Priority Files for Testing:"
echo "=================================="

llvm-cov report ./cns -instr-profile="$COVERAGE_DATA" src/*.c 2>/dev/null | \
    grep -E '\.c' | \
    sort -k4 -n | \
    head -n 5 | \
    while read -r line; do
        file=$(echo "$line" | awk '{print $1}')
        coverage=$(echo "$line" | awk '{print $4}')
        echo "  $(basename "$file"): $coverage line coverage"
    done || echo "  (Run coverage tests first to see priority files)"
