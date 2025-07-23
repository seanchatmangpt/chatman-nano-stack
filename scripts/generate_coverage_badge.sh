#!/bin/bash
# Generate coverage badge for README
# Usage: ./generate_coverage_badge.sh

set -euo pipefail

CNS_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COVERAGE_DATA="${CNS_ROOT}/coverage/cns_coverage.profdata"

if [ ! -f "$COVERAGE_DATA" ]; then
    echo "❌ Coverage data not found. Run 'make coverage-test' first."
    exit 1
fi

# Get coverage percentage
COVERAGE=$(llvm-cov report ./cns -instr-profile="$COVERAGE_DATA" | tail -n 1 | awk '{print $4}' | sed 's/%//')

# Determine badge color
if (( $(echo "$COVERAGE >= 95" | bc -l) )); then
    COLOR="brightgreen"
elif (( $(echo "$COVERAGE >= 80" | bc -l) )); then
    COLOR="yellow"
else
    COLOR="red"
fi

# Generate badge URL
BADGE_URL="https://img.shields.io/badge/coverage-${COVERAGE}%25-${COLOR}"

echo "Coverage Badge URL:"
echo "$BADGE_URL"
echo ""
echo "Markdown for README:"
echo "![Coverage](${BADGE_URL})"
