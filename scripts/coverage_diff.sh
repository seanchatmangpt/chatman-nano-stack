#!/bin/bash
# Coverage diff between branches
# Usage: ./coverage_diff.sh [base_branch] [compare_branch]

set -euo pipefail

BASE_BRANCH="${1:-master}"
COMPARE_BRANCH="${2:-$(git branch --show-current)}"

echo "📊 Coverage Diff: $BASE_BRANCH vs $COMPARE_BRANCH"
echo "============================================="

# Save current branch
CURRENT_BRANCH=$(git branch --show-current)

# Function to get coverage for a branch
get_coverage() {
    local branch=$1
    echo "Checking out $branch..."
    git checkout "$branch" > /dev/null 2>&1
    make coverage-test > /dev/null 2>&1
    llvm-cov report ./cns -instr-profile=coverage/cns_coverage.profdata | tail -n 1
}

# Get coverage for base branch
echo "Getting coverage for $BASE_BRANCH..."
BASE_COVERAGE=$(get_coverage "$BASE_BRANCH")
BASE_LINE=$(echo "$BASE_COVERAGE" | awk '{print $4}' | sed 's/%//')

# Get coverage for compare branch
echo "Getting coverage for $COMPARE_BRANCH..."
COMPARE_COVERAGE=$(get_coverage "$COMPARE_BRANCH")
COMPARE_LINE=$(echo "$COMPARE_COVERAGE" | awk '{print $4}' | sed 's/%//')

# Restore original branch
git checkout "$CURRENT_BRANCH" > /dev/null 2>&1

# Calculate diff
DIFF=$(echo "$COMPARE_LINE - $BASE_LINE" | bc -l)

echo ""
echo "Results:"
echo "  $BASE_BRANCH:    ${BASE_LINE}%"
echo "  $COMPARE_BRANCH: ${COMPARE_LINE}%"
echo "  Difference: ${DIFF}%"

if (( $(echo "$DIFF > 0" | bc -l) )); then
    echo "✅ Coverage improved!"
elif (( $(echo "$DIFF < -1" | bc -l) )); then
    echo "⚠️  Coverage decreased by more than 1%"
    exit 1
else
    echo "➡️  Coverage remained stable"
fi
