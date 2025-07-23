#!/bin/bash
# CNS Test Coverage Setup Script
# Configures LLVM source-based coverage for ultra-low latency requirements

set -euo pipefail

echo "🔧 Setting up CNS Test Coverage Infrastructure"
echo "=============================================="

# Configuration
CNS_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COVERAGE_DIR="${CNS_ROOT}/coverage"
SCRIPTS_DIR="${CNS_ROOT}/scripts"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

log_success() {
    echo -e "${GREEN}✅${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

log_error() {
    echo -e "${RED}❌${NC} $1"
}

# Check for required tools
check_dependencies() {
    log_info "Checking dependencies..."
    
    local missing_deps=()
    
    # Check for clang with coverage support
    if ! command -v clang &> /dev/null; then
        missing_deps+=("clang")
    else
        # Check clang version (need >= 10.0 for good coverage support)
        clang_version=$(clang --version | head -n1 | grep -o '[0-9]\+\.[0-9]\+' | head -n1)
        if [[ $(echo "$clang_version < 10.0" | bc -l) -eq 1 ]]; then
            log_warning "Clang version $clang_version detected. Recommend >= 10.0 for optimal coverage"
        fi
    fi
    
    # Check for llvm-profdata
    if ! command -v llvm-profdata &> /dev/null; then
        missing_deps+=("llvm-profdata")
    fi
    
    # Check for llvm-cov
    if ! command -v llvm-cov &> /dev/null; then
        missing_deps+=("llvm-cov")
    fi
    
    # Check for bc (basic calculator) for version comparison
    if ! command -v bc &> /dev/null; then
        missing_deps+=("bc")
    fi
    
    if [ ${#missing_deps[@]} -ne 0 ]; then
        log_error "Missing dependencies: ${missing_deps[*]}"
        log_info "Install with: brew install llvm bc (macOS) or apt-get install clang llvm bc (Ubuntu)"
        exit 1
    fi
    
    log_success "All dependencies found"
}

# Create coverage directory structure
setup_directories() {
    log_info "Setting up directory structure..."
    
    mkdir -p "${COVERAGE_DIR}"/{reports,profiles,scripts}
    mkdir -p "${CNS_ROOT}/tests/coverage"
    
    log_success "Coverage directories created"
}

# Create coverage-enabled Makefile fragment
create_coverage_makefile() {
    log_info "Creating coverage Makefile fragment..."
    
    cat > "${CNS_ROOT}/Makefile.coverage" << 'EOF'
# CNS Coverage Build Configuration
# Include this in main Makefile with: include Makefile.coverage

# Coverage compilation flags
COVERAGE_FLAGS = -fprofile-instr-generate -fcoverage-mapping -fcoverage-mcdc
COVERAGE_CFLAGS = $(CFLAGS) $(COVERAGE_FLAGS)

# Coverage profile settings
export LLVM_PROFILE_FILE = coverage/profiles/cns_coverage_%p_%m.profraw

# Coverage targets
.PHONY: coverage-clean coverage-build coverage-test coverage-report coverage-summary

coverage-clean:
	@echo "🧹 Cleaning coverage data..."
	@rm -rf coverage/profiles/*.profraw coverage/reports/* coverage/*.profdata

coverage-build: coverage-clean
	@echo "🔨 Building with coverage instrumentation..."
	@$(MAKE) clean
	@$(MAKE) CC=clang CFLAGS="$(COVERAGE_CFLAGS)" all

coverage-test: coverage-build
	@echo "🧪 Running tests with coverage collection..."
	@cd tests && $(MAKE) test-bdd CC=clang CFLAGS="$(COVERAGE_CFLAGS)"
	@cd bitactor/tests && $(MAKE) test CC=clang CFLAGS="$(COVERAGE_CFLAGS)"

coverage-merge:
	@echo "📊 Merging coverage profiles..."
	@llvm-profdata merge -sparse coverage/profiles/*.profraw -o coverage/cns_coverage.profdata

coverage-report: coverage-test coverage-merge
	@echo "📈 Generating HTML coverage report..."
	@mkdir -p coverage/reports
	@llvm-cov show ./cns -instr-profile=coverage/cns_coverage.profdata \
		-format=html -output-dir=coverage/reports \
		-show-line-counts -show-regions -show-expansions
	@echo "Coverage report generated: coverage/reports/index.html"

coverage-summary: coverage-merge
	@echo "📋 Coverage Summary:"
	@echo "==================="
	@llvm-cov report ./cns -instr-profile=coverage/cns_coverage.profdata \
		-show-functions -show-regions

coverage-check: coverage-merge
	@echo "🚦 Checking coverage thresholds..."
	@$(SCRIPTS_DIR)/check_coverage_gate.sh

coverage-lcov: coverage-merge
	@echo "📤 Generating LCOV format for external tools..."
	@llvm-cov export ./cns -instr-profile=coverage/cns_coverage.profdata -format=lcov > coverage/coverage.lcov

# Development helpers
coverage-file-%: coverage-merge
	@echo "📁 Coverage for file: src/$*.c"
	@llvm-cov show ./cns -instr-profile=coverage/cns_coverage.profdata src/$*.c \
		-show-line-counts -format=text

coverage-function-%: coverage-merge
	@echo "🔍 Coverage for function: $*"
	@llvm-cov report ./cns -instr-profile=coverage/cns_coverage.profdata \
		-show-functions | grep "$*" || echo "Function $* not found"
EOF
    
    log_success "Coverage Makefile created"
}

# Create coverage gate checking script
create_coverage_gate_script() {
    log_info "Creating coverage gate check script..."
    
    cat > "${SCRIPTS_DIR}/check_coverage_gate.sh" << 'EOF'
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
EOF
    
    chmod +x "${SCRIPTS_DIR}/check_coverage_gate.sh"
    log_success "Coverage gate script created"
}

# Create per-file coverage analysis script
create_file_analysis_script() {
    log_info "Creating file-by-file coverage analysis script..."
    
    cat > "${SCRIPTS_DIR}/analyze_file_coverage.sh" << 'EOF'
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
EOF
    
    chmod +x "${SCRIPTS_DIR}/analyze_file_coverage.sh"
    log_success "File analysis script created"
}

# Create CI/CD integration files
create_ci_integration() {
    log_info "Creating CI/CD coverage integration..."
    
    # GitHub Actions workflow
    mkdir -p "${CNS_ROOT}/.github/workflows"
    cat > "${CNS_ROOT}/.github/workflows/coverage.yml" << 'EOF'
name: CNS Test Coverage

on:
  push:
    branches: [ master, develop ]
  pull_request:
    branches: [ master ]

jobs:
  coverage:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Install LLVM
      run: |
        sudo apt-get update
        sudo apt-get install -y clang llvm bc
    
    - name: Setup Coverage Environment
      run: |
        chmod +x scripts/setup_coverage.sh
        ./scripts/setup_coverage.sh
    
    - name: Build with Coverage
      run: make coverage-build
    
    - name: Run Tests with Coverage
      run: make coverage-test
    
    - name: Generate Coverage Report
      run: make coverage-report
    
    - name: Check Coverage Gates
      run: make coverage-check
    
    - name: Generate LCOV Report
      run: make coverage-lcov
    
    - name: Upload Coverage to Codecov
      uses: codecov/codecov-action@v3
      with:
        files: ./coverage/coverage.lcov
        flags: cns-core
        name: CNS Coverage
        fail_ci_if_error: true
    
    - name: Upload Coverage Report
      uses: actions/upload-artifact@v3
      with:
        name: coverage-report
        path: coverage/reports/
        retention-days: 30
    
    - name: Comment Coverage Summary
      if: github.event_name == 'pull_request'
      uses: actions/github-script@v6
      with:
        script: |
          const fs = require('fs');
          try {
            const coverage = fs.readFileSync('coverage/coverage_summary.txt', 'utf8');
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: '## 📊 Coverage Report\n\n```\n' + coverage + '\n```'
            });
          } catch (error) {
            console.log('Could not read coverage summary');
          }
EOF
    
    log_success "CI/CD integration created"
}

# Create development helper scripts
create_dev_helpers() {
    log_info "Creating development helper scripts..."
    
    # Quick coverage check script
    cat > "${SCRIPTS_DIR}/quick_coverage.sh" << 'EOF'
#!/bin/bash
# Quick coverage check for specific files or functions
# Usage: ./quick_coverage.sh [file.c] [function_name]

set -euo pipefail

CNS_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$CNS_ROOT"

if [ $# -eq 0 ]; then
    echo "Usage: $0 [file.c] [function_name]"
    echo "Examples:"
    echo "  $0 bitactor.c                    # Coverage for bitactor.c"
    echo "  $0 bitactor.c bitactor_tick      # Coverage for specific function"
    echo "  $0 all                           # Overall summary"
    exit 1
fi

# Ensure coverage data exists
if [ ! -f "coverage/cns_coverage.profdata" ]; then
    echo "⚡ No coverage data found. Running quick test..."
    make coverage-test > /dev/null 2>&1
fi

if [ "$1" = "all" ]; then
    make coverage-summary
elif [ $# -eq 1 ]; then
    make coverage-file-$(basename "$1" .c)
else
    make coverage-function-"$2"
    echo ""
    make coverage-file-$(basename "$1" .c) | grep -A 5 -B 5 "$2" || echo "Function context not found"
fi
EOF
    
    chmod +x "${SCRIPTS_DIR}/quick_coverage.sh"
    
    # Coverage diff script for PR reviews
    cat > "${SCRIPTS_DIR}/coverage_diff.sh" << 'EOF'
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
EOF
    
    chmod +x "${SCRIPTS_DIR}/coverage_diff.sh"
    
    log_success "Development helper scripts created"
}

# Create documentation integration
create_docs_integration() {
    log_info "Creating documentation integration..."
    
    # Coverage badge generation
    cat > "${SCRIPTS_DIR}/generate_coverage_badge.sh" << 'EOF'
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
EOF
    
    chmod +x "${SCRIPTS_DIR}/generate_coverage_badge.sh"
    
    log_success "Documentation integration created"
}

# Main setup function
main() {
    log_info "Starting CNS coverage setup..."
    
    check_dependencies
    setup_directories
    create_coverage_makefile
    create_coverage_gate_script
    create_file_analysis_script
    create_ci_integration
    create_dev_helpers
    create_docs_integration
    
    log_success "Coverage infrastructure setup complete!"
    echo ""
    echo "🚀 Next Steps:"
    echo "  1. Include coverage Makefile: echo 'include Makefile.coverage' >> Makefile"
    echo "  2. Run initial coverage: make coverage-report"
    echo "  3. Check current status: ./scripts/analyze_file_coverage.sh"
    echo "  4. View HTML report: open coverage/reports/index.html"
    echo ""
    echo "📋 Quick Commands:"
    echo "  make coverage-summary    # Text summary"
    echo "  make coverage-report     # Full HTML report"
    echo "  make coverage-check      # Gate validation"
    echo "  ./scripts/quick_coverage.sh [file.c]  # File-specific coverage"
}

# Run main function
main "$@"