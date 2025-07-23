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
