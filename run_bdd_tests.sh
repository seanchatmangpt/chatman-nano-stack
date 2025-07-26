#!/bin/bash

# Run BDD tests with coverage analysis
echo "🚀 Running BDD Test Suite for Ash.Reactor Components"
echo "=================================================="

# Set up test environment
export MIX_ENV=test
export COVERAGE=true

# Clean previous coverage data
rm -f coverage_report.html
rm -rf cover/

# Run specific test suites
echo ""
echo "📋 Running test suites:"
echo "  - TTL Ash Reactor Transformer Tests"
echo "  - BDD Integration Tests"
echo "  - Reactor Step Tests"
echo ""

# Run tests with coverage
mix test test/cns_forge/ttl_ash_reactor_transformer_test.exs \
         test/bdd/reactor_integration_test.exs \
         --cover \
         --trace \
         --color

# Check exit code
if [ $? -eq 0 ]; then
    echo ""
    echo "✅ All tests passed!"
    echo ""
    echo "📊 Coverage report generated:"
    echo "   - Terminal output above"
    echo "   - HTML report: coverage_report.html"
    
    # Open coverage report if on macOS
    if [[ "$OSTYPE" == "darwin"* ]]; then
        open coverage_report.html
    fi
else
    echo ""
    echo "❌ Some tests failed! Check output above."
    exit 1
fi