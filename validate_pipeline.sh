#!/bin/bash
# CNS Pipeline Validation Runner
set -euo pipefail

echo "🚀 Running CNS Pipeline Validation"
echo "=================================="
echo ""

# Run validation with timeout to prevent hanging
timeout 45 uv run python pipeline_validator.py 2>&1 | tee pipeline-validation.log | \
    grep -E "(Validating|PASS|FAIL|WARN|Score:|Overall|8-Tick|8-Hour|8-MB)" | \
    head -50

# Extract summary from log
echo ""
echo "📊 VALIDATION SUMMARY"
echo "===================="
tail -100 pipeline-validation.log | grep -A20 "PIPELINE VALIDATION REPORT" | head -30 || true

# Check if report was generated
if ls pipeline-validation-*.json 2>/dev/null | head -1; then
    REPORT=$(ls -t pipeline-validation-*.json | head -1)
    echo ""
    echo "📄 Full report: $REPORT"
    
    # Extract key metrics
    echo ""
    echo "🔑 KEY METRICS:"
    jq -r '.summary | "Overall Score: \(.overall_score)/100\nVerdict: \(.verdict)\nPassed: \(.passed)/\(.total_validations)"' "$REPORT" 2>/dev/null || true
fi