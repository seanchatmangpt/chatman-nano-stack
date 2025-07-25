#!/bin/bash
# CNS Forge 80/20 Implementation Validation Script
# Validates all components multiple ways

set -e

echo "===================================="
echo "CNS Forge Validation Suite"
echo "===================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to run a test suite
run_test() {
    local test_name=$1
    local test_command=$2
    
    echo -e "${YELLOW}Running: $test_name${NC}"
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    if eval "$test_command"; then
        echo -e "${GREEN}✓ $test_name PASSED${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "${RED}✗ $test_name FAILED${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    echo ""
}

# 1. Unit Tests
echo "=== UNIT TESTS ==="
run_test "MetaCompiler Tests" "mix test test/cns_forge/meta_compiler_test.exs --color"

# 2. Integration Tests
echo "=== INTEGRATION TESTS ==="
run_test "Integration Tests" "mix test test/cns_forge/integration_test.exs --color"

# 3. Stress Tests
echo "=== STRESS TESTS ==="
run_test "Stress Tests" "mix test test/cns_forge/stress_test.exs --color --timeout 120000"

# 4. Adversarial Tests
echo "=== ADVERSARIAL TESTS ==="
run_test "Adversarial Tests" "mix test test/cns_forge/adversarial_test.exs --color"

# 5. Benchmarks
echo "=== PERFORMANCE BENCHMARKS ==="
run_test "Performance Benchmarks" "mix run benchmarks/cns_forge_bench.exs"

# 6. Terraform Validation
echo "=== INFRASTRUCTURE VALIDATION ==="
run_test "Terraform Syntax Check" "terraform -chdir=terraform init && terraform -chdir=terraform validate"

# 7. Kubernetes Manifest Validation
echo "=== KUBERNETES VALIDATION ==="
if command -v kubectl &> /dev/null; then
    run_test "K8s Manifest Validation" "kubectl apply --dry-run=client -f k8s/cns-forge-deployment.yaml"
else
    echo -e "${YELLOW}kubectl not found, skipping K8s validation${NC}"
fi

# 8. Elixir Code Quality
echo "=== CODE QUALITY ==="
run_test "Format Check" "mix format --check-formatted"
run_test "Compile Warnings" "mix compile --warnings-as-errors"

# 9. Coverage Report
echo "=== TEST COVERAGE ==="
if command -v mix &> /dev/null && mix help test.coverage &> /dev/null; then
    run_test "Test Coverage" "mix test --cover"
else
    echo -e "${YELLOW}Coverage tool not available${NC}"
fi

# 10. Memory Leak Check (simplified)
echo "=== MEMORY VALIDATION ==="
cat > /tmp/memory_check.exs << 'EOF'
# Simple memory leak check
initial = :erlang.memory(:total)
IO.puts("Initial memory: #{initial / 1024 / 1024} MB")

# Create and destroy many BitActors
for _ <- 1..1000 do
  {:ok, actor} = CNSForge.BitActor.create(%{
    type: :memory_test,
    transaction_id: "mem_#{:erlang.unique_integer()}",
    ttl: 8,
    token: %{data: :crypto.strong_rand_bytes(1024)}
  })
  
  CNSForge.BitActor.destroy(actor)
end

:erlang.garbage_collect()
Process.sleep(100)

final = :erlang.memory(:total)
IO.puts("Final memory: #{final / 1024 / 1024} MB")

growth = (final - initial) / initial * 100
IO.puts("Memory growth: #{Float.round(growth, 2)}%")

if growth > 10 do
  IO.puts("WARNING: Significant memory growth detected!")
  System.halt(1)
else
  IO.puts("Memory usage acceptable")
  System.halt(0)
end
EOF

run_test "Memory Leak Check" "mix run /tmp/memory_check.exs"
rm -f /tmp/memory_check.exs

# 11. OTEL Validation
echo "=== OBSERVABILITY VALIDATION ==="
cat > /tmp/otel_check.exs << 'EOF'
# Verify OpenTelemetry is properly configured
try do
  # Check if telemetry events are being emitted
  :telemetry.execute([:cns_forge, :test], %{count: 1}, %{})
  IO.puts("Telemetry system operational")
  
  # Verify metrics are defined
  metrics = CNSForge.Telemetry.metrics()
  IO.puts("Defined metrics: #{length(metrics)}")
  
  if length(metrics) >= 10 do
    IO.puts("OpenTelemetry configuration valid")
    System.halt(0)
  else
    IO.puts("Insufficient metrics defined")
    System.halt(1)
  end
rescue
  e ->
    IO.puts("OTEL check failed: #{inspect(e)}")
    System.halt(1)
end
EOF

run_test "OpenTelemetry Check" "mix run /tmp/otel_check.exs"
rm -f /tmp/otel_check.exs

# Summary
echo ""
echo "===================================="
echo "VALIDATION SUMMARY"
echo "===================================="
echo -e "Total Tests: $TOTAL_TESTS"
echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed: ${RED}$FAILED_TESTS${NC}"
echo ""

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}✓ ALL VALIDATIONS PASSED!${NC}"
    echo -e "${GREEN}CNS Forge 80/20 Implementation is PRODUCTION READY${NC}"
    exit 0
else
    echo -e "${RED}✗ VALIDATION FAILED${NC}"
    echo -e "${RED}$FAILED_TESTS tests did not pass${NC}"
    exit 1
fi