# CNS Comprehensive Test Coverage Plan

**Version:** 1.0  
**Date:** July 23, 2025  
**Objective:** Achieve 95%+ test coverage across all CNS source files

---

## Current Coverage Status

### ✅ Files with Existing Tests (23.5% coverage)
- `src/cns/cns_pipeline.c` → `tests/test_cns_pipeline_bdd.c`
- `src/cns/tick_parallel.c` → `tests/test_tick_parallel_bdd.c`
- `src/news/news_validator.c` → `tests/test_news_validator_bdd.c`
- BitActor core functionality → `bitactor/tests/test_bitactor_*.c`

### ❌ Files Missing Tests (76.5% coverage gap)

---

## Coverage Tool Strategy

### Recommended: LLVM Source-Based Coverage
**Rationale**: Better accuracy, optimization compatibility, MC/DC support for safety-critical code

```bash
# Compilation flags for CNS
COVERAGE_FLAGS = -fprofile-instr-generate -fcoverage-mapping -fcoverage-mcdc
CFLAGS += $(COVERAGE_FLAGS) -O3  # Optimizations enabled with coverage

# Environment setup
export LLVM_PROFILE_FILE="cns_coverage_%p_%m.profraw"
```

### Fallback: GCC gcov for Compatibility
```bash
# Traditional gcov approach
GCOV_FLAGS = --coverage -fprofile-arcs -ftest-coverage
```

---

## Priority-Based Test Implementation Plan

### 🔴 **CRITICAL PRIORITY** (Week 1-2)

#### 1. `src/cns/bitactor.c` (187 lines)
**Test File**: `bitactor/tests/test_bitactor_core_detailed.c`
**Focus Areas**:
- SIMD optimizations validation
- Memory management (zero allocation)
- Signal dispatch table accuracy
- 8-tick budget enforcement per function
- Error handling and edge cases

**Test Strategy**:
```c
FEATURE(BitActor_Core_Implementation) {
    SCENARIO("SIMD batch processing maintains 8-tick budget");
    SCENARIO("Signal dispatch table handles all 256 signal types");
    SCENARIO("Memory pool allocation never calls malloc");
    SCENARIO("Error conditions fail gracefully without heap allocation");
}
```

#### 2. `src/cns/bitactor_integration.c` (201 lines)
**Test File**: `bitactor/tests/test_bitactor_integration.c`
**Focus Areas**:
- Component integration paths
- Inter-module communication
- System initialization sequence
- Failure mode handling

#### 3. `src/sparql/sparql_parser.c` (161 lines)
**Test File**: `tests/test_sparql_parser_bdd.c`
**Focus Areas**:
- SPARQL query parsing accuracy
- AST generation correctness
- Arena allocation behavior
- Malformed query handling

### 🟠 **HIGH PRIORITY** (Week 3-4)

#### 4. `src/cns/bitactor_parallel.c` (137 lines)
**Test File**: `bitactor/tests/test_bitactor_parallel.c`
**Focus Areas**:
- Parallel execution correctness
- Load balancing algorithms
- Thread synchronization (if any)
- Performance scaling validation

#### 5. `src/cns/bitfiber.c` (132 lines)
**Test File**: `bitactor/tests/test_bitfiber.c`
**Focus Areas**:
- Fiber scheduling determinism
- Context switching overhead
- Stack management
- Yield/resume semantics

#### 6. `src/news/news_validator_demo.c` (180 lines)
**Test File**: `tests/test_news_validator_demo.c`
**Focus Areas**:
- Demo scenario accuracy
- Example data processing
- Integration with main validator
- Educational value verification

### 🟡 **MEDIUM PRIORITY** (Week 5-6)

#### SPARQL Processing Pipeline (Group Testing)
Create unified test suite: `tests/test_sparql_pipeline_bdd.c`

**Files to Cover**:
- `src/sparql/sparql_chains.c` (65 lines)
- `src/sparql/sparql_to_bitactor.c` (63 lines)
- `src/sparql/sparql_codegen.c` (62 lines)
- `src/sparql/sparql_compiler.c` (55 lines)

**Integrated Test Strategy**:
```c
FEATURE(SPARQL_to_BitActor_Pipeline) {
    SCENARIO("Complete SPARQL query compilation pipeline");
    SCENARIO("Query chaining maintains semantic correctness");
    SCENARIO("Code generation produces valid BitActor bytecode");
    SCENARIO("Compiler optimizations preserve query intent");
}
```

### 🔵 **LOW PRIORITY** (Week 7-8)

#### Utilities and Benchmarks
- `src/cns/tick_parallel_optimized.c` → Compare with base implementation
- `src/benchmark/otel_benchmark.c` → Validate OpenTelemetry integration
- `src/benchmark/benchmark_main.c` → Test harness accuracy
- `src/benchmark/simple_test.c` → Utility function validation

---

## Coverage Measurement Framework

### Makefile Integration
```makefile
# Coverage build targets
coverage-build:
	$(CC) $(CFLAGS) $(COVERAGE_FLAGS) -o $(TARGET) $(SOURCES)

coverage-test: coverage-build
	@echo "🧪 Running tests with coverage..."
	./run_all_tests.sh
	llvm-profdata merge -sparse *.profraw -o cns_coverage.profdata

coverage-report: coverage-test
	llvm-cov show ./$(TARGET) -instr-profile=cns_coverage.profdata \
		-format=html -output-dir=coverage_report
	llvm-cov report ./$(TARGET) -instr-profile=cns_coverage.profdata

coverage-summary:
	llvm-cov report ./$(TARGET) -instr-profile=cns_coverage.profdata \
		-show-functions -show-regions
```

### CI/CD Integration
```yaml
# .github/workflows/coverage.yml
- name: Generate Coverage Report
  run: |
    make coverage-report
    
- name: Coverage Gate Check
  run: |
    COVERAGE=$(llvm-cov report ./cns -instr-profile=cns_coverage.profdata | \
              grep TOTAL | awk '{print $4}' | sed 's/%//')
    if (( $(echo "$COVERAGE < 95" | bc -l) )); then
      echo "❌ Coverage $COVERAGE% below 95% threshold"
      exit 1
    fi
    echo "✅ Coverage: $COVERAGE%"

- name: Upload Coverage to Codecov
  uses: codecov/codecov-action@v3
  with:
    files: ./coverage_report/coverage.lcov
    flags: cns-core
```

### Coverage Analysis Scripts
```bash
#!/bin/bash
# scripts/analyze_coverage.sh

echo "📊 CNS Coverage Analysis"
echo "======================="

# Generate detailed coverage report
llvm-cov report ./cns -instr-profile=cns_coverage.profdata \
  -show-functions -show-regions -show-line-counts > coverage_detailed.txt

# Extract uncovered lines
llvm-cov show ./cns -instr-profile=cns_coverage.profdata \
  -format=text -show-line-counts-or-regions | \
  grep "0|" > uncovered_lines.txt

# Calculate per-file coverage
echo "Per-file coverage breakdown:"
for file in src/cns/*.c src/sparql/*.c src/news/*.c; do
    if [ -f "$file" ]; then
        coverage=$(llvm-cov report ./cns -instr-profile=cns_coverage.profdata \
                  "$file" 2>/dev/null | grep "$file" | awk '{print $4}' || echo "0%")
        printf "%-40s %s\n" "$(basename $file)" "$coverage"
    fi
done
```

---

## Test Quality Standards

### Coverage Metrics Targets
| Metric | Target | Critical |
|--------|--------|----------|
| **Line Coverage** | ≥95% | ≥90% |
| **Function Coverage** | ≥98% | ≥95% |
| **Branch Coverage** | ≥90% | ≥85% |
| **MC/DC Coverage** | ≥85% | ≥80% |

### Performance Constraints
All tests must maintain CNS performance requirements:
- **Test execution**: ≤8 CPU ticks per core function test
- **Memory**: Zero heap allocation during test execution
- **Determinism**: 100% reproducible results

### Test Documentation Requirements
Each test file must include:
```c
/*
 * Test Coverage Plan for [filename]
 * 
 * Lines Covered: XXX/XXX (XX.X%)
 * Functions Covered: XX/XX (XX.X%)
 * Branches Covered: XX/XX (XX.X%)
 * 
 * Uncovered Areas:
 * - Line XXX: [reason for exclusion]
 * - Function XXX: [integration test coverage]
 * 
 * Test Strategy:
 * - Unit tests for all public functions
 * - Integration tests for complex workflows
 * - Edge case validation for error conditions
 * - Performance validation for critical paths
 */
```

---

## Implementation Timeline

### Phase 1: Infrastructure (Week 1)
- Set up LLVM coverage toolchain
- Create coverage measurement scripts
- Integrate with existing build system

### Phase 2: Critical Tests (Week 1-2)
- BitActor core implementation tests
- Integration layer validation
- SPARQL parser fundamental tests

### Phase 3: High Priority (Week 3-4)
- Parallel processing validation
- Fiber system testing
- News validator demo coverage

### Phase 4: Complete Pipeline (Week 5-6)
- SPARQL processing pipeline tests
- End-to-end integration validation
- Cross-component interaction tests

### Phase 5: Utilities & Polish (Week 7-8)
- Benchmark and utility coverage
- Documentation completion
- CI/CD integration finalization

---

## Success Metrics

### Quantitative Goals
- **Overall Coverage**: ≥95% line coverage
- **Critical Path Coverage**: 100% for 8-tick budget functions
- **Error Path Coverage**: ≥90% for error handling code
- **Performance Impact**: <5% overhead with coverage enabled

### Qualitative Goals
- All safety-critical functions have MC/DC coverage
- Test suite completes in <60 seconds
- Coverage reports are actionable and clear
- New code automatically includes coverage validation

---

## Risk Mitigation

### Coverage Tool Compatibility
- Primary: LLVM source-based coverage
- Fallback: GCC gcov for CI/CD compatibility
- Validation: Cross-tool comparison for critical functions

### Performance Impact
- Separate coverage builds from production
- Performance regression detection in CI
- Memory allocation monitoring during tests

### Maintenance Overhead
- Automated coverage report generation
- Integration with existing BDD framework
- Clear documentation for coverage exclusions

---

This comprehensive test coverage plan will bring CNS from 23.5% to 95%+ coverage while maintaining the ultra-low latency and zero-allocation requirements that define the system's core architecture.