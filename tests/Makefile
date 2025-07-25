CC = gcc
CFLAGS = -O3 -Wall -Wextra -std=c99
CFLAGS += -I../src -I../bitactor/tests -I../bitactor/include
CFLAGS += -march=native -mtune=native

# Toggle between mock and real implementations
USE_REAL_IMPL ?= 1

# Test targets - Mock tests (legacy)
MOCK_TESTS = test_cns_pipeline_bdd test_tick_parallel_bdd test_news_validator_bdd test_sparql_parser_bdd test_generated_uhft_bdd test_bitactor_core_extended_bdd test_sparql_complete_bdd test_uhft_comprehensive_bdd test_bitactor_advanced_bdd test_algorithms_8tick_bdd test_benchmarks_validation_bdd

# Real implementation tests (primary)
REAL_TESTS = test_bitactor_core_real_bdd test_bitactor_integration_real_bdd test_sparql_real_bdd test_news_validator_real_bdd test_bitfiber_real_bdd test_cns_integration_real_bdd test_bitactor_chaos_bdd test_cns_system_integration_complete

# All tests
BDD_TESTS = $(REAL_TESTS) $(MOCK_TESTS)
PERF_TESTS = test_real_vs_mock_performance

# Source files to test
CNS_SRCS = ../src/cns/cns_pipeline.c ../src/cns/tick_parallel.c
NEWS_SRCS = ../src/news/news_validator.c

ifeq ($(USE_REAL_IMPL),1)
    # Use real implementations with adapters
    TEST_SOURCES = ../bitactor/tests/test_adapters.c
    CFLAGS += -DUSE_REAL_IMPLEMENTATIONS
    # Avoid including real implementation .c files directly as they're in test_adapters.c
else
    # Use mocks for isolated testing
    TEST_SOURCES = mock_implementations.c ../bitactor/tests/mock_bitactor.c ../bitactor/tests/mock_telemetry.c
    CFLAGS += -DMOCK_NEWS_VALIDATOR
endif

.PHONY: all clean test-bdd test-real test-mock run-all test-performance test-coverage

all: $(BDD_TESTS) $(PERF_TESTS)

# BDD Tests for existing C code - Use only test adapters to avoid symbol conflicts
test_cns_pipeline_bdd: test_cns_pipeline_bdd.c $(CNS_SRCS)
	$(CC) $(CFLAGS) -o $@ $^ ../bitactor/tests/test_adapters.c ../bitactor/tests/test_adapters_impl.c -lpthread -DMOCK_NEWS_VALIDATOR

test_tick_parallel_bdd: test_tick_parallel_bdd.c ../src/cns/tick_parallel.c
	$(CC) $(CFLAGS) -o $@ $^

test_news_validator_bdd: test_news_validator_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DMOCK_NEWS_VALIDATOR

# New comprehensive BDD tests
test_sparql_parser_bdd: test_sparql_parser_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DMOCK_SPARQL_PARSER

test_generated_uhft_bdd: test_generated_uhft_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DMOCK_UHFT_SYSTEM

test_bitactor_core_extended_bdd: test_bitactor_core_extended_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DMOCK_BITACTOR_EXTENDED

# Additional comprehensive BDD tests
test_sparql_complete_bdd: test_sparql_complete_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DMOCK_SPARQL_COMPLETE

test_uhft_comprehensive_bdd: test_uhft_comprehensive_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DMOCK_UHFT_COMPREHENSIVE

test_bitactor_advanced_bdd: test_bitactor_advanced_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DMOCK_BITACTOR_ADVANCED

test_algorithms_8tick_bdd: test_algorithms_8tick_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DMOCK_ALGORITHMS_8TICK

test_benchmarks_validation_bdd: test_benchmarks_validation_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DMOCK_BENCHMARKS_VALIDATION

# Real implementation tests
test_bitactor_core_real_bdd: test_bitactor_core_real_bdd.c
	$(CC) $(CFLAGS) -o $@ $< ../bitactor/src/bitactor.c ../bitactor/src/bitactor_dispatch.c ../bitactor/src/bitactor_telemetry.c ../bitactor/src/bitfiber.c -DUSE_REAL_IMPLEMENTATIONS -I../bitactor/src -I../bitactor/include -lpthread

test_bitactor_integration_real_bdd: test_bitactor_integration_real_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DUSE_REAL_IMPLEMENTATIONS -I../bitactor/integration -I../bitactor/src

test_sparql_real_bdd: test_sparql_real_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DUSE_REAL_IMPLEMENTATIONS -I../src/sparql

# News validator real test (link actual implementation)
test_news_validator_real_bdd: test_news_validator_real_bdd.c
	$(CC) $(CFLAGS) -o $@ $< ../bitactor/src/news_validation_real_10ns.c ../src/news/news_validator.c -DUSE_REAL_IMPLEMENTATIONS -I../src/news -I../bitactor/include -lpthread

test_bitfiber_real_bdd: test_bitfiber_real_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DUSE_REAL_IMPLEMENTATIONS -I../bitactor/src

test_cns_integration_real_bdd: test_cns_integration_real_bdd.c
	$(CC) $(CFLAGS) -o $@ $< -DUSE_REAL_IMPLEMENTATIONS -I../bitactor/integration -I../src/sparql -I../src/news

# Chaos engineering test - include all required source files
test_bitactor_chaos_bdd: test_bitactor_chaos_bdd.c
	$(CC) $(CFLAGS) -o $@ $< ../bitactor/src/bitactor.c ../bitactor/src/bitactor_dispatch.c ../bitactor/src/bitactor_telemetry.c ../bitactor/src/bitfiber.c -DUSE_REAL_IMPLEMENTATIONS -I../bitactor/src -I../bitactor/include -lpthread

# Complete system integration test - Available subsystems (BitActor core + telemetry + fiber)
test_cns_system_integration_complete: test_cns_system_integration_complete.c
	$(CC) $(CFLAGS) -o $@ $< ../bitactor/src/bitactor.c ../bitactor/src/bitactor_dispatch.c ../bitactor/src/bitactor_telemetry.c ../bitactor/src/bitfiber.c -DUSE_REAL_IMPLEMENTATIONS -I../bitactor/src -I../bitactor/include -lpthread

# Run all BDD tests
test-bdd: $(BDD_TESTS)
	@echo "🧪 Running CNS Codebase BDD Tests"
	@echo "================================="
	@for test in $(BDD_TESTS); do \
		echo "\n🔍 Testing $$test..."; \
		./$$test || exit 1; \
	done
	@echo "\n✅ All CNS BDD tests passed!"

# Run only real implementation tests (recommended)
test-real: $(REAL_TESTS)
	@echo "🚀 Running Real Implementation Tests"
	@echo "===================================="
	@for test in $(REAL_TESTS); do \
		echo "\n🔍 Testing $$test..."; \
		./$$test || exit 1; \
	done
	@echo "\n✅ All real implementation tests passed!"

# Run only mock tests (legacy)
test-mock: $(MOCK_TESTS)
	@echo "🎭 Running Mock Tests (Legacy)"
	@echo "=============================="
	@for test in $(MOCK_TESTS); do \
		echo "\n🔍 Testing $$test..."; \
		./$$test || exit 1; \
	done
	@echo "\n✅ All mock tests passed!"

# Run tests with detailed output
run-all: $(BDD_TESTS)
	@for test in $(BDD_TESTS); do \
		echo "\n" | ./$$test; \
	done

# Performance comparison test
test_real_vs_mock_performance: test_real_vs_mock_performance.c ../bitactor/tests/test_adapters.c ../bitactor/tests/test_adapters_impl.c
	$(CC) $(CFLAGS) -DUSE_REAL_IMPL -o $@ $^

# Run performance tests
test-performance: $(PERF_TESTS)
	@echo "⚡ Running Performance Comparison Tests"
	@echo "======================================"
	@./test_real_vs_mock_performance
	@echo "\n✅ Performance tests completed!"

# Coverage report (requires gcov)
test-coverage: CFLAGS += -fprofile-arcs -ftest-coverage
test-coverage: clean $(BDD_TESTS)
	@echo "📊 Running Coverage Analysis"
	@echo "==========================="
	@for test in $(BDD_TESTS); do \
		./$$test > /dev/null 2>&1; \
	done
	@gcov *.c
	@echo "\n✅ Coverage report generated!"

# Benchmark test
test_bitactor_benchmarks: test_bitactor_benchmarks.c
	$(CC) $(CFLAGS) -o $@ $< ../bitactor/src/bitactor.c ../bitactor/src/bitactor_dispatch.c ../bitactor/src/bitactor_telemetry.c ../bitactor/src/bitfiber.c -DUSE_REAL_IMPLEMENTATIONS -I../bitactor/src -I../bitactor/include -lpthread -lm

# Core protection tests
test_core_protection: test_core_protection.c ../src/protection/core_protection.c
	$(CC) $(CFLAGS) -o $@ $^ -lpthread -lm

benchmark_protection: benchmark_protection.c ../src/protection/core_protection.c
	$(CC) $(CFLAGS) -DBENCHMARK_MODE -o $@ $^ -lpthread -lm

stress_test_protection: stress_test_protection.c ../src/protection/core_protection.c
	$(CC) $(CFLAGS) -o $@ $^ -lpthread -lm

adversarial_test: adversarial_test.c ../src/protection/core_protection.c
	$(CC) $(CFLAGS) -o $@ $^ -lpthread -lm

# Run 80/20 protection tests
test-protection: test_core_protection benchmark_protection stress_test_protection adversarial_test
	@echo "🛡️ Running 80/20 Core Protection Tests"
	@echo "====================================="
	@./test_core_protection
	@echo "\n⚡ Running Performance Benchmarks"
	@./benchmark_protection
	@echo "\n💪 Running Stress Tests"
	@./stress_test_protection
	@echo "\n⚔️ Running Adversarial Tests"
	@./adversarial_test
	@echo "\n✅ All protection tests passed!"

# Enhanced protection tests (100% adversarial survival rate)
test_enhanced_protection: test_enhanced_protection.c ../src/protection/enhanced_protection.c ../src/protection/core_protection.c
	$(CC) $(CFLAGS) -o $@ $^ -lpthread -lm

test_enhanced_adversarial: test_enhanced_adversarial.c ../src/protection/enhanced_protection.c ../src/protection/core_protection.c
	$(CC) $(CFLAGS) -o $@ $^ -lpthread -lm

# Run enhanced protection tests with 90%+ survival rate validation
test-enhanced: test_enhanced_protection test_enhanced_adversarial
	@echo "🛡️ Running Enhanced Protection Tests (90%+ Survival Rate Required)"
	@echo "================================================================="
	@echo "\n🧪 Running Enhanced Protection Unit Tests"
	@./test_enhanced_protection
	@echo "\n⚔️ Running Enhanced Adversarial Tests (10 Attack Vectors)"
	@./test_enhanced_adversarial
	@echo "\n✅ Enhanced protection system validated for production deployment"

# Clean build artifacts
clean:
	rm -f $(BDD_TESTS) $(PERF_TESTS) $(REAL_TESTS) $(MOCK_TESTS) test_bitactor_benchmarks *.o *.gcno *.gcda *.gcov
	rm -f test_core_protection benchmark_protection stress_test_protection adversarial_test
	rm -f test_enhanced_protection test_enhanced_adversarial
