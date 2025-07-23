CC = gcc
CFLAGS = -O3 -ffreestanding -fno-exceptions -Wall -Wextra
CFLAGS += -march=native -mtune=native
CFLAGS += -I../include -I.
CFLAGS += -DENABLE_ASSERT

# Test targets
TESTS = test_bitactor_core test_compiler test_telemetry test_memory test_performance

# BDD test targets
BDD_TESTS = test_bitactor_bdd test_bitactor_performance_bdd test_bitactor_telemetry_bdd

# All tests
ALL_TESTS = $(TESTS) $(BDD_TESTS)

# Object files
OBJS = ../src/bitactor.o

# Mock implementations for BDD tests
MOCK_OBJS = mock_bitactor.o mock_telemetry.o

.PHONY: all clean test test-bdd valgrind perf

all: $(ALL_TESTS)

# Build BitActor library
../src/bitactor.o: ../src/bitactor.c
	$(CC) $(CFLAGS) -c $< -o $@

# Build mock objects for BDD tests
mock_bitactor.o: mock_bitactor.c
	$(CC) $(CFLAGS) -c $< -o $@

mock_telemetry.o: mock_telemetry.c
	$(CC) $(CFLAGS) -c $< -o $@

# Build traditional tests
test_bitactor_core: test_bitactor_core.c $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

test_compiler: test_compiler.c
	$(CC) $(CFLAGS) -o $@ $<

test_telemetry: test_telemetry.c $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

test_memory: test_memory.c $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

test_performance: test_performance.c $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

# Build BDD tests
test_bitactor_bdd: test_bitactor_bdd.c $(MOCK_OBJS)
	$(CC) $(CFLAGS) -o $@ $^

test_bitactor_performance_bdd: test_bitactor_performance_bdd.c $(MOCK_OBJS)
	$(CC) $(CFLAGS) -o $@ $^

test_bitactor_telemetry_bdd: test_bitactor_telemetry_bdd.c $(MOCK_OBJS)
	$(CC) $(CFLAGS) -o $@ $^

# Run all tests
test: $(TESTS)
	@echo "🚀 Running BitActor Test Suite"
	@echo "=============================="
	@for test in $(TESTS); do \
		echo "\n📋 Running $$test..."; \
		./$$test || exit 1; \
	done
	@echo "\n✅ All tests passed!"

# Run BDD tests
test-bdd: $(BDD_TESTS)
	@echo "🧪 Running BitActor BDD Specifications"
	@echo "====================================="
	@for test in $(BDD_TESTS); do \
		echo "\n"; \
		./$$test || exit 1; \
	done
	@echo "\n✅ All BDD specifications passed!"

# Run all tests including BDD
test-all: test test-bdd

# Memory analysis
valgrind: test_bitactor_core
	valgrind --tool=massif --pages-as-heap=yes ./test_bitactor_core
	@echo "Check massif.out.* for heap analysis"

# Performance profiling
perf: test_performance
	perf stat -e cycles,instructions,cache-misses ./test_performance
	perf record -F 99 ./test_performance
	perf report

# Check for branches in assembly
check-branches: ../src/bitactor.o
	@echo "Checking for conditional branches..."
	@objdump -d ../src/bitactor.o | grep -E '\b(jne|je|jz|jnz|ja|jb)\b' || echo "✅ No conditional branches found"

# Generate BDD test report
bdd-report: $(BDD_TESTS)
	@echo "# BitActor BDD Test Report" > bdd_report.md
	@echo "Generated: $$(date)" >> bdd_report.md
	@echo "" >> bdd_report.md
	@for test in $(BDD_TESTS); do \
		echo "## $$test" >> bdd_report.md; \
		echo '```' >> bdd_report.md; \
		./$$test >> bdd_report.md 2>&1; \
		echo '```' >> bdd_report.md; \
		echo "" >> bdd_report.md; \
	done

# Clean build artifacts
clean:
	rm -f $(ALL_TESTS) $(OBJS) $(MOCK_OBJS) *.o
	rm -f massif.out.* perf.data*
	rm -f test_report_*.json bdd_report.md