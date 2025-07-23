CC = gcc
CFLAGS = -O3 -Wall -Wextra -std=c99
CFLAGS += -I../src -I../bitactor/tests -I../bitactor/include
CFLAGS += -march=native -mtune=native

# Test targets
BDD_TESTS = test_cns_pipeline_bdd test_tick_parallel_bdd test_news_validator_bdd test_sparql_parser_bdd test_generated_uhft_bdd test_bitactor_core_extended_bdd test_sparql_complete_bdd test_uhft_comprehensive_bdd test_bitactor_advanced_bdd test_algorithms_8tick_bdd test_benchmarks_validation_bdd

# Source files to test
CNS_SRCS = ../src/cns/cns_pipeline.c ../src/cns/tick_parallel.c
NEWS_SRCS = ../src/news/news_validator.c

.PHONY: all clean test-bdd run-all

all: $(BDD_TESTS)

# BDD Tests for existing C code
test_cns_pipeline_bdd: test_cns_pipeline_bdd.c $(CNS_SRCS) mock_implementations.c
	$(CC) $(CFLAGS) -o $@ $^

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

# Run all BDD tests
test-bdd: $(BDD_TESTS)
	@echo "🧪 Running CNS Codebase BDD Tests"
	@echo "================================="
	@for test in $(BDD_TESTS); do \
		echo "\n🔍 Testing $$test..."; \
		./$$test || exit 1; \
	done
	@echo "\n✅ All CNS BDD tests passed!"

# Run tests with detailed output
run-all: $(BDD_TESTS)
	@for test in $(BDD_TESTS); do \
		echo "\n" | ./$$test; \
	done

# Clean build artifacts
clean:
	rm -f $(BDD_TESTS) *.o