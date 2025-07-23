CC = gcc
CFLAGS = -O3 -Wall -Wextra -std=c99
CFLAGS += -I../src -I../bitactor/tests -I../bitactor/include
CFLAGS += -march=native -mtune=native

# Test targets
BDD_TESTS = test_cns_pipeline_bdd test_tick_parallel_bdd test_news_validator_bdd

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