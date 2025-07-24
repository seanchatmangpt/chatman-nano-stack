# BitActor Makefile - Production Build with C NIF
# Ultra-High-Frequency Trading Engine

CC = clang
CFLAGS = -O3 -march=native -mtune=native -fPIC -Wall -Wextra
CFLAGS += -I/usr/local/opt/erlang/lib/erlang/erts-*/include
LDFLAGS = -shared -undefined dynamic_lookup

# Platform-specific optimizations
UNAME_M := $(shell uname -m)
ifeq ($(UNAME_M),x86_64)
	CFLAGS += -mavx2 -mfma
endif
ifeq ($(UNAME_M),arm64)
	CFLAGS += -mcpu=native
endif

# Directories
SRC_DIR = c_src
PRIV_DIR = priv
BUILD_DIR = _build

# Files
C_SRC = $(SRC_DIR)/bitactor_nif_portable.c
NIF_SO = $(PRIV_DIR)/bitactor_nif.so

# Find Erlang include directory
ERL_INCLUDE := $(shell erl -eval 'io:format("~s", [code:lib_dir(erl_interface, include)])' -s init stop -noshell)
ERTS_INCLUDE := $(shell erl -eval 'io:format("~s", [code:root_dir() ++ "/erts-" ++ erlang:system_info(version) ++ "/include"])' -s init stop -noshell)

CFLAGS += -I$(ERL_INCLUDE) -I$(ERTS_INCLUDE)

.PHONY: all compile test benchmark clean dialyzer release

# Default target
all: compile

# Create directories
$(PRIV_DIR):
	@mkdir -p $(PRIV_DIR)

# Compile C NIF
$(NIF_SO): $(C_SRC) | $(PRIV_DIR)
	@echo "Compiling C NIF with SIMD optimizations..."
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<
	@echo "NIF compiled: $@"

# Compile Erlang code
compile: $(NIF_SO)
	@echo "Compiling Erlang code..."
	rebar3 compile

# Run tests
test: compile
	@echo "Running unit tests..."
	rebar3 eunit
	@echo "Running Common Test suites..."
	rebar3 ct

# Run benchmarks
benchmark: compile
	@echo "Running UHFT benchmarks..."
	erl -pa _build/default/lib/*/ebin -noshell -s bitactor_benchmark run_all -s init stop

# Run benchmarks with detailed output
benchmark-verbose: compile
	@echo "Running detailed UHFT benchmarks..."
	erl -pa _build/default/lib/*/ebin -eval 'bitactor_benchmark:run_all(#{verbose => true}).' -s init stop

# Quick performance test
perf-test: compile
	@echo "Quick performance validation..."
	erl -pa _build/default/lib/*/ebin -noshell -eval '\
		application:ensure_all_started(bitactor), \
		io:format("~nQuick Performance Test~n"), \
		{ok, _, LatencyNs} = bitactor_server:spawn_actor(test, #{}), \
		io:format("Spawn latency: ~p ns~n", [LatencyNs]), \
		{ok, Min, Avg, Max} = bitactor_nif:measure_latency(), \
		io:format("RDTSC overhead: Min=~p ns, Avg=~p ns, Max=~p ns~n", [Min, Avg, Max]), \
		init:stop().'

# Run stress test
stress-test: compile
	@echo "Running stress test..."
	erl -pa _build/default/lib/*/ebin -noshell -eval '\
		application:ensure_all_started(bitactor), \
		bitactor_benchmark:stress_test_actors(10000), \
		init:stop().'

# Dialyzer
dialyzer: compile
	rebar3 dialyzer

# Build release
release: compile test
	@echo "Building production release..."
	rebar3 as prod release

# Clean
clean:
	@echo "Cleaning..."
	rm -rf _build
	rm -f $(NIF_SO)
	rm -rf priv
	rebar3 clean

# Development shell
shell: compile
	@echo "Starting Erlang shell with BitActor..."
	rebar3 shell

# Check C compiler and flags
check-env:
	@echo "Checking build environment..."
	@echo "CC: $(CC)"
	@echo "CFLAGS: $(CFLAGS)"
	@echo "ERL_INCLUDE: $(ERL_INCLUDE)"
	@echo "ERTS_INCLUDE: $(ERTS_INCLUDE)"
	@which $(CC) > /dev/null || (echo "Error: $(CC) not found"; exit 1)
	@echo "CPU features:"
	@sysctl -n machdep.cpu.features 2>/dev/null || echo "Unable to detect CPU features"

# Production build with maximum optimization
prod: CFLAGS += -DNDEBUG -flto
prod: clean compile test benchmark
	@echo "Production build complete!"

# Debug build
debug: CFLAGS += -g -DDEBUG -fsanitize=address
debug: compile
	@echo "Debug build complete"

# Profile-guided optimization build
pgo-generate: CFLAGS += -fprofile-generate
pgo-generate: compile

pgo-use: CFLAGS += -fprofile-use
pgo-use: compile

# Help
help:
	@echo "BitActor Makefile targets:"
	@echo "  make              - Compile everything"
	@echo "  make test         - Run all tests"
	@echo "  make benchmark    - Run performance benchmarks"
	@echo "  make perf-test    - Quick performance validation"
	@echo "  make stress-test  - Run stress test with 10k actors"
	@echo "  make shell        - Start Erlang shell"
	@echo "  make release      - Build production release"
	@echo "  make clean        - Clean build artifacts"
	@echo "  make check-env    - Check build environment"
	@echo "  make prod         - Production build with optimizations"
	@echo "  make debug        - Debug build with sanitizers"