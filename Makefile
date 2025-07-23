# CNS Makefile - Integrated with uv package management
# Built for reliability. Designed to last.

# Python environment (managed by uv)
PYTHON_ENV = .venv/bin/python
UV = uv

# C compiler settings
CC = clang
CFLAGS = -O3 -Wall -march=native -falign-functions=64
LDFLAGS = -lm -pthread

SPARQL_SRCS = src/sparql/sparql_parser.c src/sparql/sparql_to_bitactor.c src/sparql/sparql_codegen.c
CNS_SRCS = src/cns/tick_parallel.c src/cns/cns_pipeline.c
BENCHMARK_SRCS = src/benchmark/otel_benchmark.c src/benchmark/benchmark_main.c

# Main targets
all: setup sparql_compiler benchmark test_sparql

# Python environment setup
setup: .venv/pyvenv.cfg
	@echo "✓ Python environment ready"

.venv/pyvenv.cfg:
	@echo "Setting up Python environment with uv..."
	$(UV) venv
	$(UV) sync

# Install development dependencies
dev-setup: setup
	$(UV) sync --extra dev --extra benchmark --extra trading

# Python tools (run in uv environment)
owl-compile: setup
	$(UV) run python owl_compiler.py $(ARGS)

ttl2dspy: setup
	$(UV) run python ttl2dspy.py $(ARGS)

python-benchmark: setup
	$(UV) run python run_benchmark.py

python-test: setup
	$(UV) run pytest $(ARGS)

lint: setup
	$(UV) run ruff check .
	$(UV) run black --check .
	$(UV) run mypy .

format: setup
	$(UV) run ruff check --fix .
	$(UV) run black .

# C compilation targets (unchanged)

sparql_compiler: src/sparql/sparql_compiler.c $(SPARQL_SRCS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

src/sparql/sparql_chains.c: sparql_compiler queries/*.rq
	./sparql_compiler queries/*.rq

benchmark: $(BENCHMARK_SRCS) $(CNS_SRCS) src/sparql/sparql_to_bitactor.c src/sparql/sparql_chains.c
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

test_sparql: queries/market_access.rq queries/compliance_check.rq

queries/market_access.rq:
	@mkdir -p queries
	@echo "PREFIX cap: <http://chatman.ai/capability#>" > $@
	@echo "PREFIX ba: <http://chatman.ai/bitactor#>" >> $@
	@echo "" >> $@
	@echo "SELECT ?actor ?market" >> $@
	@echo "WHERE {" >> $@
	@echo "    ?actor cap:hasCapability ?cap ." >> $@
	@echo "    ?cap cap:type cap:MarketDataAccess ." >> $@
	@echo "    ?cap cap:market ?market ." >> $@
	@echo "    ?cap cap:validUntil ?expiry ." >> $@
	@echo "    FILTER(?expiry > NOW())" >> $@
	@echo "}" >> $@

queries/compliance_check.rq:
	@mkdir -p queries
	@echo "PREFIX cap: <http://chatman.ai/capability#>" > $@
	@echo "PREFIX risk: <http://chatman.ai/risk#>" >> $@
	@echo "" >> $@
	@echo "SELECT ?order" >> $@
	@echo "WHERE {" >> $@
	@echo "    ?order cap:type cap:Order ." >> $@
	@echo "    ?order cap:value ?value ." >> $@
	@echo "    ?order risk:exposure ?exposure ." >> $@
	@echo "    FILTER(?exposure < 1000000)" >> $@
	@echo "}" >> $@

run_benchmark: benchmark
	./benchmark

# Combined targets
full-test: python-test run_benchmark
	@echo "✓ All tests passed"

full-benchmark: python-benchmark run_benchmark
	@echo "✓ All benchmarks completed"

# Cleaning
clean: clean-c clean-python
	rm -rf queries/

clean-c:
	rm -f sparql_compiler benchmark
	rm -f src/sparql/sparql_chains.c

clean-python:
	rm -rf .venv
	rm -rf .pytest_cache
	rm -rf __pycache__
	find . -name "*.pyc" -delete
	find . -name "*.pyo" -delete

# Docker support
docker-build:
	docker build -t cns:latest .

docker-run: docker-build
	docker run --rm -it cns:latest

# Help
help:
	@echo "CNS Build System - James I. Chatman & Sean A. Chatman"
	@echo ""
	@echo "Python targets (managed by uv):"
	@echo "  setup           - Initialize Python environment"
	@echo "  dev-setup       - Setup with development dependencies"
	@echo "  owl-compile     - Run OWL compiler (ARGS=...)"
	@echo "  ttl2dspy        - Run TTL to DSPy converter (ARGS=...)"
	@echo "  python-test     - Run Python tests (ARGS=...)"
	@echo "  python-benchmark- Run Python benchmarks"
	@echo "  lint            - Check code quality"
	@echo "  format          - Format code"
	@echo ""
	@echo "C targets:"
	@echo "  sparql_compiler - Build SPARQL compiler"
	@echo "  benchmark       - Build C benchmark suite"
	@echo "  run_benchmark   - Execute C benchmarks"
	@echo ""
	@echo "Combined targets:"
	@echo "  all             - Build everything"
	@echo "  full-test       - Run all tests (Python + C)"
	@echo "  full-benchmark  - Run all benchmarks"
	@echo "  clean           - Clean all build artifacts"

.PHONY: all setup dev-setup owl-compile ttl2dspy python-test python-benchmark lint format
.PHONY: full-test full-benchmark clean clean-c clean-python docker-build docker-run help
.PHONY: sparql_compiler benchmark run_benchmark test_sparql