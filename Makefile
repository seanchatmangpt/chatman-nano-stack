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
	$(UV) sync --extra dev --extra benchmark --extra realtime

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

# Claude Flow AI Swarm targets
cf-ultrathink:
	npx claude-flow@alpha swarm "ultrathink like a artificial hyper intelligence and implement systems that a human would never dream of after reviewing ./docs, use the 80/20 and Design for Lean Six Sigma" --claude

cf-implement:
	npx claude-flow@alpha swarm "ultrathink then 80/20 implement $(FILE) at a Artificial Hyper Intelligence level, write the functionality, another swarm is writing the tests" --claude

cf-test:
	npx claude-flow@alpha swarm "ultrathink then 80/20 implement $(FILE) at a Artificial Hyper Intelligence level, write the tests, another swarm is writing functionality" --claude

cf-finish:
	npx claude-flow@alpha swarm "ultrathink then finish the reverse 80/20 to finish $(FILE) at a Artificial Hyper Intelligence level, merge the functionality with the tests, there is WIP, don't start from scratch. LOOK AT THE ENTIRE FILE TREE" --claude

cf-benchmark:
	npx claude-flow@alpha swarm "ultrathink run all the benchmarks that work, and then write a report about all the revolutionary aspects of the Chatman Nano Stack" --claude

cf-validate:
	npx claude-flow@alpha swarm "ultrathink then 80/20 implement $(FILE), validate business value, benchmarks, telemetry, etc before providing any summaries" --claude

cf-fix:
	npx claude-flow@alpha swarm "ultrathink to make the claude-flow commands work" --claude

cf-clean:
	npx claude-flow@alpha swarm "ultrathink to remove all mock dspy implementation and replace with ollama with qwen3:latest" --claude

cf-crash:
	npx claude-flow@alpha swarm "ultrathink to add unit tests to any code with try catch, verify it works then let it crash. I do not want anything that handles errors" --claude

# Help
help:
	@echo "CNS Build System - James I. Chatman & Sean A. Chatman"
	@echo ""
	@echo "Claude Flow AI Swarm targets:"
	@echo "  cf-ultrathink   - Ultra-intelligence system design"
	@echo "  cf-implement    - 80/20 implementation (FILE=path)"
	@echo "  cf-test         - 80/20 test generation (FILE=path)"
	@echo "  cf-finish       - Merge functionality with tests (FILE=path)"
	@echo "  cf-benchmark    - Run benchmarks and generate report"
	@echo "  cf-validate     - Validate with telemetry (FILE=path)"
	@echo "  cf-fix          - Fix claude-flow command issues"
	@echo "  cf-clean        - Replace mock with ollama qwen3"
	@echo "  cf-crash        - Add crash-first unit tests"
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
.PHONY: cf-ultrathink cf-implement cf-test cf-finish cf-benchmark cf-validate cf-fix cf-clean cf-crash

include Makefile.coverage
