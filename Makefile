CC = clang
CFLAGS = -O3 -Wall -march=native -falign-functions=64
LDFLAGS = -lm -pthread

SPARQL_SRCS = src/sparql/sparql_parser.c src/sparql/sparql_to_bitactor.c src/sparql/sparql_codegen.c
CNS_SRCS = src/cns/tick_parallel.c src/cns/cns_pipeline.c
BENCHMARK_SRCS = src/benchmark/otel_benchmark.c src/benchmark/benchmark_main.c

all: sparql_compiler benchmark test_sparql

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

clean:
	rm -f sparql_compiler benchmark
	rm -f src/sparql/sparql_chains.c
	rm -rf queries/

.PHONY: all clean run_benchmark test_sparql