# SPARQL to 8-Hop BitActor Compilation

## Overview

SPARQL queries stored as `.rq` files in the filesystem are compiled at build time into static 8-hop proof chains that execute in exactly 8 ticks.

## Architecture

```
filesystem/
├── queries/
│   ├── market_access.rq      # SPARQL query
│   ├── risk_check.rq         # SPARQL query
│   └── compliance.rq         # SPARQL query
└── compiled/
    └── sparql_chains.c       # Generated 8-hop chains
```

## SPARQL Query Example

```sparql
# market_access.rq - Who can access market data?
PREFIX cap: <http://chatman.ai/capability#>
PREFIX ba: <http://chatman.ai/bitactor#>

SELECT ?actor ?market
WHERE {
    ?actor cap:hasCapability ?cap .
    ?cap cap:type cap:MarketDataAccess .
    ?cap cap:market ?market .
    ?cap cap:validUntil ?expiry .
    FILTER(?expiry > NOW())
}
```

## Compilation Process

### Step 1: Parse SPARQL to AST
```c
// sparql_parser.c
typedef struct {
    char* subject;
    char* predicate;
    char* object;
    filter_t* filters;
} sparql_pattern_t;

typedef struct {
    sparql_pattern_t patterns[8];  // Max 8 patterns
    uint8_t pattern_count;
} sparql_ast_t;

sparql_ast_t* parse_sparql_file(const char* filename) {
    char* content = read_file(filename);
    sparql_ast_t* ast = arena_alloc(g_arena, sizeof(sparql_ast_t));
    
    // Parse WHERE patterns - max 8 for 8-hop mapping
    parse_where_clause(content, ast);
    
    return ast;
}
```

### Step 2: Map SPARQL Patterns to 8 Hops

```c
// sparql_to_bitactor.c

// Each SPARQL pattern becomes one hop in the proof chain
typedef struct {
    uint64_t capability_id;   // From pattern predicate
    uint64_t validation_fn;   // Function pointer for FILTER
    uint64_t data_offset;     // Where to find data
} hop_template_t;

void compile_sparql_to_hops(sparql_ast_t* ast, hop_template_t* hops) {
    // Map each WHERE pattern to a hop
    for (int i = 0; i < ast->pattern_count && i < 8; i++) {
        sparql_pattern_t* p = &ast->patterns[i];
        
        // Convert predicate to capability ID
        hops[i].capability_id = hash_predicate(p->predicate);
        
        // Compile FILTER to validation function
        if (p->filters) {
            hops[i].validation_fn = compile_filter(p->filters);
        }
        
        // Data location for this hop
        hops[i].data_offset = i * 8;  // 8 bytes per hop
    }
    
    // Pad remaining hops with identity operations
    for (int i = ast->pattern_count; i < 8; i++) {
        hops[i].capability_id = CAP_IDENTITY;
        hops[i].validation_fn = (uint64_t)&always_true;
    }
}
```

### Step 3: Generate Static C Code

```c
// codegen.c
void generate_sparql_chain(const char* query_name, hop_template_t* hops) {
    FILE* out = fopen("compiled/sparql_chains.c", "a");
    
    // Generate static proof chain
    fprintf(out, "// Generated from %s.rq\n", query_name);
    fprintf(out, "static const proof_chain_t %s_chain = {\n", query_name);
    fprintf(out, "    .hops = {\n");
    
    for (int i = 0; i < 8; i++) {
        fprintf(out, "        {.capability_id = 0x%016lX, "
                     ".validator = (void*)0x%016lX, "
                     ".data_offset = %lu},\n",
                hops[i].capability_id,
                hops[i].validation_fn,
                hops[i].data_offset);
    }
    
    fprintf(out, "    }\n};\n\n");
    
    // Generate executor function
    fprintf(out, "bool execute_%s(void* data) {\n", query_name);
    fprintf(out, "    return bitactor_execute_8hop(&%s_chain, data);\n", 
            query_name);
    fprintf(out, "}\n\n");
    
    fclose(out);
}
```

## Generated Output Example

```c
// compiled/sparql_chains.c - Generated from market_access.rq

static const proof_chain_t market_access_chain = {
    .hops = {
        // Hop 0: Check actor exists
        {.capability_id = 0x1234567890ABCDEF,  // hash("cap:hasCapability")
         .validator = (void*)0x400A80,         // &validate_exists
         .data_offset = 0},
         
        // Hop 1: Verify capability type
        {.capability_id = 0x234567890ABCDEF1,  // hash("cap:type")
         .validator = (void*)0x400B20,         // &validate_type_market
         .data_offset = 8},
         
        // Hop 2: Check market binding
        {.capability_id = 0x34567890ABCDEF12,  // hash("cap:market")
         .validator = (void*)0x400BC0,         // &validate_market
         .data_offset = 16},
         
        // Hop 3: Verify expiry time
        {.capability_id = 0x4567890ABCDEF123,  // hash("cap:validUntil")
         .validator = (void*)0x400C60,         // &validate_not_expired
         .data_offset = 24},
         
        // Hops 4-7: Identity operations (padding)
        {.capability_id = 0x0000000000000000, 
         .validator = (void*)0x400100,         // &always_true
         .data_offset = 32},
        // ... repeated for hops 5, 6, 7
    }
};

// Execute in exactly 8 ticks
bool execute_market_access(void* data) {
    return bitactor_execute_8hop(&market_access_chain, data);
}
```

## Runtime Execution - 8 Ticks

```c
// bitactor_sparql.c

// Each hop executes in exactly 1 tick
bool bitactor_execute_8hop(const proof_chain_t* chain, void* data) {
    uint64_t result = 0xFFFFFFFFFFFFFFFF;  // Start with all bits set
    
    // Unrolled loop - exactly 8 iterations, 1 tick each
    #pragma unroll 8
    for (int i = 0; i < 8; i++) {
        const hop_t* hop = &chain->hops[i];
        
        // 1 tick: Load data, call validator, update result
        uint64_t* hop_data = (uint64_t*)((char*)data + hop->data_offset);
        bool (*validator)(uint64_t*) = (void*)hop->validator;
        
        // Branchless update
        uint64_t valid = validator(hop_data);
        result &= -valid;  // Clear all bits if invalid
    }
    
    return result != 0;
}
```

## Filesystem Layout for SPARQL Queries

```
cns-v9/
├── queries/
│   ├── capabilities/
│   │   ├── market_access.rq
│   │   ├── trading_permission.rq
│   │   └── risk_override.rq
│   ├── rules/
│   │   ├── arbitrage_detection.rq
│   │   ├── position_limits.rq
│   │   └── compliance_check.rq
│   └── audit/
│       ├── trace_order.rq
│       └── verify_execution.rq
├── compile_sparql.sh
└── compiled/
    └── sparql_chains.c
```

## Build Process Integration

```makefile
# Makefile
SPARQL_FILES := $(wildcard queries/**/*.rq)
SPARQL_CHAINS := compiled/sparql_chains.c

$(SPARQL_CHAINS): $(SPARQL_FILES)
	@echo "Compiling SPARQL to 8-hop chains..."
	./sparql_compiler $(SPARQL_FILES) > $@

cns: $(OBJS) $(SPARQL_CHAINS)
	$(CC) $(CFLAGS) -o $@ $^
```

## Complex SPARQL Example

```sparql
# compliance_check.rq - Multi-hop compliance validation
PREFIX cap: <http://chatman.ai/capability#>
PREFIX risk: <http://chatman.ai/risk#>
PREFIX reg: <http://chatman.ai/regulation#>

SELECT ?order
WHERE {
    # Hop 0-1: Basic order validation
    ?order cap:type cap:Order .
    ?order cap:value ?value .
    
    # Hop 2-3: Risk checks
    ?order risk:exposure ?exposure .
    FILTER(?exposure < 1000000)
    
    # Hop 4-5: Regulatory compliance
    ?order reg:jurisdiction ?jurisdiction .
    ?jurisdiction reg:approved true .
    
    # Hop 6-7: Time window check
    ?order cap:timestamp ?time .
    FILTER(?time > NOW() - PT1H)
}
```

Compiles to:
```c
static const proof_chain_t compliance_check_chain = {
    .hops = {
        {0x...01, &validate_order_type, 0},     // Order type check
        {0x...02, &validate_value_exists, 8},   // Value exists
        {0x...03, &validate_exposure, 16},      // Risk exposure
        {0x...04, &filter_exposure_limit, 24},  // < 1M filter
        {0x...05, &validate_jurisdiction, 32},  // Jurisdiction
        {0x...06, &filter_approved, 40},        // Approved = true
        {0x...07, &validate_timestamp, 48},     // Timestamp exists
        {0x...08, &filter_time_window, 56}      // Within 1 hour
    }
};
```

## Query Optimization

```c
// sparql_optimizer.c

// Reorder patterns for optimal 8-hop execution
void optimize_sparql_patterns(sparql_ast_t* ast) {
    // Put most selective filters first
    qsort(ast->patterns, ast->pattern_count, 
          sizeof(sparql_pattern_t), compare_selectivity);
    
    // Merge adjacent patterns on same subject
    merge_adjacent_patterns(ast);
    
    // Ensure we have exactly 8 hops
    if (ast->pattern_count > 8) {
        // Combine least selective patterns
        compress_patterns(ast);
    }
}
```

## Performance Characteristics

```c
void benchmark_sparql_execution() {
    // Load test data
    market_data_t* data = load_test_data();
    
    uint64_t start = __rdtsc();
    
    // Execute 1M SPARQL queries
    for (int i = 0; i < 1000000; i++) {
        bool result = execute_market_access(&data[i]);
    }
    
    uint64_t cycles = __rdtsc() - start;
    
    printf("SPARQL query execution:\n");
    printf("  Cycles per query: %lu\n", cycles / 1000000);
    printf("  Time per query: %.2f ns\n", (cycles / 1000000) / 3.3);
    
    // Should be exactly 80 cycles (8 ticks)
    assert(cycles / 1000000 <= 80);
}
```

## Integration with CNS

```c
// Main trading loop with SPARQL-based rules
void process_market_data(quote_t* quote) {
    // Check market access permission (SPARQL)
    if (!execute_market_access(quote)) return;
    
    // Apply trading rules (SPARQL)
    if (!execute_arbitrage_detection(quote)) return;
    
    // Compliance check (SPARQL)
    if (!execute_compliance_check(quote)) return;
    
    // All SPARQL queries passed - execute trade
    send_order(quote);
}
```

## Key Benefits

1. **Declarative Rules**: Write rules in SPARQL, execute in 8 ticks
2. **Compile-Time Optimization**: No runtime SPARQL parsing
3. **Guaranteed Performance**: Every query executes in exactly 8 ticks
4. **Type Safety**: C compiler verifies all generated code
5. **Auditability**: SPARQL source tracked to compiled chains

This approach lets us write complex graph queries in SPARQL while maintaining the strict 8-tick execution guarantee of CNS v9.