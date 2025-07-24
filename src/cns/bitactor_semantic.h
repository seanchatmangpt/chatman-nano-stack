#ifndef BITACTOR_SEMANTIC_H
#define BITACTOR_SEMANTIC_H

#include "bitactor.h"
#include <stdint.h>
#include <stdbool.h>

// Semantic BitActor Integration Layer
// Connects TTL/OWL/SHACL/SPARQL with 8-tick BitActor runtime

// Core semantic signal structure
typedef struct {
    signal_t base;              // Base BitActor signal
    uint32_t subject_hash;      // RDF subject hash
    uint32_t predicate_hash;    // RDF predicate hash  
    uint32_t object_hash;       // RDF object hash
    uint32_t context_id;        // Triple store context ID
    uint8_t semantic_flags;     // Semantic processing flags
} semantic_signal_t;

// Semantic handler registration
typedef struct {
    handler_fn base_handler;    // BitActor handler function
    uint32_t predicate_hash;    // Handled predicate hash
    uint8_t tick_budget;        // Performance budget
    bool vectorizable;          // SIMD capable
    uint8_t batch_size;         // Optimal batch size
} semantic_handler_t;

// Triple store for 8-tick access
typedef struct {
    uint32_t* subject_index;    // Hash-indexed subjects
    uint32_t* predicate_index;  // Hash-indexed predicates
    uint32_t* object_index;     // Hash-indexed objects
    uint32_t triple_count;      // Number of triples
    uint32_t index_size;        // Index table size
    uint8_t* triple_data;       // Packed triple data
} fast_triple_store_t;

// SHACL validation context
typedef struct {
    uint32_t shape_count;       // Number of validation shapes
    uint32_t* shape_hashes;     // Shape identifier hashes
    uint8_t* constraint_data;   // Packed constraint rules
    uint32_t constraint_size;   // Total constraint data size
} shacl_validator_t;

// SPARQL query compilation
typedef struct {
    uint32_t query_hash;        // Query identifier hash
    uint8_t* bytecode;          // Compiled BitActor bytecode
    uint32_t bytecode_size;     // Bytecode length
    uint8_t complexity;         // Estimated tick count
    bool cached;                // Pre-compiled flag
} compiled_query_t;

// Semantic engine state
typedef struct {
    bitactor_t* base_engine;        // Base BitActor engine
    fast_triple_store_t* kb;        // Knowledge base
    shacl_validator_t* validator;   // SHACL validator
    compiled_query_t* queries;      // Compiled SPARQL queries
    uint32_t query_count;           // Number of cached queries
    semantic_handler_t* handlers;   // Semantic handlers
    uint32_t handler_count;         // Number of handlers
    uint8_t* scratch_semantic;      // Semantic scratch memory
} semantic_engine_t;

// Core API - maintains 8-tick guarantee
semantic_engine_t* semantic_engine_init(void);
void semantic_engine_destroy(semantic_engine_t* engine);

// Signal processing with semantic enrichment (≤2 ticks)
result_t semantic_process_signal(semantic_engine_t* engine, semantic_signal_t* signal);

// Triple store operations (≤1 tick each)
bool semantic_store_triple(semantic_engine_t* engine, 
                          uint32_t subject, uint32_t predicate, uint32_t object);
bool semantic_lookup_triple(semantic_engine_t* engine,
                           uint32_t subject, uint32_t predicate, uint32_t* object);

// SHACL validation (≤2 ticks)
bool semantic_validate_signal(semantic_engine_t* engine, semantic_signal_t* signal);

// SPARQL query execution (≤4 ticks for simple, ≤8 for complex)
result_t semantic_execute_query(semantic_engine_t* engine, uint32_t query_hash, 
                               uint32_t* bindings, uint32_t max_bindings);

// Handler registration (initialization only)
bool semantic_register_handler(semantic_engine_t* engine, 
                              semantic_handler_t* handler);

// Hash utilities for RDF terms
uint32_t semantic_hash_uri(const char* uri);
uint32_t semantic_hash_literal(const char* literal);
uint32_t semantic_hash_predicate(const char* predicate);

// Perfect hash dispatch for semantic signals
static inline uint32_t semantic_dispatch_hash(semantic_signal_t* signal) {
    return signal->predicate_hash & (BITACTOR_DISPATCH_SIZE - 1);
}

// Fast triple pattern matching
static inline bool semantic_match_pattern(fast_triple_store_t* kb,
                                         uint32_t s, uint32_t p, uint32_t o) {
    if (s != 0 && p != 0 && o != 0) {
        // SPO lookup - O(1)
        uint32_t index = (s ^ p ^ o) % kb->index_size;
        return kb->subject_index[index] == s && 
               kb->predicate_index[index] == p &&
               kb->object_index[index] == o;
    }
    return false; // Pattern queries require iteration
}

// SIMD batch processing for semantic signals
typedef struct {
    semantic_signal_t* signals[32];  // SIMD-aligned batch
    uint32_t count;                  // Signals in batch
    uint32_t handler_hash;           // Common handler
} semantic_batch_t;

result_t semantic_process_batch(semantic_engine_t* engine, semantic_batch_t* batch);

// Telemetry for semantic operations
typedef struct {
    uint64_t semantic_signals_processed;
    uint64_t triples_accessed;
    uint64_t validations_performed;
    uint64_t queries_executed;
    uint64_t avg_semantic_ticks;
    uint64_t max_semantic_ticks;
} semantic_telemetry_t;

void semantic_get_telemetry(semantic_engine_t* engine, semantic_telemetry_t* telemetry);

// Memory-mapped ontology loading for zero-allocation runtime
typedef struct {
    void* mapped_memory;        // Memory-mapped TTL file
    size_t file_size;           // File size in bytes
    uint32_t* term_index;       // Hash index into file
    uint32_t term_count;        // Number of indexed terms
} mmapped_ontology_t;

mmapped_ontology_t* semantic_load_ontology(const char* ttl_file);
void semantic_unload_ontology(mmapped_ontology_t* ontology);

// News validation integration (specialized for production use)
typedef struct {
    uint32_t article_id;
    uint32_t source_hash;
    uint32_t author_hash;
    uint64_t timestamp;
    double credibility_score;
    bool validated;
} news_semantic_signal_t;

result_t semantic_validate_news(semantic_engine_t* engine, news_semantic_signal_t* news);

// Performance optimization flags
#define SEMANTIC_FLAG_CACHED        0x01    // Use cached results
#define SEMANTIC_FLAG_VECTORIZE     0x02    // Enable SIMD processing
#define SEMANTIC_FLAG_NO_VALIDATION 0x04    // Skip SHACL validation
#define SEMANTIC_FLAG_FAST_LOOKUP   0x08    // Use hash-only lookup
#define SEMANTIC_FLAG_BATCH_MODE    0x10    // Batch multiple signals

// Error codes for semantic operations
typedef enum {
    SEMANTIC_OK = 0,
    SEMANTIC_INVALID_SIGNAL = 1,
    SEMANTIC_VALIDATION_FAILED = 2,
    SEMANTIC_QUERY_TIMEOUT = 3,
    SEMANTIC_KB_FULL = 4,
    SEMANTIC_HANDLER_NOT_FOUND = 5
} semantic_status_t;

// Inline helpers for performance-critical paths
__attribute__((always_inline))
static inline bool semantic_is_valid_hash(uint32_t hash) {
    return hash != 0 && hash != UINT32_MAX;
}

__attribute__((always_inline))
static inline uint8_t semantic_estimate_ticks(semantic_signal_t* signal) {
    uint8_t base_ticks = 1;  // Base processing
    
    if (signal->semantic_flags & SEMANTIC_FLAG_NO_VALIDATION) {
        return base_ticks;
    }
    
    base_ticks += 2;  // SHACL validation
    
    if (signal->context_id != 0) {
        base_ticks += 1;  // Context lookup
    }
    
    return base_ticks;
}

// Context-aware processing for different domains
typedef enum {
    SEMANTIC_CONTEXT_NEWS = 1,
    SEMANTIC_CONTEXT_FINANCIAL = 2,
    SEMANTIC_CONTEXT_GENERAL = 3
} semantic_context_type_t;

// Domain-specific optimization
result_t semantic_process_with_context(semantic_engine_t* engine,
                                     semantic_signal_t* signal,
                                     semantic_context_type_t context_type);

#endif // BITACTOR_SEMANTIC_H