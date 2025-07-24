#ifndef SEMANTIC_BITACTOR_INTEGRATION_H
#define SEMANTIC_BITACTOR_INTEGRATION_H

/*
 * Production Semantic BitActor Integration
 * Generated from TTL ontologies with 8-tick performance guarantee
 */

#include <stdint.h>
#include <stdbool.h>

// DSPy Signature Mappings to C Structures

// SemanticSignalSignature -> semantic_signal_t
typedef struct {
    uint32_t has_subject;       // Subject hash
    uint32_t has_predicate;     // Predicate hash  
    uint32_t has_object;        // Object hash
    uint32_t has_semantic_context; // Context ID
} semantic_signal_data_t;

// HandlerSignature -> handler_config_t
typedef struct {
    uint8_t has_tick_budget;    // Tick budget (1-8)
    bool vectorizable;          // SIMD capability
    uint8_t batch_size;         // Batch size (4,8,16,32)
    uint32_t has_hash;          // Dispatch hash
} handler_config_t;

// ExecutionResultSignature -> execution_metrics_t
typedef struct {
    uint8_t actual_ticks;       // Measured ticks
    char execution_status[16];  // Status string
    uint32_t has_trace_id;      // Trace ID
} execution_metrics_t;

// MemoryPoolSignature -> memory_config_t
typedef struct {
    uint32_t pool_size;         // Pool size in bytes
    uint8_t alignment_bytes;    // Memory alignment
} memory_config_t;

// SPARQLQuerySignature -> query_config_t
typedef struct {
    uint32_t query_complexity;  // Tick complexity (1-8)
} query_config_t;

// Production API
int semantic_bitactor_init(void);
int semantic_process_dspy_signal(semantic_signal_data_t* signal, 
                                execution_metrics_t* result);
int semantic_register_dspy_handler(handler_config_t* config);
void semantic_bitactor_shutdown(void);

// Performance validation
bool semantic_validate_8tick_guarantee(void);

#endif // SEMANTIC_BITACTOR_INTEGRATION_H
