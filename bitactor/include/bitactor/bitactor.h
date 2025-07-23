/*
 * BitActor - Deterministic Causal Interface for CNS
 * Zero-allocation, ≤8 tick execution guarantee
 * 
 * Core design principles:
 * - All memory statically allocated at init
 * - Branchless execution paths
 * - Fixed CPU tick budget enforcement
 * - 100% reversible telemetry traces
 */
#ifndef BITACTOR_H
#define BITACTOR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Configuration - all sizes fixed at compile time */
#define BITACTOR_MAX_SIGNALS     1024    /* Signal ring buffer size */
#define BITACTOR_MAX_FIBERS      32      /* Max concurrent fibers */
#define BITACTOR_TICK_BUDGET     8       /* CPU tick limit per operation */
#define BITACTOR_DISPATCH_SIZE   256     /* Perfect hash dispatch table */
#define BITACTOR_SCRATCH_SIZE    2048    /* Per-fiber scratch memory */
#define BITACTOR_TELEMETRY_SIZE  4096    /* Telemetry ring frames */

/* Status codes for deterministic error handling */
typedef enum {
    BITACTOR_OK              = 0x00,
    BITACTOR_TICK_EXCEEDED   = 0x01,
    BITACTOR_INVALID_SIGNAL  = 0x02,
    BITACTOR_BUFFER_FULL     = 0x03,
    BITACTOR_NO_HANDLER      = 0x04,
    BITACTOR_FIBER_LIMIT     = 0x05
} bitactor_status_t;

/* Fixed-size signal structure - 32 bytes aligned */
typedef struct __attribute__((packed)) {
    uint32_t id;          /* Unique signal identifier */
    uint8_t  kind;        /* Signal type for dispatch (0-255) */
    uint8_t  priority;    /* Execution priority (0-255) */
    uint16_t flags;       /* Signal flags/metadata */
    uint64_t payload;     /* Primary data payload */
    uint64_t timestamp;   /* Nanosecond timestamp */
    uint64_t context;     /* Additional context data */
} signal_t;

/* Execution result - 24 bytes */
typedef struct __attribute__((packed)) {
    uint32_t signal_id;   /* Original signal ID */
    uint32_t exec_hash;   /* Execution path hash */
    uint8_t  status;      /* Execution status code */
    uint8_t  ticks;       /* CPU ticks consumed */
    uint16_t flags;       /* Result flags */
    uint64_t result;      /* Result payload */
    uint32_t fiber_id;    /* Executing fiber ID */
} result_t;

/* Handler function prototype - executes in bounded ticks */
typedef result_t (*bitactor_handler_fn)(signal_t* signal, void* scratch);

/* Forward declarations */
typedef struct bitactor_engine bitactor_engine_t;
typedef struct fiber_context fiber_context_t;

/* Core API - minimal surface for maximum performance */
bitactor_engine_t* bitactor_init(void);
void bitactor_destroy(bitactor_engine_t* engine);

/* Main execution - GUARANTEED ≤8 CPU ticks */
result_t bitactor_tick(bitactor_engine_t* engine, signal_t* signal);

/* Batch operations for efficiency */
bool bitactor_enqueue(bitactor_engine_t* engine, signal_t* signal);
uint32_t bitactor_drain(bitactor_engine_t* engine, uint32_t max_signals);

/* Handler registration - static dispatch table */
int bitactor_register(bitactor_engine_t* engine, 
                      uint8_t kind, 
                      bitactor_handler_fn handler);

/* Engine introspection */
uint32_t bitactor_pending_count(const bitactor_engine_t* engine);
bool bitactor_is_ready(const bitactor_engine_t* engine);
void bitactor_stats(const bitactor_engine_t* engine, void* stats_out);

/* Inline helpers for zero-overhead operations */
static inline uint64_t bitactor_rdtsc(void) {
#if defined(__x86_64__) || defined(__i386__)
    uint32_t lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return ((uint64_t)hi << 32) | lo;
#else
    // Fallback for non-x86 architectures
    return 0;
#endif
}

/* Branchless min/max */
static inline uint32_t bitactor_min(uint32_t a, uint32_t b) {
    return b ^ ((a ^ b) & -(a < b));
}

static inline uint32_t bitactor_max(uint32_t a, uint32_t b) {
    return a ^ ((a ^ b) & -(a < b));
}

#endif /* BITACTOR_H */