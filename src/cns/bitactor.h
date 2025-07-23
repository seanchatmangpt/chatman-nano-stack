#ifndef BITACTOR_H
#define BITACTOR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <immintrin.h>

// Include BitActor subsystem headers
#include "../../bitactor/include/bitactor/bitactor_blake3.h"

// Core constants - all power of 2 for bit manipulation efficiency
#define BITACTOR_MAX_SIGNALS      256
#define BITACTOR_RING_SIZE        4096
#define BITACTOR_SCRATCH_SIZE     2048
#define BITACTOR_DISPATCH_SIZE    1024
#define BITACTOR_TELEMETRY_SIZE   65536
#define BITACTOR_MAX_BYTECODE     32768
#define BITACTOR_TICK_BUDGET      8

// Signal types for external events
typedef struct {
    uint32_t kind;      // Signal type for dispatch
    uint32_t flags;     // Control flags
    uint64_t timestamp; // Arrival timestamp
    uint64_t payload;   // Signal data
} signal_t;

// Bitcode instruction - 4 bytes for cache alignment
typedef struct {
    uint8_t opcode;     // Operation code
    uint8_t dst;        // Destination register
    uint8_t src1;       // Source register 1
    uint8_t src2;       // Source register 2
} bitinstr_t;

// Telemetry frame for reversible tracing
typedef struct {
    uint64_t timestamp;
    uint32_t signal_id;
    uint32_t exec_hash;
    uint8_t trace_ops[16];
} telemetry_frame_t;

// Static dispatch entry
typedef void (*handler_fn)(signal_t* sig, void* scratch);

// BitActor state - all memory pre-allocated
typedef struct {
    // Signal ring buffer (lockfree SPSC)
    signal_t signal_ring[BITACTOR_RING_SIZE];
    volatile uint32_t signal_head;
    volatile uint32_t signal_tail;
    
    // Fiber scratch memory
    uint8_t scratch[BITACTOR_SCRATCH_SIZE] __attribute__((aligned(64)));
    
    // Static dispatch table (perfect hash)
    handler_fn dispatch[BITACTOR_DISPATCH_SIZE];
    
    // Bitcode program memory
    bitinstr_t bytecode[BITACTOR_MAX_BYTECODE];
    uint32_t bytecode_size;
    
    // Telemetry ring buffer
    telemetry_frame_t telemetry[BITACTOR_TELEMETRY_SIZE];
    volatile uint32_t telemetry_head;
    
    // Hash verification state
    bitactor_hash_state_t hash_state;
    
    // Performance counters
    uint64_t tick_count;
    uint64_t signal_count;
    uint64_t cycle_count;
    uint64_t budget_exceeded_count;
} bitactor_t;

// Core API - minimal surface area
void bitactor_init(bitactor_t* ba);
void bitactor_tick(bitactor_t* ba);
bool bitactor_enqueue_signal(bitactor_t* ba, const signal_t* sig);
void bitactor_load_bytecode(bitactor_t* ba, const bitinstr_t* code, uint32_t size);
bool bitactor_verify_hash_integrity(bitactor_t* ba, uint32_t max_diff);

// Extended API for execution engine integration
typedef struct {
    uint32_t signal_id;
    uint64_t result;
    uint8_t ticks;
    uint32_t exec_hash;
    uint8_t status;
} bitactor_result_t;

bitactor_result_t bitactor_execute_program(bitactor_t* ba, const signal_t* signal, 
                                          const bitinstr_t* program, uint32_t program_size);

// Inline helpers for performance
__attribute__((always_inline))
static inline uint32_t bitactor_ring_next(uint32_t idx) {
    return (idx + 1) & (BITACTOR_RING_SIZE - 1);
}

__attribute__((always_inline))
static inline bool bitactor_ring_empty(const bitactor_t* ba) {
    return ba->signal_head == ba->signal_tail;
}

// SIMD operations for parallel bit manipulation
__attribute__((always_inline))
static inline __m256i bitactor_simd_and(__m256i a, __m256i b) {
    return _mm256_and_si256(a, b);
}

__attribute__((always_inline))
static inline __m256i bitactor_simd_or(__m256i a, __m256i b) {
    return _mm256_or_si256(a, b);
}

__attribute__((always_inline))
static inline __m256i bitactor_simd_xor(__m256i a, __m256i b) {
    return _mm256_xor_si256(a, b);
}

#endif