/*
 * BitActor Core Runtime Header - PRODUCTION VERSION
 * Real header definitions for zero-tick optimization
 */
#ifndef BITACTOR_H
#define BITACTOR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Core constants */
#define BITACTOR_TICK_BUDGET 8
#define BITACTOR_MAX_FIBERS 256
#define BITACTOR_DISPATCH_SIZE 256

/* Status codes */
typedef enum {
    BITACTOR_OK = 0,
    BITACTOR_INVALID_SIGNAL = 1,
    BITACTOR_TICK_EXCEEDED = 2,
    BITACTOR_ERROR = 3
} bitactor_status_t;

/* Signal types */
#define SIG_HEARTBEAT 0xFF
#define SIG_NORMAL 0x01
#define SIG_DEBUG 0x80

/* Zero-tick optimization flags */
#define ZERO_TICK_FLAG 0x01
#define TEST_SIGNAL_FLAG 0x80

/* Signal structure */
typedef struct signal {
    uint32_t id;
    uint8_t type;
    uint64_t payload;
    uint8_t flags;
    uint64_t timestamp;
    uint8_t kind;  // For dispatch compatibility
} signal_t;

/* Result structure */
typedef struct result {
    uint32_t signal_id;
    bitactor_status_t status;
    uint8_t ticks;
    uint32_t exec_hash;
    uint64_t result;
    uint8_t flags;
} result_t;

/* Forward declarations */
typedef struct bitactor_engine bitactor_engine_t;
typedef struct dispatch_table dispatch_table_t;
typedef struct fiber_scheduler fiber_scheduler_t;
typedef struct telemetry_ring telemetry_ring_t;

/* Handler function type */
typedef result_t (*bitactor_handler_fn)(signal_t* signal, void* context);

/* Engine management */
bitactor_engine_t* bitactor_engine_create(void);
void bitactor_engine_destroy(bitactor_engine_t* engine);
int bitactor_register(bitactor_engine_t* engine, uint8_t signal_kind, 
                     bitactor_handler_fn handler);

/* Signal processing */
result_t bitactor_process_signal(bitactor_engine_t* engine, signal_t* signal);

/* Zero-tick optimization functions */
bool bitactor_signal_is_zero_tick(const signal_t* signal);
result_t bitactor_zero_tick_handler(signal_t* signal, void* context);

/* Utility functions */
static inline uint32_t bitactor_min(uint32_t a, uint32_t b) {
    return a < b ? a : b;
}

static inline uint32_t bitactor_max(uint32_t a, uint32_t b) {
    return a > b ? a : b;
}

/* Portable cycle counter */
#ifdef __x86_64__
#include <x86intrin.h>
static inline uint64_t bitactor_get_cycles(void) {
    return __rdtsc();
}
#elif defined(__aarch64__)
static inline uint64_t bitactor_get_cycles(void) {
    uint64_t val;
    __asm__ volatile("mrs %0, cntvct_el0" : "=r" (val));
    return val;
}
#else
static inline uint64_t bitactor_get_cycles(void) {
    return 0;
}
#endif

#endif /* BITACTOR_H */