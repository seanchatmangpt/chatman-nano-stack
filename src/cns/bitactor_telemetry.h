#ifndef BITACTOR_TELEMETRY_H
#define BITACTOR_TELEMETRY_H

#include "bitactor.h"
#include <stdint.h>

// Telemetry opcodes for trace reconstruction
#define TELEMETRY_OP_SIGNAL   0x01
#define TELEMETRY_OP_DISPATCH 0x02
#define TELEMETRY_OP_EXECUTE  0x03
#define TELEMETRY_OP_COMPLETE 0x04
#define TELEMETRY_OP_YIELD    0x05
#define TELEMETRY_OP_ERROR    0x06

// Extended telemetry frame
typedef struct {
    telemetry_frame_t base;
    uint64_t cycle_start;
    uint64_t cycle_end;
    uint32_t fiber_id;
    uint32_t bytecode_pc;
} telemetry_extended_t;

// Telemetry ring buffer manager
typedef struct {
    telemetry_extended_t* ring;
    uint32_t size;
    volatile uint32_t head;
    volatile uint32_t tail;
    uint64_t total_frames;
    uint64_t dropped_frames;
} telemetry_manager_t;

// Telemetry API
void telemetry_init(telemetry_manager_t* tm, telemetry_extended_t* buffer, uint32_t size);
void telemetry_record(telemetry_manager_t* tm, const telemetry_extended_t* frame);
bool telemetry_read(telemetry_manager_t* tm, telemetry_extended_t* frame);
void telemetry_flush(telemetry_manager_t* tm, void (*writer)(const telemetry_extended_t*));

// Inline telemetry helpers
__attribute__((always_inline))
static inline void telemetry_start(telemetry_extended_t* frame, uint32_t signal_id) {
    frame->base.timestamp = __rdtsc();
    frame->base.signal_id = signal_id;
    frame->cycle_start = frame->base.timestamp;
    frame->base.trace_ops[0] = TELEMETRY_OP_SIGNAL;
}

__attribute__((always_inline))
static inline void telemetry_end(telemetry_extended_t* frame) {
    frame->cycle_end = __rdtsc();
    frame->base.trace_ops[15] = TELEMETRY_OP_COMPLETE;
}

// Reversible trace to TTL conversion
typedef struct {
    uint32_t signal_kind;
    uint32_t exec_hash;
    const char* ttl_subject;
    const char* ttl_predicate;
    const char* ttl_object;
} trace_mapping_t;

// Trace reversal API
void trace_to_ttl(const telemetry_frame_t* frame, char* ttl_buffer, size_t size);
bool trace_verify_reversible(const telemetry_frame_t* frame, const char* original_ttl);

#endif