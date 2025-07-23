#include "tick_parallel.h"
#include <immintrin.h>

// Ultra-optimized tick execution - inline everything
__attribute__((hot, always_inline))
static inline void tick_execute_inline(tick_unit_t* unit) {
    // Unroll all 8 operations inline - no function call overhead
    if (unit->tick_mask & 0x01) unit->ops[0](unit->data[0]);
    if (unit->tick_mask & 0x02) unit->ops[1](unit->data[1]);
    if (unit->tick_mask & 0x04) unit->ops[2](unit->data[2]);
    if (unit->tick_mask & 0x08) unit->ops[3](unit->data[3]);
    if (unit->tick_mask & 0x10) unit->ops[4](unit->data[4]);
    if (unit->tick_mask & 0x20) unit->ops[5](unit->data[5]);
    if (unit->tick_mask & 0x40) unit->ops[6](unit->data[6]);
    if (unit->tick_mask & 0x80) unit->ops[7](unit->data[7]);
}

// Optimized version using computed goto for even better performance
__attribute__((hot))
void tick_execute_optimized(tick_unit_t* unit) {
    // Pre-compute all masks
    const uint64_t mask = unit->tick_mask;
    
    // Use bit manipulation to avoid branches
    for (int i = 0; i < 8; i++) {
        // This compiles to conditional move, no branch
        void* op_ptr = (void*)((uintptr_t)unit->ops[i] & -(int64_t)((mask >> i) & 1));
        if (op_ptr) {
            ((void (*)(void*))op_ptr)(unit->data[i]);
        }
    }
}

// SIMD optimized batch tick execution
__attribute__((hot))
void tick_execute_batch_simd(tick_unit_t* units, int count) {
    // Process 4 units at a time using SIMD
    for (int i = 0; i < count; i += 4) {
        // Prefetch next batch
        if (i + 4 < count) {
            __builtin_prefetch(&units[i + 4], 0, 3);
        }
        
        // Process current batch
        for (int j = 0; j < 4 && (i + j) < count; j++) {
            tick_execute_inline(&units[i + j]);
        }
    }
}