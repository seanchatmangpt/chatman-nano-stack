#ifndef BITACTOR_PARALLEL_H
#define BITACTOR_PARALLEL_H

#include "bitactor.h"
#include "bitfiber.h"
#include "tick_parallel.h"
#include <immintrin.h>

// Parallel actor group - processes multiple actors in SIMD
#define BITACTOR_GROUP_SIZE 8
#define BITACTOR_MAX_GROUPS 32

// Actor state flags
#define ACTOR_FLAG_ACTIVE    0x01
#define ACTOR_FLAG_READY     0x02
#define ACTOR_FLAG_BLOCKED   0x04
#define ACTOR_FLAG_ERROR     0x08

// Parallel actor instance
typedef struct {
    bitactor_t actor;
    fiber_scheduler_t scheduler;
    uint32_t flags;
    uint32_t group_id;
    uint64_t activation_mask;
} parallel_actor_t;

// Actor group for SIMD processing
typedef struct {
    parallel_actor_t actors[BITACTOR_GROUP_SIZE];
    uint64_t active_mask;
    uint64_t ready_mask;
    tick_unit_t tick_unit;
} actor_group_t;

// Parallel BitActor system
typedef struct {
    actor_group_t groups[BITACTOR_MAX_GROUPS];
    uint32_t group_count;
    uint64_t global_tick;
    
    // Performance metrics
    uint64_t total_signals;
    uint64_t total_cycles;
    uint64_t parallel_efficiency;
} bitactor_parallel_t;

// Parallel API
void bitactor_parallel_init(bitactor_parallel_t* bp);
uint32_t bitactor_parallel_spawn_group(bitactor_parallel_t* bp);
void bitactor_parallel_activate(bitactor_parallel_t* bp, uint32_t group_id, uint64_t mask);
void bitactor_parallel_tick(bitactor_parallel_t* bp);

// SIMD operations for parallel actor processing
__attribute__((hot, always_inline))
static inline void bitactor_simd_process_signals(actor_group_t* group) {
    // Process 8 actors in parallel using AVX2
    __m256i active = _mm256_set1_epi64x(group->active_mask);
    __m256i ready = _mm256_set1_epi64x(group->ready_mask);
    
    // Check which actors have signals
    __m256i signal_mask = _mm256_and_si256(active, ready);
    
    // Process signals for active actors
    uint64_t mask = _mm256_extract_epi64(signal_mask, 0);
    while (mask) {
        uint32_t idx = __builtin_ctzll(mask);
        parallel_actor_t* actor = &group->actors[idx];
        
        if (!bitactor_ring_empty(&actor->actor)) {
            bitactor_tick(&actor->actor);
            fiber_schedule_tick(&actor->scheduler, &actor->actor);
        }
        
        mask &= mask - 1; // Clear lowest set bit
    }
}

// Parallel tick execution
__attribute__((hot, always_inline))
static inline void bitactor_parallel_execute_group(actor_group_t* group) {
    // Setup tick operations for active actors
    uint64_t mask = group->active_mask;
    int op_idx = 0;
    
    while (mask && op_idx < 8) {
        uint32_t idx = __builtin_ctzll(mask);
        parallel_actor_t* actor = &group->actors[idx];
        
        // Assign tick operation
        group->tick_unit.ops[op_idx] = (void (*)(void*))bitactor_tick;
        group->tick_unit.data[op_idx] = &actor->actor;
        group->tick_unit.tick_mask |= (1 << op_idx);
        
        op_idx++;
        mask &= mask - 1;
    }
    
    // Execute all operations in parallel
    tick_execute(&group->tick_unit);
}

#endif