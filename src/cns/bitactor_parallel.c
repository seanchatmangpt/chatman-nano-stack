#include "bitactor_parallel.h"
#include <string.h>

// Initialize parallel BitActor system
void bitactor_parallel_init(bitactor_parallel_t* bp) {
    memset(bp, 0, sizeof(bitactor_parallel_t));
    
    // Initialize all groups
    for (uint32_t i = 0; i < BITACTOR_MAX_GROUPS; i++) {
        actor_group_t* group = &bp->groups[i];
        
        // Initialize actors in group
        for (uint32_t j = 0; j < BITACTOR_GROUP_SIZE; j++) {
            parallel_actor_t* actor = &group->actors[j];
            bitactor_init(&actor->actor);
            fiber_scheduler_init(&actor->scheduler);
            actor->group_id = i;
            actor->flags = 0;
        }
        
        // Clear tick unit
        memset(&group->tick_unit, 0, sizeof(tick_unit_t));
    }
}

// Spawn a new actor group
uint32_t bitactor_parallel_spawn_group(bitactor_parallel_t* bp) {
    if (bp->group_count >= BITACTOR_MAX_GROUPS) {
        return UINT32_MAX;
    }
    
    uint32_t group_id = bp->group_count++;
    actor_group_t* group = &bp->groups[group_id];
    
    // Activate all actors in group
    group->active_mask = 0xFF; // All 8 actors active
    group->ready_mask = 0xFF;
    
    // Set actor flags
    for (uint32_t i = 0; i < BITACTOR_GROUP_SIZE; i++) {
        group->actors[i].flags = ACTOR_FLAG_ACTIVE | ACTOR_FLAG_READY;
        group->actors[i].activation_mask = (1ULL << i);
    }
    
    return group_id;
}

// Activate specific actors in a group
void bitactor_parallel_activate(bitactor_parallel_t* bp, uint32_t group_id, uint64_t mask) {
    if (group_id >= bp->group_count) {
        return;
    }
    
    actor_group_t* group = &bp->groups[group_id];
    group->active_mask = mask & 0xFF;
    
    // Update actor flags
    for (uint32_t i = 0; i < BITACTOR_GROUP_SIZE; i++) {
        if (mask & (1ULL << i)) {
            group->actors[i].flags |= ACTOR_FLAG_ACTIVE;
        } else {
            group->actors[i].flags &= ~ACTOR_FLAG_ACTIVE;
        }
    }
}

// Main parallel tick function
__attribute__((hot))
void bitactor_parallel_tick(bitactor_parallel_t* bp) {
    uint64_t start_cycle = __rdtsc();
    
    // Process each active group
    for (uint32_t g = 0; g < bp->group_count; g++) {
        actor_group_t* group = &bp->groups[g];
        
        if (group->active_mask == 0) {
            continue;
        }
        
        // Process signals in parallel using SIMD
        bitactor_simd_process_signals(group);
        
        // Execute tick operations in parallel
        bitactor_parallel_execute_group(group);
        
        // Update group ready mask based on signal availability
        group->ready_mask = 0;
        for (uint32_t i = 0; i < BITACTOR_GROUP_SIZE; i++) {
            if ((group->active_mask & (1ULL << i)) && 
                !bitactor_ring_empty(&group->actors[i].actor)) {
                group->ready_mask |= (1ULL << i);
            }
        }
    }
    
    // Update metrics
    bp->global_tick++;
    bp->total_cycles += __rdtsc() - start_cycle;
    
    // Calculate parallel efficiency
    uint32_t active_actors = 0;
    for (uint32_t g = 0; g < bp->group_count; g++) {
        active_actors += __builtin_popcountll(bp->groups[g].active_mask);
    }
    
    if (active_actors > 0) {
        bp->parallel_efficiency = (bp->total_cycles / active_actors) * 100 / bp->total_cycles;
    }
}

// Batch signal enqueue for parallel actors
__attribute__((hot))
void bitactor_parallel_enqueue_batch(bitactor_parallel_t* bp, uint32_t group_id, 
                                   const signal_t* signals, uint32_t count) {
    if (group_id >= bp->group_count) {
        return;
    }
    
    actor_group_t* group = &bp->groups[group_id];
    
    // Distribute signals round-robin to active actors
    uint32_t actor_idx = 0;
    for (uint32_t i = 0; i < count; i++) {
        // Find next active actor
        while (!(group->active_mask & (1ULL << actor_idx))) {
            actor_idx = (actor_idx + 1) & 7;
        }
        
        // Enqueue signal
        parallel_actor_t* actor = &group->actors[actor_idx];
        bitactor_enqueue_signal(&actor->actor, &signals[i]);
        
        // Move to next actor
        actor_idx = (actor_idx + 1) & 7;
    }
    
    bp->total_signals += count;
}