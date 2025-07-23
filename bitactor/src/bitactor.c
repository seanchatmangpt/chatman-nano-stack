/*
 * BitActor Core Engine Implementation
 * Zero-allocation, deterministic execution in ≤8 CPU ticks
 */
#include "../include/bitactor/bitactor.h"
#include "../include/bitactor/bitfiber.h"
#include "../include/bitactor/bitactor_dispatch.h"
#include "../include/bitactor/bitactor_telemetry.h"
#include <string.h>
#include <stdlib.h>

/* Engine state - all memory pre-allocated */
struct bitactor_engine {
    /* Signal ring buffer */
    signal_t signal_ring[BITACTOR_MAX_SIGNALS];
    uint32_t signal_head;
    uint32_t signal_tail;
    uint32_t signal_count;
    
    /* Static dispatch table */
    dispatch_table_t dispatch;
    
    /* Fiber scheduler */
    fiber_scheduler_t* fiber_sched;
    
    /* Telemetry system */
    telemetry_ring_t telemetry;
    
    /* Scratch memory pools */
    uint8_t scratch_pools[BITACTOR_MAX_FIBERS][BITACTOR_SCRATCH_SIZE];
    
    /* Engine statistics */
    uint64_t total_signals;
    uint64_t total_ticks;
    uint32_t max_ticks;
    
    /* Engine state */
    bool initialized;
};

/* Static instance - single engine per process */
static struct bitactor_engine g_engine = {0};

/* Initialize the BitActor engine */
bitactor_engine_t* bitactor_init(void) {
    bitactor_engine_t* engine = &g_engine;
    
    if (engine->initialized) {
        return engine;
    }
    
    /* Clear all memory */
    memset(engine, 0, sizeof(*engine));
    
    /* Initialize subsystems */
    dispatch_init(&engine->dispatch);
    engine->fiber_sched = fiber_scheduler_init();
    telemetry_init(&engine->telemetry);
    
    /* Register default handlers */
    for (int i = 0; i < BITACTOR_DISPATCH_SIZE; i++) {
        dispatch_register(&engine->dispatch, i, dispatch_noop, NULL);
    }
    
    engine->initialized = true;
    return engine;
}

/* Destroy engine - just marks as uninitialized */
void bitactor_destroy(bitactor_engine_t* engine) {
    if (engine && engine->initialized) {
        fiber_scheduler_destroy(engine->fiber_sched);
        engine->initialized = false;
    }
}

/* Main tick function - MUST execute in ≤8 CPU ticks */
result_t bitactor_tick(bitactor_engine_t* engine, signal_t* signal) {
    uint64_t start_ticks = bitactor_rdtsc();
    result_t result = {0};
    
    /* Validate inputs */
    if (!engine || !engine->initialized || !signal) {
        result.status = BITACTOR_INVALID_SIGNAL;
        return result;
    }
    
    /* Set up result */
    result.signal_id = signal->id;
    result.fiber_id = fiber_current();
    
    /* Dispatch signal - branchless execution */
    result = dispatch_signal(&engine->dispatch, signal);
    
    /* Measure ticks */
    uint64_t end_ticks = bitactor_rdtsc();
    uint8_t ticks_used = (uint8_t)bitactor_min(end_ticks - start_ticks, 255);
    result.ticks = ticks_used;
    
    /* Check tick budget */
    if (ticks_used > BITACTOR_TICK_BUDGET) {
        result.status = BITACTOR_TICK_EXCEEDED;
    }
    
    /* Record telemetry */
    telemetry_record(&engine->telemetry, signal, &result, ticks_used);
    
    /* Update statistics */
    engine->total_signals++;
    engine->total_ticks += ticks_used;
    engine->max_ticks = bitactor_max(engine->max_ticks, ticks_used);
    
    return result;
}

/* Enqueue signal for processing */
bool bitactor_enqueue(bitactor_engine_t* engine, signal_t* signal) {
    if (!engine || !signal || engine->signal_count >= BITACTOR_MAX_SIGNALS) {
        return false;
    }
    
    /* Copy signal to ring buffer */
    engine->signal_ring[engine->signal_tail] = *signal;
    engine->signal_tail = (engine->signal_tail + 1) % BITACTOR_MAX_SIGNALS;
    engine->signal_count++;
    
    return true;
}

/* Process all pending signals */
uint32_t bitactor_drain(bitactor_engine_t* engine, uint32_t max_signals) {
    uint32_t processed = 0;
    
    while (engine->signal_count > 0 && processed < max_signals) {
        /* Get next signal */
        signal_t* signal = &engine->signal_ring[engine->signal_head];
        
        /* Process it */
        bitactor_tick(engine, signal);
        
        /* Advance ring buffer */
        engine->signal_head = (engine->signal_head + 1) % BITACTOR_MAX_SIGNALS;
        engine->signal_count--;
        processed++;
    }
    
    return processed;
}

/* Register a handler */
int bitactor_register(bitactor_engine_t* engine, 
                      uint8_t kind, 
                      bitactor_handler_fn handler) {
    if (!engine || !handler) {
        return -1;
    }
    
    return dispatch_register(&engine->dispatch, kind, handler, 
                             &engine->scratch_pools[kind % BITACTOR_MAX_FIBERS]);
}

/* Get pending signal count */
uint32_t bitactor_pending_count(const bitactor_engine_t* engine) {
    return engine ? engine->signal_count : 0;
}

/* Check if engine is ready */
bool bitactor_is_ready(const bitactor_engine_t* engine) {
    return engine && engine->initialized;
}

/* Get engine statistics */
void bitactor_stats(const bitactor_engine_t* engine, void* stats_out) {
    if (!engine || !stats_out) {
        return;
    }
    
    struct {
        uint64_t total_signals;
        uint64_t total_ticks;
        uint32_t max_ticks;
        uint32_t pending_signals;
        float avg_ticks;
    }* stats = stats_out;
    
    stats->total_signals = engine->total_signals;
    stats->total_ticks = engine->total_ticks;
    stats->max_ticks = engine->max_ticks;
    stats->pending_signals = engine->signal_count;
    stats->avg_ticks = engine->total_signals > 0 ? 
                       (float)engine->total_ticks / engine->total_signals : 0.0f;
}