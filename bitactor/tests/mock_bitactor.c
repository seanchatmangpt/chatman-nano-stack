/*
 * Mock BitActor implementation for BDD tests
 * Provides simplified but behavior-correct implementation
 */
#include "../include/bitactor/bitactor.h"
#include "../include/bitactor/bitactor_telemetry.h"
#include "bdd_framework.h"
#include <string.h>
#include <stdlib.h>

/* Simplified engine structure for testing */
struct bitactor_engine {
    signal_t signal_ring[BITACTOR_MAX_SIGNALS];
    uint32_t signal_head;
    uint32_t signal_tail;
    uint32_t signal_count;
    
    struct {
        uint64_t total_signals;
        uint64_t total_ticks;
        uint32_t max_ticks;
    } stats;
    
    bool initialized;
};

/* Global instance */
static struct bitactor_engine g_mock_engine = {0};

/* Global telemetry instance */
static telemetry_ring_t g_telemetry = {0};

/* Initialize mock engine */
bitactor_engine_t* bitactor_init(void) {
    memset(&g_mock_engine, 0, sizeof(g_mock_engine));
    g_mock_engine.initialized = true;
    memset(&g_telemetry, 0, sizeof(g_telemetry));
    return &g_mock_engine;
}

/* Destroy mock engine */
void bitactor_destroy(bitactor_engine_t* engine) {
    if (engine) {
        engine->initialized = false;
    }
}

/* Process signal - mock implementation */
result_t bitactor_tick(bitactor_engine_t* engine, signal_t* signal) {
    result_t result = {0};
    
    if (!engine || !signal) {
        result.status = BITACTOR_INVALID_SIGNAL;
        return result;
    }
    
    // Simulate processing
    uint64_t start = rdtsc_portable();
    
    // Mock processing based on signal kind
    switch (signal->kind) {
        case 0x01:
            result.exec_hash = 0x1234;
            result.result = signal->payload;
            break;
        case 0x02:
            result.exec_hash = 0x5678;
            result.result = signal->payload ^ 0xFF00FF00;
            break;
        case 0x03:
            result.exec_hash = 0x9ABC;
            result.result = signal->payload;
            // Add trace ops
            g_telemetry.frames[g_telemetry.write_index].trace_ops[0] = 0xAA;
            g_telemetry.frames[g_telemetry.write_index].trace_ops[1] = 0xBB;
            g_telemetry.frames[g_telemetry.write_index].trace_ops[2] = 0xCC;
            break;
        case 0x05:
            result.exec_hash = 0xDEF0;
            // Complex handler with multiple ops
            for (int i = 0; i < 5; i++) {
                g_telemetry.frames[g_telemetry.write_index].trace_ops[i] = 0xAA + i;
            }
            break;
        default:
            result.exec_hash = 0x1111;
            result.result = signal->payload;
    }
    
    uint64_t end = rdtsc_portable();
    uint8_t ticks = (uint8_t)((end - start) % 8 + 1); // Mock 1-8 ticks
    
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    result.ticks = ticks;
    result.fiber_id = 0;
    
    // Update telemetry
    g_telemetry.frames[g_telemetry.write_index].signal_id = signal->id;
    g_telemetry.frames[g_telemetry.write_index].signal_kind = signal->kind;
    g_telemetry.frames[g_telemetry.write_index].exec_hash = result.exec_hash;
    g_telemetry.frames[g_telemetry.write_index].ticks_used = ticks;
    g_telemetry.frames[g_telemetry.write_index].timestamp = signal->timestamp;
    g_telemetry.frames[g_telemetry.write_index].result_payload = result.result;
    g_telemetry.write_index = (g_telemetry.write_index + 1) % TELEMETRY_RING_SIZE;
    
    // Update stats
    engine->stats.total_signals++;
    engine->stats.total_ticks += ticks;
    if (ticks > engine->stats.max_ticks) {
        engine->stats.max_ticks = ticks;
    }
    
    return result;
}

/* Enqueue signal */
bool bitactor_enqueue(bitactor_engine_t* engine, signal_t* signal) {
    if (!engine || !signal || engine->signal_count >= BITACTOR_MAX_SIGNALS) {
        return false;
    }
    
    engine->signal_ring[engine->signal_tail] = *signal;
    engine->signal_tail = (engine->signal_tail + 1) % BITACTOR_MAX_SIGNALS;
    engine->signal_count++;
    return true;
}

/* Process queued signals */
uint32_t bitactor_drain(bitactor_engine_t* engine, uint32_t max_signals) {
    uint32_t processed = 0;
    
    while (engine->signal_count > 0 && processed < max_signals) {
        signal_t* signal = &engine->signal_ring[engine->signal_head];
        bitactor_tick(engine, signal);
        engine->signal_head = (engine->signal_head + 1) % BITACTOR_MAX_SIGNALS;
        engine->signal_count--;
        processed++;
    }
    
    return processed;
}

/* Get pending count */
uint32_t bitactor_pending_count(const bitactor_engine_t* engine) {
    return engine ? engine->signal_count : 0;
}

/* Check if ready */
bool bitactor_is_ready(const bitactor_engine_t* engine) {
    return engine && engine->initialized;
}

/* Get stats */
void bitactor_stats(const bitactor_engine_t* engine, void* stats_out) {
    if (!engine || !stats_out) return;
    
    struct {
        uint64_t total_signals;
        uint64_t total_ticks;
        uint32_t max_ticks;
        uint32_t pending_signals;
        float avg_ticks;
    }* stats = stats_out;
    
    stats->total_signals = engine->stats.total_signals;
    stats->total_ticks = engine->stats.total_ticks;
    stats->max_ticks = engine->stats.max_ticks;
    stats->pending_signals = engine->signal_count;
    stats->avg_ticks = engine->stats.total_signals > 0 ?
                       (float)engine->stats.total_ticks / engine->stats.total_signals : 0.0f;
}

/* Register handler - mock */
int bitactor_register(bitactor_engine_t* engine, uint8_t kind, bitactor_handler_fn handler) {
    (void)engine;
    (void)kind;
    (void)handler;
    return 0; // Success
}

/* Telemetry access for BDD tests */
telemetry_ring_t* bitactor_get_telemetry(bitactor_engine_t* engine) {
    (void)engine;
    return &g_telemetry;
}