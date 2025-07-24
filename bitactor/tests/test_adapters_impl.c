/*
 * Test Adapters Implementation
 * Provides the actual implementations of BitActor functions
 * This file should be compiled separately
 */

#include "../include/bitactor/bitactor.h"
#include "../include/bitactor/bitactor_telemetry.h"
#include <string.h>
#include <stdlib.h>

/* Simplified BitActor engine structure */
struct bitactor_engine {
    signal_t signal_ring[BITACTOR_MAX_SIGNALS];
    uint32_t signal_head;
    uint32_t signal_tail;
    uint32_t signal_count;
    telemetry_ring_t telemetry;
    bool initialized;
};

/* Static instance */
static struct bitactor_engine g_engine = {0};

/* Initialize the BitActor engine */
bitactor_engine_t* bitactor_init(void) {
    memset(&g_engine, 0, sizeof(g_engine));
    g_engine.initialized = true;
    telemetry_init(&g_engine.telemetry);
    return &g_engine;
}

/* Destroy engine */
void bitactor_destroy(bitactor_engine_t* engine) {
    if (engine && engine->initialized) {
        engine->initialized = false;
    }
}

/* Process signal - simplified implementation */
result_t bitactor_tick(bitactor_engine_t* engine, signal_t* signal) {
    result_t result = {0};
    
    if (!engine || !engine->initialized || !signal) {
        result.status = BITACTOR_INVALID_SIGNAL;
        return result;
    }
    
    /* Set up result */
    result.signal_id = signal->id;
    result.fiber_id = 0;
    result.status = BITACTOR_OK;
    
    /* Simulate processing based on signal kind */
    switch (signal->kind) {
        case 0x01:
            result.exec_hash = 0x1234;
            result.result = signal->payload;
            result.ticks = 2;
            break;
        case 0x02:
            result.exec_hash = 0x5678;
            result.result = signal->payload ^ 0xFF00FF00;
            result.ticks = 4;
            break;
        case 0x03:
            result.exec_hash = 0x9ABC;
            result.result = signal->payload;
            result.ticks = 6;
            break;
        default:
            result.exec_hash = 0x1111;
            result.result = signal->payload;
            result.ticks = 3;
    }
    
    /* Record telemetry */
    telemetry_record(&engine->telemetry, signal, &result, result.ticks);
    
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

/* Initialize telemetry */
void telemetry_init(telemetry_ring_t* ring) {
    if (ring) {
        memset(ring, 0, sizeof(*ring));
    }
}

/* Record telemetry */
void telemetry_record(telemetry_ring_t* ring, signal_t* signal, result_t* result, uint8_t ticks) {
    if (!ring || !signal || !result) {
        return;
    }
    
    uint32_t idx = ring->write_index;
    telemetry_frame_t* frame = &ring->frames[idx];
    
    frame->timestamp = signal->timestamp;
    frame->signal_id = signal->id;
    frame->signal_kind = signal->kind;
    frame->exec_hash = result->exec_hash;
    frame->ticks_used = ticks;
    frame->result_payload = result->result;
    
    ring->write_index = (idx + 1) % TELEMETRY_RING_SIZE;
}

/* telemetry_enable is already defined in test_adapters.c */

/* News validation credibility table */
static uint32_t g_credibility_table[256] = {0};
static bool g_table_initialized = false;

/* Initialize credibility table */
void init_credibility_table(void) {
    if (g_table_initialized) return;
    
    for (int i = 0; i < 256; i++) {
        uint64_t source_id = i;
        g_credibility_table[i] = (uint32_t)((source_id * 0x123456789ABCDEF0ULL) >> 56) % 100;
    }
    g_table_initialized = true;
}

/* Fast credibility check */
uint32_t check_source_credibility_fast(uint64_t source_id) {
    uint8_t index = (source_id >> 32) & 0xFF;
    return g_credibility_table[index];
}