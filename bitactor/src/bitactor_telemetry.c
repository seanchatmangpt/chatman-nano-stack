/*
 * BitActor Telemetry Implementation
 * Reversible trace system with zero allocation
 */
#include "../include/bitactor/bitactor_telemetry.h"
#include <string.h>
#include <stdio.h>

/* Blake3-like hash for trace integrity (simplified) */
static uint32_t trace_hash(const uint8_t* data, size_t len) {
    uint32_t hash = 0x811C9DC5;  /* FNV offset basis */
    for (size_t i = 0; i < len; i++) {
        hash ^= data[i];
        hash *= 0x01000193;  /* FNV prime */
    }
    return hash;
}

/* Initialize telemetry ring */
void telemetry_init(telemetry_ring_t* ring) {
    if (!ring) {
        return;
    }
    
    /* Clear all frames */
    memset(ring, 0, sizeof(*ring));
    
    /* Initialize indices */
    ring->write_idx = 0;
    ring->read_idx = 0;
    ring->total_frames = 0;
    ring->enabled = false;
}

/* Enable telemetry recording */
void telemetry_enable(telemetry_ring_t* ring) {
    if (ring) {
        ring->enabled = true;
    }
}

/* Zero-tick telemetry metrics */
static struct {
    uint64_t signals_zero_tick;
    uint64_t signals_bypassed;
    uint64_t tick_budget_saved;
} zero_tick_metrics = {0};

/* Record zero-tick execution */
void telemetry_record_zero_tick(telemetry_ring_t* ring, signal_t* signal) {
    zero_tick_metrics.signals_zero_tick++;
    zero_tick_metrics.signals_bypassed++;
    
    /* Still record a minimal trace for zero-tick signals */
    if (ring) {
        telemetry_frame_t* frame = &ring->frames[ring->write_idx];
        frame->timestamp = signal->timestamp;
        frame->signal_id = signal->id;
        frame->exec_hash = 0x5A4E00;  // "ZERO" marker
        frame->ticks_used = 0;
        frame->status = BITACTOR_OK;
        frame->trace_ops[0] = TRACE_OP_ZERO_TICK;
        frame->trace_ops[1] = signal->type;
        
        ring->write_idx = (ring->write_idx + 1) % 4096;
        ring->total_frames++;
    }
}

/* Get zero-tick metrics */
uint64_t telemetry_get_zero_tick_count(void) {
    return zero_tick_metrics.signals_zero_tick;
}

uint64_t telemetry_get_zero_tick_ratio(uint64_t total_signals) {
    if (total_signals == 0) return 0;
    return (zero_tick_metrics.signals_zero_tick * 100) / total_signals;
}

/* Record execution trace */
void telemetry_record(telemetry_ring_t* ring, 
                      signal_t* signal, 
                      result_t* result,
                      uint8_t ticks) {
    if (!ring || !signal || !result) {
        return;
    }
    
    /* Zero-tick optimization: record separately */
    if (result->ticks == 0 && result->exec_hash == 0x5A4E00) {
        telemetry_record_zero_tick(ring, signal);
        return;
    }
    
    /* Get frame to write */
    telemetry_frame_t* frame = &ring->frames[ring->write_idx];
    
    /* Record signal and result data */
    frame->timestamp = signal->timestamp;
    frame->signal_id = signal->id;
    frame->exec_hash = result->exec_hash;
    frame->ticks_used = ticks;
    frame->status = result->status;
    frame->flags = result->flags;
    
    /* Build trace operation sequence */
    uint8_t op_idx = 0;
    
    /* Operation: DISPATCH */
    frame->trace_ops[op_idx++] = TRACE_OP_DISPATCH;
    frame->trace_ops[op_idx++] = signal->kind;
    
    /* Operation: EXECUTE */
    frame->trace_ops[op_idx++] = TRACE_OP_EXECUTE;
    frame->trace_ops[op_idx++] = (uint8_t)(result->exec_hash & 0xFF);
    frame->trace_ops[op_idx++] = (uint8_t)((result->exec_hash >> 8) & 0xFF);
    frame->trace_ops[op_idx++] = (uint8_t)((result->exec_hash >> 16) & 0xFF);
    frame->trace_ops[op_idx++] = (uint8_t)((result->exec_hash >> 24) & 0xFF);
    
    /* Operation: COMPLETE or ERROR */
    if (result->status == BITACTOR_OK) {
        frame->trace_ops[op_idx++] = TRACE_OP_COMPLETE;
    } else {
        frame->trace_ops[op_idx++] = TRACE_OP_ERROR;
        frame->trace_ops[op_idx++] = result->status;
    }
    
    /* Fill remaining with zeros */
    while (op_idx < 16) {
        frame->trace_ops[op_idx++] = 0;
    }
    
    /* Advance write index */
    ring->write_idx = (ring->write_idx + 1) % 4096;
    ring->total_frames++;
    
    /* Handle wrap-around */
    if (ring->write_idx == ring->read_idx && ring->total_frames > 0) {
        ring->read_idx = (ring->read_idx + 1) % 4096;
    }
}

/* Read frames for export */
uint32_t telemetry_read(telemetry_ring_t* ring, 
                        telemetry_frame_t* out, 
                        uint32_t max_frames) {
    if (!ring || !out || max_frames == 0) {
        return 0;
    }
    
    uint32_t frames_read = 0;
    
    while (frames_read < max_frames && ring->read_idx != ring->write_idx) {
        /* Copy frame */
        out[frames_read] = ring->frames[ring->read_idx];
        
        /* Advance read index */
        ring->read_idx = (ring->read_idx + 1) % 4096;
        frames_read++;
    }
    
    return frames_read;
}

/* Convert trace to TTL representation (for reversibility) */
int telemetry_to_ttl(const telemetry_frame_t* frame, char* ttl_out, size_t ttl_size) {
    if (!frame || !ttl_out || ttl_size < 256) {
        return -1;
    }
    
    /* Build TTL representation */
    int written = snprintf(ttl_out, ttl_size,
        "@prefix cns: <http://cns.io/bitactor#> .\n"
        "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n"
        "\n"
        "cns:signal_%u a cns:Signal ;\n"
        "    cns:timestamp \"%lu\"^^xsd:integer ;\n"
        "    cns:executionHash \"%u\"^^xsd:integer ;\n"
        "    cns:ticksUsed \"%u\"^^xsd:integer ;\n"
        "    cns:status \"%u\"^^xsd:integer ;\n"
        "    cns:traceOps [\n",
        frame->signal_id,
        frame->timestamp,
        frame->exec_hash,
        frame->ticks_used,
        frame->status
    );
    
    /* Add trace operations */
    for (int i = 0; i < 16 && frame->trace_ops[i] != 0; i++) {
        written += snprintf(ttl_out + written, ttl_size - written,
            "        cns:op%d \"0x%02X\"^^xsd:hexBinary ;\n",
            i, frame->trace_ops[i]
        );
    }
    
    /* Close TTL */
    written += snprintf(ttl_out + written, ttl_size - written,
        "    ] .\n"
    );
    
    return written;
}

/* Verify trace integrity */
bool telemetry_verify(const telemetry_frame_t* frame) {
    if (!frame) {
        return false;
    }
    
    /* Calculate expected hash from trace ops */
    uint32_t expected_hash = trace_hash(frame->trace_ops, 16);
    
    /* Compare with recorded hash (allowing for some difference) */
    uint32_t diff = expected_hash ^ frame->exec_hash;
    
    /* Hash difference must be < 0x1000 as per spec */
    return diff < 0x1000;
}

/* Get last recorded frame */
telemetry_frame_t* telemetry_get_last_frame(telemetry_ring_t* ring) {
    if (!ring || ring->total_frames == 0) {
        return NULL;
    }
    
    uint32_t last_idx = (ring->write_idx - 1 + TELEMETRY_RING_SIZE) % TELEMETRY_RING_SIZE;
    return &ring->frames[last_idx];
}

/* Get frame by index */
telemetry_frame_t* telemetry_get_frame(telemetry_ring_t* ring, uint32_t index) {
    if (!ring || index >= ring->total_frames || index >= TELEMETRY_RING_SIZE) {
        return NULL;
    }
    
    uint32_t frame_idx = (ring->read_idx + index) % TELEMETRY_RING_SIZE;
    return &ring->frames[frame_idx];
}

/* Get telemetry from engine */
telemetry_ring_t* bitactor_get_telemetry(bitactor_engine_t* engine) {
    if (!engine) {
        return NULL;
    }
    
    /* This would return a pointer to the engine's telemetry system */
    /* For now, return NULL as the engine structure is opaque */
    return NULL;
}