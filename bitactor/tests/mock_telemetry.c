/*
 * Mock telemetry implementation for BDD tests
 */
#include "../include/bitactor/bitactor_telemetry.h"
#include <string.h>

/* External telemetry from mock_bitactor.c */
extern telemetry_ring_t* bitactor_get_telemetry(bitactor_engine_t* engine);

/* Enable telemetry */
void telemetry_enable(telemetry_ring_t* telemetry) {
    if (telemetry) {
        telemetry->enabled = true;
    }
}

/* Get last frame */
telemetry_frame_t* telemetry_get_last_frame(telemetry_ring_t* telemetry) {
    if (!telemetry || telemetry->write_index == 0) {
        return NULL;
    }
    
    uint32_t last_index = (telemetry->write_index - 1 + TELEMETRY_RING_SIZE) % TELEMETRY_RING_SIZE;
    return (telemetry_frame_t*)&telemetry->frames[last_index];
}

/* Get specific frame */
telemetry_frame_t* telemetry_get_frame(telemetry_ring_t* telemetry, uint32_t index) {
    if (!telemetry || index >= TELEMETRY_RING_SIZE) {
        return NULL;
    }
    
    return (telemetry_frame_t*)&telemetry->frames[index];
}

/* Initialize telemetry */
void telemetry_init(telemetry_ring_t* telemetry) {
    if (telemetry) {
        memset(telemetry, 0, sizeof(telemetry_ring_t));
    }
}

/* Record telemetry */
void telemetry_record(telemetry_ring_t* telemetry, 
                     signal_t* signal, 
                     result_t* result, 
                     uint8_t ticks) {
    if (!telemetry || !signal || !result) {
        return;
    }
    
    uint32_t index = telemetry->write_index;
    
    telemetry->frames[index].signal_id = signal->id;
    telemetry->frames[index].signal_kind = signal->kind;
    telemetry->frames[index].exec_hash = result->exec_hash;
    telemetry->frames[index].ticks_used = ticks;
    telemetry->frames[index].timestamp = signal->timestamp;
    telemetry->frames[index].result_payload = result->result;
    
    telemetry->write_index = (index + 1) % TELEMETRY_RING_SIZE;
}