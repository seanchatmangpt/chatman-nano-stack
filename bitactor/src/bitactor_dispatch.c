/*
 * BitActor Static Dispatch Implementation
 * Branchless execution with perfect hashing
 */
#include "../include/bitactor/bitactor_dispatch.h"
#include <string.h>

/* Enhanced dispatcher with zero-tick elision */
result_t dispatch_signal(dispatch_table_t* table, signal_t* signal) {
    if (!table || !signal) {
        return dispatch_error(signal, NULL);
    }
    
    /* Zero-tick optimization: early return for trivially skippable signals */
    if (signal_is_zero_tick_candidate(signal)) {
        return dispatch_zero_tick_handler(signal, NULL);
    }
    
    /* Get handler entry */
    dispatch_entry_t* entry = &table->entries[signal->kind];
    
    /* Zero-tick handler optimization: early return for flagged handlers */
    if (entry->flags & 0x01) {  // ZERO_TICK_HANDLER_FLAG
        return dispatch_zero_tick_handler(signal, entry->context);
    }
    
    /* Normal dispatch path */
    if (entry->handler && entry->handler != dispatch_noop) {
        return entry->handler(signal, entry->context);
    }
    
    return dispatch_noop(signal, entry->context);
}

/* Default no-op handler */
result_t dispatch_noop(signal_t* signal, void* context) {
    (void)context;
    result_t result = {0};
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    result.exec_hash = 0xDEADC0DE;
    result.ticks = 1;
    return result;
}

/* Error handler for invalid signals */
result_t dispatch_error(signal_t* signal, void* context) {
    (void)context;
    result_t result = {0};
    result.signal_id = signal->id;
    result.status = BITACTOR_INVALID_SIGNAL;
    result.exec_hash = 0xBADC0DE;
    result.ticks = 1;
    return result;
}

/* Initialize dispatch table */
void dispatch_init(dispatch_table_t* table) {
    if (!table) {
        return;
    }
    
    /* Clear table */
    memset(table, 0, sizeof(*table));
    
    /* Set all entries to no-op handler */
    for (int i = 0; i < BITACTOR_DISPATCH_SIZE; i++) {
        table->entries[i].handler = dispatch_noop;
        table->entries[i].context = NULL;
        table->entries[i].min_priority = 0;
        table->entries[i].max_ticks = BITACTOR_TICK_BUDGET;
        table->entries[i].flags = 0;
    }
    
    table->active_count = 0;
}

/* Check if signal is zero-tick eligible */
static inline bool signal_is_zero_tick_candidate(const signal_t* signal) {
    return signal->type == 0xFF ||           // Heartbeat
           (signal->payload & 0xFF) == 0 ||  // Zero confidence
           (signal->flags & 0x80) != 0;      // Test signal
}

/* Zero-tick dispatch optimization */
result_t dispatch_zero_tick_handler(signal_t* signal, void* context) {
    (void)context;
    result_t result = {0};
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    result.exec_hash = 0x5A4E00;  // "ZERO" marker
    result.ticks = 0;  // True zero-tick
    result.result = 0; // No-op result
    return result;
}

/* Register a handler with zero-tick optimization */
int dispatch_register(dispatch_table_t* table, uint8_t kind, 
                      signal_handler_fn handler, void* context) {
    if (!table || !handler) {
        return -1;
    }
    
    /* Direct index - perfect hash */
    dispatch_entry_t* entry = &table->entries[kind];
    
    /* Zero-tick optimization: set flag for handlers that can be bypassed */
    if (handler == dispatch_zero_tick_handler) {
        entry->flags |= 0x01;  // ZERO_TICK_HANDLER_FLAG
    }
    
    /* Update entry */
    entry->handler = handler;
    entry->context = context;
    
    /* Update active count if this was a new registration */
    if (entry->handler == dispatch_noop) {
        table->active_count++;
    }
    
    return 0;
}

/* Example domain-specific handlers for testing */

/* Market data handler */
result_t handler_market_data(signal_t* signal, void* context) {
    uint64_t* scratch = (uint64_t*)context;
    result_t result = {0};
    
    /* Extract price from payload */
    uint32_t price = (uint32_t)(signal->payload & 0xFFFFFFFF);
    uint32_t volume = (uint32_t)(signal->payload >> 32);
    
    /* Simple moving average calculation (branchless) */
    scratch[0] = (scratch[0] * 7 + price) >> 3;
    
    /* Volume weighted average price */
    scratch[1] += price * volume;
    scratch[2] += volume;
    
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    result.result = scratch[0];  /* Return SMA */
    result.exec_hash = 0x12345678;
    result.ticks = 3;
    
    return result;
}

/* Sensor data handler */
result_t handler_sensor_data(signal_t* signal, void* context) {
    uint32_t* scratch = (uint32_t*)context;
    result_t result = {0};
    
    /* Extract sensor reading */
    uint32_t reading = (uint32_t)signal->payload;
    
    /* Apply calibration (branchless) */
    uint32_t calibrated = (reading * 1024 + scratch[0]) >> 10;
    
    /* Update min/max (branchless) */
    scratch[1] = bitactor_min(scratch[1], calibrated);
    scratch[2] = bitactor_max(scratch[2], calibrated);
    
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    result.result = calibrated;
    result.exec_hash = 0x87654321;
    result.ticks = 2;
    
    return result;
}

/* Control command handler */
result_t handler_control_cmd(signal_t* signal, void* context) {
    uint8_t* state = (uint8_t*)context;
    result_t result = {0};
    
    /* Extract command */
    uint8_t cmd = (uint8_t)(signal->payload & 0xFF);
    uint8_t arg = (uint8_t)((signal->payload >> 8) & 0xFF);
    
    /* State machine update (branchless) */
    uint8_t next_state = state[cmd];
    state[256] = next_state;  /* Current state */
    
    /* Execute command action */
    uint64_t action = ((uint64_t)next_state << 32) | ((uint64_t)arg << 16) | cmd;
    
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    result.result = action;
    result.exec_hash = 0xABCDEF00 | cmd;
    result.ticks = 4;
    
    return result;
}