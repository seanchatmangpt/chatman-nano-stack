/*
 * BitActor Telemetry Interface
 * Reversible trace logging for audit and debugging
 */
#ifndef BITACTOR_TELEMETRY_H
#define BITACTOR_TELEMETRY_H

#include "bitactor.h"

/* Telemetry configuration */
#define TELEMETRY_RING_SIZE 4096
#define TRACE_OPS_SIZE 16

/* Trace operation codes */
#define TRACE_OP_ZERO_TICK 0x00
#define TRACE_OP_DISPATCH 0x01
#define TRACE_OP_EXECUTE 0x02
#define TRACE_OP_COMPLETE 0x03
#define TRACE_OP_ERROR 0x04

/* Forward declarations */
typedef struct telemetry_frame telemetry_frame_t;

/* Telemetry frame structure */
struct telemetry_frame {
    uint32_t signal_id;
    uint8_t signal_kind;
    uint32_t exec_hash;
    uint8_t ticks_used;
    uint64_t timestamp;
    uint64_t result_payload;
    uint8_t status;
    uint8_t flags;
    uint8_t trace_ops[TRACE_OPS_SIZE];
};

/* Telemetry ring structure */
struct telemetry_ring {
    uint32_t write_idx;
    uint32_t read_idx;
    uint32_t total_frames;
    bool enabled;
    struct telemetry_frame frames[TELEMETRY_RING_SIZE];
};

/* Telemetry API */
void telemetry_init(telemetry_ring_t* telemetry);
void telemetry_enable(telemetry_ring_t* telemetry);
void telemetry_record(telemetry_ring_t* telemetry, 
                     signal_t* signal, 
                     result_t* result, 
                     uint8_t ticks);

/* Frame access */
telemetry_frame_t* telemetry_get_last_frame(telemetry_ring_t* telemetry);
telemetry_frame_t* telemetry_get_frame(telemetry_ring_t* telemetry, uint32_t index);

/* Get telemetry from engine */
telemetry_ring_t* bitactor_get_telemetry(bitactor_engine_t* engine);

#endif /* BITACTOR_TELEMETRY_H */