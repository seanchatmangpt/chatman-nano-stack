#include "pipeline_bitactor.h"
#include <string.h>
#include <stdio.h>

// Actor instances
bitactor_t datastream_actor;
bitactor_t processor_actor;
bitactor_t pattern_actor;
bitactor_t alert_actor;

// Message handlers

static void handle_datastream(void* msg) {
    // Handle DataStream messages
    // Attributes: id, source, format, rate
    printf("Processing DataStream message\n");
}

static void handle_processor(void* msg) {
    // Handle Processor messages
    // Attributes: id, type, config, capacity
    printf("Processing Processor message\n");
}

static void handle_pattern(void* msg) {
    // Handle Pattern messages
    // Attributes: id, expression, severity
    printf("Processing Pattern message\n");
}

static void handle_alert(void* msg) {
    // Handle Alert messages
    // Attributes: id, message, timestamp, priority
    printf("Processing Alert message\n");
}

void pipeline_init_actors(void) {
    printf("Initializing pipeline actors...\n");
    
    datastream_actor.handler = handle_datastream;
    processor_actor.handler = handle_processor;
    pattern_actor.handler = handle_pattern;
    alert_actor.handler = handle_alert;
    
    printf("Pipeline actors initialized\n");
}

int pipeline_send_message(bitactor_t* actor, void* msg, size_t size) {
    if (actor && actor->handler) {
        actor->handler(msg);
        return 0;
    }
    return -1;
}
