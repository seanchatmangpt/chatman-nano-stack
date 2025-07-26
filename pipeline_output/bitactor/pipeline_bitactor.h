#ifndef PIPELINE_BITACTOR_H
#define PIPELINE_BITACTOR_H

#include <stdint.h>

// BitActor definitions for 4 types
typedef struct {
    uint32_t id;
    char state[64];
    void (*handler)(void* msg);
} bitactor_t;

// Actor instances
extern bitactor_t datastream_actor;
extern bitactor_t processor_actor;
extern bitactor_t pattern_actor;
extern bitactor_t alert_actor;

// Functions
void pipeline_init_actors(void);
int pipeline_send_message(bitactor_t* actor, void* msg, size_t size);

#endif // PIPELINE_BITACTOR_H
