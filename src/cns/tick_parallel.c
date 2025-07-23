#include "tick_parallel.h"
#include <pthread.h>

typedef struct {
    void (*op)(void*);
    void* data;
} thread_arg_t;

static void* thread_worker(void* arg) {
    thread_arg_t* targ = (thread_arg_t*)arg;
    targ->op(targ->data);
    return NULL;
}

// Original threaded version (slow)
void tick_execute_threaded(tick_unit_t* unit) {
    pthread_t threads[8];
    thread_arg_t args[8];
    int thread_count = 0;
    
    // Launch threads for active operations
    for (int i = 0; i < 8; i++) {
        if (unit->tick_mask & (1 << i)) {
            args[thread_count].op = unit->ops[i];
            args[thread_count].data = unit->data[i];
            pthread_create(&threads[thread_count], NULL, thread_worker, &args[thread_count]);
            thread_count++;
        }
    }
    
    // Wait for all threads to complete
    for (int i = 0; i < thread_count; i++) {
        pthread_join(threads[i], NULL);
    }
}

// Optimized inline version - no threading overhead
__attribute__((hot, always_inline))
void tick_execute(tick_unit_t* unit) {
    // Direct inline execution - much faster than threading
    uint64_t mask = unit->tick_mask;
    
    // Unrolled loop with branch-free execution
    if (mask & 0x01) unit->ops[0](unit->data[0]);
    if (mask & 0x02) unit->ops[1](unit->data[1]);
    if (mask & 0x04) unit->ops[2](unit->data[2]);
    if (mask & 0x08) unit->ops[3](unit->data[3]);
    if (mask & 0x10) unit->ops[4](unit->data[4]);
    if (mask & 0x20) unit->ops[5](unit->data[5]);
    if (mask & 0x40) unit->ops[6](unit->data[6]);
    if (mask & 0x80) unit->ops[7](unit->data[7]);
}