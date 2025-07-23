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

void tick_execute(tick_unit_t* unit) {
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