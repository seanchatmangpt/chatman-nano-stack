#ifndef TICK_PARALLEL_H
#define TICK_PARALLEL_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef struct {
    void (*ops[8])(void* data);
    void* data[8];
    uint64_t tick_mask;
} tick_unit_t;

void tick_execute(tick_unit_t* unit);

static inline void* tick_arena_alloc(void* arena, size_t size) {
    void** ptr = (void**)arena;
    void* p = *ptr;
    *ptr = (char*)*ptr + ((size + 63) & ~63);
    return p;
}

#endif