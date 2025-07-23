#ifndef BITFIBER_H
#define BITFIBER_H

#include <stdint.h>
#include <stdbool.h>
#include "bitactor.h"

// Fiber states
#define FIBER_STATE_READY     0x00
#define FIBER_STATE_RUNNING   0x01
#define FIBER_STATE_YIELDED   0x02
#define FIBER_STATE_COMPLETE  0x03

// Maximum fibers per actor
#define BITFIBER_MAX_FIBERS   64
#define BITFIBER_STACK_SIZE   512

// Fiber context - minimal state for cooperative scheduling
typedef struct {
    uint32_t state;         // Fiber state
    uint32_t pc;            // Program counter
    uint32_t tick_count;    // Ticks consumed
    uint32_t yield_point;   // Where to resume
    uint64_t regs[8];       // Virtual registers
    uint8_t stack[BITFIBER_STACK_SIZE];  // Local stack
} fiber_t;

// Fiber scheduler - manages cooperative execution
typedef struct {
    fiber_t fibers[BITFIBER_MAX_FIBERS];
    uint32_t active_count;
    uint32_t current_fiber;
    uint64_t schedule_mask;  // Bitmask of ready fibers
} fiber_scheduler_t;

// Fiber API
void fiber_scheduler_init(fiber_scheduler_t* sched);
uint32_t fiber_spawn(fiber_scheduler_t* sched, uint32_t pc);
void fiber_yield(fiber_scheduler_t* sched);
void fiber_complete(fiber_scheduler_t* sched);
void fiber_schedule_tick(fiber_scheduler_t* sched, bitactor_t* ba);

// Inline helpers for performance
__attribute__((always_inline))
static inline fiber_t* fiber_current(fiber_scheduler_t* sched) {
    return &sched->fibers[sched->current_fiber];
}

__attribute__((always_inline))
static inline bool fiber_has_ready(fiber_scheduler_t* sched) {
    return sched->schedule_mask != 0;
}

__attribute__((always_inline))
static inline uint32_t fiber_next_ready(fiber_scheduler_t* sched) {
    return __builtin_ctzll(sched->schedule_mask);
}

#endif