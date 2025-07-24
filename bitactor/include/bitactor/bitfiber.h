/*
 * BitFiber - Cooperative Fiber Scheduler
 * Zero-allocation, stackless execution
 */
#ifndef BITFIBER_H
#define BITFIBER_H

#include <stdint.h>
#include <stdbool.h>

/* Fiber execution context - fixed size, no heap */
typedef struct {
    uint64_t regs[8];      /* Saved registers */
    uint64_t ip;           /* Instruction pointer */
    uint64_t sp;           /* Stack pointer */
    uint32_t fiber_id;     /* Fiber identifier */
    uint8_t  status;       /* Fiber status */
    uint8_t  priority;     /* Execution priority */
    uint16_t flags;        /* Fiber flags */
} fiber_context_t;

/* Fiber function prototype */
typedef void (*fiber_fn)(void* arg);

/* Fiber scheduler state - forward declared in bitactor.h */

/* Fiber status codes */
enum {
    FIBER_READY    = 0,
    FIBER_RUNNING  = 1,
    FIBER_COMPLETE = 2,
    FIBER_BLOCKED  = 3
};

/* Core fiber API */
fiber_scheduler_t* fiber_scheduler_init(void);
void fiber_scheduler_destroy(fiber_scheduler_t* sched);

/* Create a fiber - returns fiber ID or -1 on failure */
int32_t fiber_create(fiber_scheduler_t* sched, fiber_fn fn, void* arg);

/* Execute one scheduler tick - returns active fiber count */
uint32_t fiber_tick(fiber_scheduler_t* sched);

/* Yield current fiber - cooperative multitasking */
void fiber_yield(void);

/* Get current fiber ID */
uint32_t fiber_current(void);

/* Fiber memory layout - all pre-allocated */
#define FIBER_STACK_SIZE    2048  /* Per-fiber stack */
#define FIBER_SCRATCH_SIZE  256   /* Scratchpad memory */

#endif /* BITFIBER_H */