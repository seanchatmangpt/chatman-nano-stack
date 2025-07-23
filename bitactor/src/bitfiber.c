/*
 * BitFiber - Cooperative Fiber Scheduler Implementation
 * Stackless, zero-allocation execution
 */
#include "../include/bitactor/bitfiber.h"
#include <string.h>
#include <stdlib.h>

/* Fiber states */
typedef struct {
    fiber_context_t context;
    fiber_fn function;
    void* arg;
    uint8_t stack[FIBER_STACK_SIZE];
    uint8_t scratch[FIBER_SCRATCH_SIZE];
} fiber_t;

/* Fiber scheduler state */
struct fiber_scheduler {
    fiber_t fibers[BITACTOR_MAX_FIBERS];
    uint32_t active_count;
    uint32_t current_fiber;
    bool initialized;
};

/* Global scheduler instance */
static struct fiber_scheduler g_scheduler = {0};
static __thread uint32_t tls_current_fiber = 0;

/* Assembly helpers for context switching */
extern void fiber_switch(fiber_context_t* from, fiber_context_t* to);

/* Minimal context switch - just save/restore essential registers */
__attribute__((naked)) void fiber_switch_asm(fiber_context_t* from, fiber_context_t* to) {
    __asm__ __volatile__(
        /* Save current context */
        "movq %%rsp, 80(%%rdi)\n"      /* Save RSP */
        "movq %%rbp, 72(%%rdi)\n"      /* Save RBP */
        "movq %%rbx, 0(%%rdi)\n"       /* Save RBX */
        "movq %%r12, 8(%%rdi)\n"       /* Save R12 */
        "movq %%r13, 16(%%rdi)\n"      /* Save R13 */
        "movq %%r14, 24(%%rdi)\n"      /* Save R14 */
        "movq %%r15, 32(%%rdi)\n"      /* Save R15 */
        "leaq 0f(%%rip), %%rax\n"      /* Save return address */
        "movq %%rax, 64(%%rdi)\n"
        
        /* Load new context */
        "movq 80(%%rsi), %%rsp\n"      /* Load RSP */
        "movq 72(%%rsi), %%rbp\n"      /* Load RBP */
        "movq 0(%%rsi), %%rbx\n"       /* Load RBX */
        "movq 8(%%rsi), %%r12\n"       /* Load R12 */
        "movq 16(%%rsi), %%r13\n"      /* Load R13 */
        "movq 24(%%rsi), %%r14\n"      /* Load R14 */
        "movq 32(%%rsi), %%r15\n"      /* Load R15 */
        "movq 64(%%rsi), %%rax\n"      /* Load return address */
        "jmp *%%rax\n"                  /* Jump to new context */
        "0:\n"
        "ret\n"
        ::: "rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"
    );
}

/* Initialize scheduler */
fiber_scheduler_t* fiber_scheduler_init(void) {
    fiber_scheduler_t* sched = &g_scheduler;
    
    if (sched->initialized) {
        return sched;
    }
    
    memset(sched, 0, sizeof(*sched));
    
    /* Initialize main fiber (fiber 0) */
    sched->fibers[0].context.fiber_id = 0;
    sched->fibers[0].context.status = FIBER_RUNNING;
    sched->current_fiber = 0;
    sched->active_count = 1;
    
    sched->initialized = true;
    return sched;
}

/* Destroy scheduler */
void fiber_scheduler_destroy(fiber_scheduler_t* sched) {
    if (sched && sched->initialized) {
        sched->initialized = false;
    }
}

/* Fiber entry point wrapper */
static void fiber_entry(fiber_t* fiber) {
    /* Execute fiber function */
    fiber->function(fiber->arg);
    
    /* Mark as complete */
    fiber->context.status = FIBER_COMPLETE;
    
    /* Yield back to scheduler */
    fiber_yield();
}

/* Create a new fiber */
int32_t fiber_create(fiber_scheduler_t* sched, fiber_fn fn, void* arg) {
    if (!sched || !fn || sched->active_count >= BITACTOR_MAX_FIBERS) {
        return -1;
    }
    
    /* Find free slot */
    uint32_t fiber_id = 0;
    for (uint32_t i = 1; i < BITACTOR_MAX_FIBERS; i++) {
        if (sched->fibers[i].context.status == 0) {
            fiber_id = i;
            break;
        }
    }
    
    if (fiber_id == 0) {
        return -1;
    }
    
    fiber_t* fiber = &sched->fibers[fiber_id];
    memset(fiber, 0, sizeof(*fiber));
    
    /* Set up fiber */
    fiber->function = fn;
    fiber->arg = arg;
    fiber->context.fiber_id = fiber_id;
    fiber->context.status = FIBER_READY;
    
    /* Set up initial stack */
    uint64_t stack_top = (uint64_t)&fiber->stack[FIBER_STACK_SIZE - 16];
    fiber->context.sp = stack_top;
    fiber->context.ip = (uint64_t)fiber_entry;
    
    /* Pass fiber pointer as first argument */
    fiber->context.regs[0] = (uint64_t)fiber;
    
    sched->active_count++;
    return fiber_id;
}

/* Execute one scheduler tick */
uint32_t fiber_tick(fiber_scheduler_t* sched) {
    if (!sched || sched->active_count <= 1) {
        return 0;
    }
    
    uint32_t executed = 0;
    uint32_t start_fiber = sched->current_fiber;
    
    /* Round-robin scheduling */
    do {
        sched->current_fiber = (sched->current_fiber + 1) % BITACTOR_MAX_FIBERS;
        fiber_t* fiber = &sched->fibers[sched->current_fiber];
        
        if (fiber->context.status == FIBER_READY) {
            /* Switch to this fiber */
            fiber_t* current = &sched->fibers[start_fiber];
            fiber->context.status = FIBER_RUNNING;
            tls_current_fiber = sched->current_fiber;
            
            fiber_switch_asm(&current->context, &fiber->context);
            
            executed++;
            break;
        }
    } while (sched->current_fiber != start_fiber);
    
    return executed;
}

/* Yield current fiber */
void fiber_yield(void) {
    fiber_scheduler_t* sched = &g_scheduler;
    if (!sched->initialized) {
        return;
    }
    
    uint32_t current = tls_current_fiber;
    fiber_t* current_fiber = &sched->fibers[current];
    
    /* Mark as ready if still running */
    if (current_fiber->context.status == FIBER_RUNNING) {
        current_fiber->context.status = FIBER_READY;
    }
    
    /* Find next fiber to run */
    uint32_t next = current;
    for (uint32_t i = 1; i < BITACTOR_MAX_FIBERS; i++) {
        uint32_t candidate = (current + i) % BITACTOR_MAX_FIBERS;
        if (sched->fibers[candidate].context.status == FIBER_READY ||
            sched->fibers[candidate].context.status == FIBER_RUNNING) {
            next = candidate;
            break;
        }
    }
    
    if (next != current) {
        fiber_t* next_fiber = &sched->fibers[next];
        next_fiber->context.status = FIBER_RUNNING;
        tls_current_fiber = next;
        
        fiber_switch_asm(&current_fiber->context, &next_fiber->context);
    }
}

/* Get current fiber ID */
uint32_t fiber_current(void) {
    return tls_current_fiber;
}