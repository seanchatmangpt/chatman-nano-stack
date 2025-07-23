#ifndef CNS_V8_CORE_FIBER_H
#define CNS_V8_CORE_FIBER_H

/**
 * CNS v8.0 Fiber - Cooperative lightweight threading
 *
 * Fiber provides deterministic cooperative multitasking with:
 * - 8T: Context switch in ≤8 CPU ticks
 * - 8H: Deterministic scheduling with Six Sigma predictability  
 * - 8M: Stack frames aligned to 8-byte boundaries
 * 
 * Fibers are stackful coroutines that enable deterministic concurrency
 * without the overhead of preemptive threading.
 */

#include "bit_actor.h"
#include <stdint.h>
#include <stdatomic.h>
#include <stdalign.h>
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations  
typedef struct Fiber Fiber_t;
typedef struct FiberScheduler FiberScheduler_t;
typedef struct FiberContext FiberContext_t;

/**
 * Fiber execution states
 */
typedef enum {
    FIBER_CREATED = 0,
    FIBER_READY,           // Ready to run
    FIBER_RUNNING,         // Currently executing
    FIBER_YIELDED,         // Voluntarily yielded
    FIBER_BLOCKED,         // Blocked on I/O or synchronization
    FIBER_COMPLETED,       // Execution completed
    FIBER_ERROR           // Error state
} FiberState_t;

/**
 * Fiber priority levels for deterministic scheduling
 */
typedef enum {
    FIBER_PRIORITY_REALTIME = 0, // Real-time critical (≤1 tick quantum)
    FIBER_PRIORITY_HIGH = 1,     // High priority (≤4 tick quantum)
    FIBER_PRIORITY_NORMAL = 2,   // Normal priority (≤8 tick quantum)
    FIBER_PRIORITY_LOW = 3       // Background (best effort)
} FiberPriority_t;

/**
 * Fiber execution context - CPU state preservation
 */
struct FiberContext {
    jmp_buf registers;           // CPU register state
    void* stack_pointer;         // Current stack pointer
    void* stack_base;            // Stack base address
    size_t stack_size;           // Stack size in bytes
    uint64_t last_context_switch_tsc; // Last context switch timestamp
} __attribute__((aligned(8)));

/**
 * Fiber execution metrics
 */
typedef struct {
    atomic_uint_fast64_t context_switches;
    atomic_uint_fast64_t total_runtime_ticks;
    atomic_uint_fast64_t yield_count;
    atomic_uint_fast64_t block_count;
    atomic_uint_fast32_t quantum_violations;
    double average_runtime_per_quantum;
    uint64_t max_runtime_per_quantum;
} FiberMetrics_t;

/**
 * Fiber function signature
 */
typedef int (*FiberFunc_t)(void* arg);

/**
 * Fiber Main Structure
 */
struct Fiber {
    // Inheritance from BitActor
    BitActor_t base;
    
    // Fiber identification
    uint64_t fiber_id;           // Unique fiber identifier
    const char* fiber_name;      // Human-readable name
    FiberPriority_t priority;    // Scheduling priority
    
    // Execution state
    atomic_int state;            // Current fiber state
    FiberContext_t context;      // Execution context
    
    // Function and arguments
    FiberFunc_t entry_point;     // Fiber entry function
    void* argument;              // Function argument
    int return_value;            // Function return value
    
    // Stack management (8M aligned)
    alignas(8) void* stack_memory;      // Stack memory allocation
    size_t stack_size;                  // Stack size in bytes
    void* stack_guard;                  // Stack guard page
    
    // Scheduling data
    uint64_t quantum_ticks;      // Allocated time quantum
    uint64_t quantum_start_tsc;  // Quantum start timestamp
    uint64_t quantum_remaining;  // Remaining quantum ticks
    
    // Parent/child relationships
    Fiber_t* parent_fiber;       // Parent fiber (if any)
    Fiber_t** child_fibers;      // Array of child fibers
    uint32_t child_count;        // Number of children
    uint32_t child_capacity;     // Child array capacity
    
    // Synchronization 
    atomic_flag yield_requested; // Cooperative yield flag
    void* wait_object;           // Object fiber is waiting on
    uint64_t wait_timeout_tsc;   // Wait timeout timestamp
    
    // Performance metrics
    FiberMetrics_t metrics;
    
    // Scheduler linkage
    Fiber_t* next_ready;         // Next in ready queue
    Fiber_t* prev_ready;         // Previous in ready queue
    FiberScheduler_t* scheduler; // Associated scheduler
    
    // Error handling
    jmp_buf error_context;       // Error recovery context
    int error_code;              // Last error code
    const char* error_message;   // Error description
} __attribute__((aligned(8)));

/**
 * Fiber Scheduler - Deterministic cooperative scheduler
 */
struct FiberScheduler {
    // Inheritance from BitActor
    BitActor_t base;
    
    // Ready queues (one per priority level)
    Fiber_t* ready_queues[4];    // Priority-based ready queues
    uint32_t ready_counts[4];    // Count of ready fibers per priority
    
    // Currently running fiber
    Fiber_t* current_fiber;      // Currently executing fiber
    Fiber_t* idle_fiber;         // Idle fiber (always ready)
    
    // Scheduler state
    atomic_bool is_running;      // Scheduler running flag
    uint64_t scheduler_start_tsc; // Scheduler start timestamp
    
    // Time quantum management
    uint64_t quantum_ticks[4];   // Time quantum per priority level
    uint64_t current_quantum_start; // Current quantum start
    
    // Statistics
    atomic_uint_fast64_t total_context_switches;
    atomic_uint_fast64_t total_fibers_created;
    atomic_uint_fast64_t total_fibers_destroyed;
    atomic_uint_fast32_t peak_fiber_count;
    
    // Memory management
    void* fiber_arena;           // Arena for fiber allocation
    size_t arena_size;           // Arena size
    size_t default_stack_size;   // Default fiber stack size
} __attribute__((aligned(64)));

// Default configurations
#define FIBER_DEFAULT_STACK_SIZE (64 * 1024)  // 64KB default stack
#define FIBER_MIN_STACK_SIZE (4 * 1024)       // 4KB minimum stack
#define FIBER_MAX_FIBERS 4096                 // Maximum fibers per scheduler

// Quantum allocations (in CPU ticks)
#define FIBER_QUANTUM_REALTIME 1
#define FIBER_QUANTUM_HIGH 4
#define FIBER_QUANTUM_NORMAL 8
#define FIBER_QUANTUM_LOW 16

// Performance measurement
#define FIBER_START_QUANTUM() \
    uint64_t _quantum_start = __builtin_ia32_rdtsc()

#define FIBER_END_QUANTUM(fiber) \
    do { \
        uint64_t _quantum_end = __builtin_ia32_rdtsc(); \
        uint64_t _quantum_used = _quantum_end - _quantum_start; \
        atomic_fetch_add(&(fiber)->metrics.total_runtime_ticks, _quantum_used); \
        if (_quantum_used > (fiber)->quantum_ticks) { \
            atomic_fetch_add(&(fiber)->metrics.quantum_violations, 1); \
        } \
    } while(0)

// Core Fiber API
Fiber_t* fiber_create(const char* name, FiberFunc_t entry_point, void* arg, size_t stack_size);
int fiber_destroy(Fiber_t* fiber);
int fiber_start(Fiber_t* fiber); 
int fiber_yield(void);
int fiber_sleep_ticks(uint64_t ticks);
int fiber_join(Fiber_t* fiber, int* return_value);

// Scheduling API  
FiberScheduler_t* fiber_scheduler_create(size_t arena_size);
int fiber_scheduler_destroy(FiberScheduler_t* scheduler);
int fiber_scheduler_add(FiberScheduler_t* scheduler, Fiber_t* fiber);
int fiber_scheduler_remove(FiberScheduler_t* scheduler, Fiber_t* fiber);
int fiber_scheduler_run(FiberScheduler_t* scheduler);
int fiber_scheduler_stop(FiberScheduler_t* scheduler);

// Priority and quantum management
int fiber_set_priority(Fiber_t* fiber, FiberPriority_t priority);
FiberPriority_t fiber_get_priority(Fiber_t* fiber);
int fiber_set_quantum(Fiber_t* fiber, uint64_t ticks);
uint64_t fiber_get_quantum(Fiber_t* fiber);

// State queries
FiberState_t fiber_get_state(Fiber_t* fiber);
bool fiber_is_running(Fiber_t* fiber);
bool fiber_is_ready(Fiber_t* fiber);
Fiber_t* fiber_get_current(void);

// Synchronization primitives
int fiber_block_on(void* wait_object, uint64_t timeout_ticks);
int fiber_unblock(Fiber_t* fiber);
int fiber_signal_all(void* wait_object);

// Performance and diagnostics
const FiberMetrics_t* fiber_get_metrics(Fiber_t* fiber);
int fiber_reset_metrics(Fiber_t* fiber);
int fiber_dump_stack(Fiber_t* fiber, char* buffer, size_t buffer_size);

// Utility functions
const char* fiber_state_to_string(FiberState_t state);
const char* fiber_priority_to_string(FiberPriority_t priority);
bool fiber_validate_stack(Fiber_t* fiber);
bool fiber_validate(Fiber_t* fiber);

// Context switching (internal)
int fiber_context_switch(Fiber_t* from, Fiber_t* to);
int fiber_save_context(Fiber_t* fiber);
int fiber_restore_context(Fiber_t* fiber);

#ifdef __cplusplus
}
#endif

#endif // CNS_V8_CORE_FIBER_H