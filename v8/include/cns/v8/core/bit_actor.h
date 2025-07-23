#ifndef CNS_V8_CORE_BIT_ACTOR_H
#define CNS_V8_CORE_BIT_ACTOR_H

/**
 * CNS v8.0 BitActor - Quantum computational unit
 * 
 * BitActor represents the fundamental computational unit in CNS v8.0.
 * Every BitActor must comply with the 8T-8H-8M trinity:
 * - 8T: Maximum 8 CPU ticks per operation
 * - 8H: Six Sigma quality (Cpk > 20)
 * - 8M: 8-byte quantum alignment
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <stdalign.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations
typedef struct BitActor BitActor_t;
typedef struct BitActorVTable BitActorVTable_t;

/**
 * BitActor Performance Metrics
 * Tracks real-time performance to enforce 8T-8H-8M contracts
 */
typedef struct {
    atomic_uint_fast64_t tick_count;        // Current CPU tick counter
    atomic_uint_fast32_t operation_count;   // Operations performed
    atomic_uint_fast32_t violation_count;   // Contract violations
    double cpk_current;                     // Current process capability
    uint64_t last_measurement_tsc;          // Last TSC measurement
} BitActorMetrics_t;

/**
 * BitActor State Machine
 * Tracks lifecycle and health of the actor
 */
typedef enum {
    BIT_ACTOR_CREATED = 0,
    BIT_ACTOR_INITIALIZED,
    BIT_ACTOR_RUNNING,
    BIT_ACTOR_SUSPENDED,
    BIT_ACTOR_ERROR,
    BIT_ACTOR_TERMINATED
} BitActorState_t;

/**
 * BitActor Virtual Function Table
 * Enables polymorphic behavior with zero-cost abstractions
 */
struct BitActorVTable {
    // Core lifecycle operations (must complete in ≤8 ticks)
    int (*initialize)(BitActor_t* self);
    int (*execute)(BitActor_t* self, const void* input, void* output);
    int (*suspend)(BitActor_t* self);
    int (*resume)(BitActor_t* self);
    int (*terminate)(BitActor_t* self);
    
    // Health and metrics operations
    int (*health_check)(BitActor_t* self);
    int (*reset_metrics)(BitActor_t* self);
    
    // Self-healing operations
    int (*detect_anomaly)(BitActor_t* self);
    int (*self_repair)(BitActor_t* self);
};

/**
 * BitActor Base Structure
 * All CNS v8.0 computational units inherit from this base
 */
struct BitActor {
    // Memory alignment enforcement (8M contract)
    alignas(8) uint64_t magic_header;       // 0xCNS8_BITA_CTOR8
    
    // Virtual function table for polymorphism
    const BitActorVTable_t* vtable;
    
    // Actor identification and metadata
    uint64_t actor_id;                      // Unique actor identifier
    const char* actor_name;                 // Human-readable name
    uint32_t actor_version;                 // Version number
    
    // State management
    atomic_int state;                       // Current actor state
    atomic_bool is_healthy;                 // Health status flag
    
    // Performance metrics (8T-8H enforcement)
    BitActorMetrics_t metrics;
    
    // Memory management
    void* private_arena;                    // Private memory arena
    size_t arena_size;                      // Arena allocation size
    
    // Timing constraints
    uint32_t max_ticks;                     // Maximum allowed ticks (≤8)
    uint32_t timeout_ns;                    // Timeout in nanoseconds
    
    // Parent and child relationships
    BitActor_t* parent;                     // Parent actor (if any)
    BitActor_t** children;                  // Array of child actors
    uint32_t child_count;                   // Number of children
    uint32_t child_capacity;                // Child array capacity
    
    // Thread safety
    atomic_flag lock;                       // Spinlock for atomic operations
    
    // Self-healing data
    uint64_t last_health_check_tsc;         // Last health check timestamp
    uint32_t consecutive_errors;            // Consecutive error count
    uint32_t recovery_attempts;             // Recovery attempt count
    
    // Padding to ensure 8-byte alignment
    uint8_t padding[8 - (sizeof(void*) % 8)];
} __attribute__((aligned(8)));

// Magic constants
#define BIT_ACTOR_MAGIC 0x8B17AC708ULL
#define BIT_ACTOR_MAX_TICKS 8
#define BIT_ACTOR_MAX_CHILDREN 1024
#define BIT_ACTOR_SIX_SIGMA_CPK 20.0

// Performance measurement macros
#define BIT_ACTOR_START_TIMING(actor) \
    uint64_t _start_tsc = __builtin_ia32_rdtsc()

#define BIT_ACTOR_END_TIMING(actor) \
    do { \
        uint64_t _end_tsc = __builtin_ia32_rdtsc(); \
        uint64_t _delta = _end_tsc - _start_tsc; \
        atomic_fetch_add(&(actor)->metrics.tick_count, _delta); \
        atomic_fetch_add(&(actor)->metrics.operation_count, 1); \
        if (_delta > BIT_ACTOR_MAX_TICKS) { \
            atomic_fetch_add(&(actor)->metrics.violation_count, 1); \
        } \
    } while(0)

// Core BitActor API
BitActor_t* bit_actor_create(const char* name, const BitActorVTable_t* vtable);
int bit_actor_destroy(BitActor_t* actor);
int bit_actor_initialize(BitActor_t* actor);
int bit_actor_execute(BitActor_t* actor, const void* input, void* output);
int bit_actor_add_child(BitActor_t* parent, BitActor_t* child);
int bit_actor_remove_child(BitActor_t* parent, BitActor_t* child);

// Health and metrics API
bool bit_actor_is_healthy(BitActor_t* actor);
int bit_actor_check_performance(BitActor_t* actor);
double bit_actor_get_cpk(BitActor_t* actor);
uint64_t bit_actor_get_tick_count(BitActor_t* actor);
uint32_t bit_actor_get_violation_count(BitActor_t* actor);

// Self-healing API
int bit_actor_trigger_self_heal(BitActor_t* actor);
int bit_actor_reset_health_metrics(BitActor_t* actor);

// Utility functions
const char* bit_actor_state_to_string(BitActorState_t state);
bool bit_actor_validate_magic(BitActor_t* actor);
bool bit_actor_validate(BitActor_t* actor);

// Compatibility functions for generated code
void long_destroy(long_t* ptr);
void arena_destroy(BitActor_t* arena);
bool arena_validate(BitActor_t* arena);

#ifdef __cplusplus
}
#endif

#endif // CNS_V8_CORE_BIT_ACTOR_H