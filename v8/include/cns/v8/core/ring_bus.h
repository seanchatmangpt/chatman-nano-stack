#ifndef CNS_V8_CORE_RING_BUS_H
#define CNS_V8_CORE_RING_BUS_H

/**
 * CNS v8.0 RingBus - Lock-free inter-actor communication
 *
 * RingBus provides zero-contention message passing between BitActors.
 * Implements Michael & Scott lock-free queue with:
 * - 8T: Single-tick message enqueue/dequeue
 * - 8H: Six Sigma delivery guarantee 
 * - 8M: Cache-line aligned message slots
 */

#include "bit_actor.h"
#include <stdint.h>
#include <stdatomic.h>
#include <stdalign.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations
typedef struct RingBus RingBus_t;
typedef struct RingBusMessage RingBusMessage_t;
typedef struct RingBusSlot RingBusSlot_t;

/**
 * Message priority levels for QoS
 */
typedef enum {
    RING_BUS_PRIORITY_CRITICAL = 0,  // Real-time critical (≤1 tick)
    RING_BUS_PRIORITY_HIGH = 1,      // High priority (≤4 ticks)  
    RING_BUS_PRIORITY_NORMAL = 2,    // Normal priority (≤8 ticks)
    RING_BUS_PRIORITY_LOW = 3        // Background (best effort)
} RingBusPriority_t;

/**
 * Message structure - cache line aligned
 */
struct RingBusMessage {
    alignas(64) uint64_t message_id;     // Unique message identifier
    uint64_t sender_id;                  // Sender BitActor ID
    uint64_t receiver_id;                // Receiver BitActor ID (0 = broadcast)
    uint64_t timestamp_tsc;              // TSC timestamp when sent
    
    RingBusPriority_t priority;          // Message priority
    uint32_t payload_size;               // Payload size in bytes
    uint32_t message_type;               // Application-defined type
    uint32_t sequence_number;            // Sequence number for ordering
    
    // Payload data (maximum 64-byte cache line minus header)
    uint8_t payload[24];                 // Inline payload buffer
    void* extended_payload;              // Pointer to larger payloads
    
    // Delivery tracking
    atomic_bool delivered;               // Delivery confirmation
    atomic_uint_fast32_t retry_count;    // Retry attempts
} __attribute__((aligned(64)));

/**
 * Ring buffer slot - single cache line
 */
struct RingBusSlot {
    alignas(64) atomic_uint_fast64_t sequence;  // Sequence number
    RingBusMessage_t message;                   // Message data
} __attribute__((aligned(64)));

/**
 * RingBus Statistics
 */
typedef struct {
    atomic_uint_fast64_t messages_sent;
    atomic_uint_fast64_t messages_received;
    atomic_uint_fast64_t messages_dropped;
    atomic_uint_fast64_t contention_events;
    atomic_uint_fast64_t retry_events;
    double average_latency_ticks;
    uint64_t peak_latency_ticks;
} RingBusStats_t;

/**
 * RingBus Configuration
 */
typedef struct {
    uint32_t capacity;                   // Ring buffer capacity (power of 2)
    uint32_t max_message_size;           // Maximum message size
    uint32_t timeout_ticks;              // Send timeout in ticks
    bool enable_broadcast;               // Enable broadcast messages
    bool enable_priorities;              // Enable priority queuing
    bool enable_flow_control;            // Enable back-pressure
} RingBusConfig_t;

/**
 * RingBus Main Structure
 */
struct RingBus {
    // Inheritance from BitActor
    BitActor_t base;
    
    // Ring buffer configuration
    RingBusConfig_t config;
    
    // Lock-free ring buffer
    RingBusSlot_t* slots;                // Ring buffer slots
    uint32_t capacity_mask;              // Capacity - 1 (for fast modulo)
    
    // Producer/Consumer cursors (cache line separated)
    alignas(64) atomic_uint_fast64_t producer_cursor;
    alignas(64) atomic_uint_fast64_t consumer_cursor;
    
    // Multi-producer/multi-consumer support
    atomic_uint_fast64_t producer_barrier;
    atomic_uint_fast64_t consumer_barrier;
    
    // Priority queues (if enabled)
    atomic_uint_fast64_t priority_cursors[4];  // One per priority level
    
    // Statistics and monitoring
    RingBusStats_t stats;
    
    // Memory management
    void* slot_arena;                    // Memory arena for slots
    size_t arena_size;                   // Arena size
    
    // Flow control
    atomic_bool flow_control_active;     // Back-pressure active
    uint32_t high_water_mark;            // Flow control threshold
    uint32_t low_water_mark;             // Flow control release
    
    // Health monitoring
    uint64_t last_health_check_tsc;      // Last health check
    atomic_uint_fast32_t error_count;    // Error counter
} __attribute__((aligned(64)));

// Default configuration
#define RING_BUS_DEFAULT_CAPACITY 4096
#define RING_BUS_DEFAULT_MAX_MESSAGE_SIZE 1024
#define RING_BUS_DEFAULT_TIMEOUT_TICKS 8

// Performance measurement
#define RING_BUS_START_TIMING() \
    uint64_t _start_tsc = __builtin_ia32_rdtsc()

#define RING_BUS_END_TIMING(bus, operation) \
    do { \
        uint64_t _end_tsc = __builtin_ia32_rdtsc(); \
        uint64_t _delta = _end_tsc - _start_tsc; \
        if (_delta > (bus)->config.timeout_ticks) { \
            atomic_fetch_add(&(bus)->stats.contention_events, 1); \
        } \
    } while(0)

// Core RingBus API
RingBus_t* ring_bus_create(const RingBusConfig_t* config);
int ring_bus_destroy(RingBus_t* bus);
int ring_bus_initialize(RingBus_t* bus);

// Message passing API
int ring_bus_send(RingBus_t* bus, const RingBusMessage_t* message);
int ring_bus_send_priority(RingBus_t* bus, const RingBusMessage_t* message, RingBusPriority_t priority);
int ring_bus_receive(RingBus_t* bus, RingBusMessage_t* message);
int ring_bus_try_receive(RingBus_t* bus, RingBusMessage_t* message);

// Broadcast API
int ring_bus_broadcast(RingBus_t* bus, const RingBusMessage_t* message);
int ring_bus_subscribe(RingBus_t* bus, uint64_t actor_id);
int ring_bus_unsubscribe(RingBus_t* bus, uint64_t actor_id);

// Statistics and monitoring
const RingBusStats_t* ring_bus_get_stats(RingBus_t* bus);
int ring_bus_reset_stats(RingBus_t* bus);
double ring_bus_utilization(RingBus_t* bus);
bool ring_bus_is_congested(RingBus_t* bus);

// Flow control
int ring_bus_enable_flow_control(RingBus_t* bus, uint32_t high_water, uint32_t low_water);
int ring_bus_disable_flow_control(RingBus_t* bus);
bool ring_bus_is_flow_controlled(RingBus_t* bus);

// Health and diagnostics
int ring_bus_health_check(RingBus_t* bus);
int ring_bus_diagnose(RingBus_t* bus, char* buffer, size_t buffer_size);

// Utility functions
const char* ring_bus_priority_to_string(RingBusPriority_t priority);
RingBusConfig_t ring_bus_default_config(void);
bool ring_bus_validate(RingBus_t* bus);

// Lock-free primitives (internal)
static inline uint64_t ring_bus_next_sequence(RingBus_t* bus, uint64_t current) {
    return (current + 1) & bus->capacity_mask;
}

static inline bool ring_bus_is_full(RingBus_t* bus) {
    uint64_t producer = atomic_load(&bus->producer_cursor);
    uint64_t consumer = atomic_load(&bus->consumer_cursor);
    return (producer - consumer) >= bus->config.capacity;
}

static inline bool ring_bus_is_empty(RingBus_t* bus) {
    uint64_t producer = atomic_load(&bus->producer_cursor);
    uint64_t consumer = atomic_load(&bus->consumer_cursor);
    return producer == consumer;
}

#ifdef __cplusplus
}
#endif

#endif // CNS_V8_CORE_RING_BUS_H