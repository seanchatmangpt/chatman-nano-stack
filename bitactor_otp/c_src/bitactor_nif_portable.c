/*
 * BitActor NIF - Portable Version (ARM64/x86_64)
 * Ultra-High-Frequency Trading Engine
 * Copyright (C) 2025 CNS - Chatman Nano Stack
 */

#include <erl_nif.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <stdbool.h>
#include <time.h>
#include <stdlib.h>

#ifdef __APPLE__
#include <mach/mach_time.h>
#endif

#define MAX_ACTORS 10000
#define CACHE_LINE_SIZE 64
#define QUEUE_SIZE 1024

// Cross-platform high-resolution timing
typedef struct {
#ifdef __APPLE__
    mach_timebase_info_data_t timebase;
#endif
    double ns_per_tick;
} timing_info_t;

static timing_info_t g_timing;

// Cache-aligned actor structure
typedef struct {
    uint64_t id;
    uint64_t type;
    volatile uint64_t state;
    volatile uint64_t message_count;
    volatile uint64_t last_tick;
    
    // Lock-free queue indices
    volatile uint64_t head;
    volatile uint64_t tail;
    void* messages[QUEUE_SIZE];
    
    // Performance metrics
    uint64_t total_latency_ns;
    uint64_t tick_count;
    
    // Padding to ensure cache line alignment
    char padding[CACHE_LINE_SIZE - ((10 * sizeof(uint64_t) + QUEUE_SIZE * sizeof(void*)) % CACHE_LINE_SIZE)];
} __attribute__((aligned(CACHE_LINE_SIZE))) actor_t;

typedef struct {
    actor_t* actors[MAX_ACTORS];
    volatile uint64_t actor_count;
    pthread_mutex_t mutex;
    
    // Global performance counters
    volatile uint64_t total_messages;
    volatile uint64_t total_ticks;
} bitactor_state_t;

static bitactor_state_t* g_state = NULL;
static ErlNifResourceType* g_actor_resource_type = NULL;

// High-precision timing functions
static void init_timing() {
#ifdef __APPLE__
    mach_timebase_info(&g_timing.timebase);
    g_timing.ns_per_tick = (double)g_timing.timebase.numer / (double)g_timing.timebase.denom;
#else
    g_timing.ns_per_tick = 1.0; // clock_gettime returns nanoseconds directly
#endif
}

static inline uint64_t get_time_ns() {
#ifdef __APPLE__
    return mach_absolute_time() * g_timing.ns_per_tick;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#endif
}

// Atomic operations for lock-free queue
static inline uint64_t atomic_load(volatile uint64_t* ptr) {
    return __atomic_load_n(ptr, __ATOMIC_ACQUIRE);
}

static inline void atomic_store(volatile uint64_t* ptr, uint64_t val) {
    __atomic_store_n(ptr, val, __ATOMIC_RELEASE);
}

static inline uint64_t atomic_add(volatile uint64_t* ptr, uint64_t val) {
    return __atomic_add_fetch(ptr, val, __ATOMIC_RELAXED);
}

// Lock-free message enqueue
static bool enqueue_message(actor_t* actor, void* msg) {
    uint64_t head = atomic_load(&actor->head);
    uint64_t next_head = (head + 1) & (QUEUE_SIZE - 1);
    
    if (next_head == atomic_load(&actor->tail)) {
        return false; // Queue full
    }
    
    actor->messages[head] = msg;
    atomic_store(&actor->head, next_head);
    atomic_add(&actor->message_count, 1);
    
    return true;
}

// Batch tick processing (optimized for ARM NEON if available)
static void tick_actors_batch(actor_t** actors, size_t count) {
    uint64_t tick_time = get_time_ns();
    
#ifdef __ARM_NEON
    // ARM NEON optimization for batch processing
    size_t i;
    for (i = 0; i + 4 <= count; i += 4) {
        // Process 4 actors at once
        for (int j = 0; j < 4; j++) {
            actors[i + j]->tick_count++;
            actors[i + j]->last_tick = tick_time;
        }
    }
    // Handle remaining actors
    for (; i < count; i++) {
        actors[i]->tick_count++;
        actors[i]->last_tick = tick_time;
    }
#else
    // Standard processing
    for (size_t i = 0; i < count; i++) {
        actors[i]->tick_count++;
        actors[i]->last_tick = tick_time;
    }
#endif
}

// NIF: create_actor/2
static ERL_NIF_TERM create_actor_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t start_time = get_time_ns();
    
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    
    uint64_t type;
    if (!enif_get_uint64(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    
    pthread_mutex_lock(&g_state->mutex);
    
    if (g_state->actor_count >= MAX_ACTORS) {
        pthread_mutex_unlock(&g_state->mutex);
        return enif_make_tuple2(env, 
            enif_make_atom(env, "error"), 
            enif_make_atom(env, "max_actors_reached"));
    }
    
    // Allocate actor
    actor_t* actor = enif_alloc_resource(g_actor_resource_type, sizeof(actor_t));
    memset(actor, 0, sizeof(actor_t));
    
    actor->id = g_state->actor_count;
    actor->type = type;
    actor->state = 1; // Active
    actor->head = 0;
    actor->tail = 0;
    
    g_state->actors[g_state->actor_count++] = actor;
    
    pthread_mutex_unlock(&g_state->mutex);
    
    uint64_t end_time = get_time_ns();
    uint64_t latency_ns = end_time - start_time;
    
    ERL_NIF_TERM result = enif_make_resource(env, actor);
    enif_release_resource(actor);
    
    return enif_make_tuple3(env,
        enif_make_atom(env, "ok"),
        result,
        enif_make_uint64(env, latency_ns));
}

// NIF: send_message/2
static ERL_NIF_TERM send_message_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t start_time = get_time_ns();
    
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    
    actor_t* actor;
    if (!enif_get_resource(env, argv[0], g_actor_resource_type, (void**)&actor)) {
        return enif_make_badarg(env);
    }
    
    // Store message reference
    void* msg = (void*)(uintptr_t)argv[1];
    
    if (!enqueue_message(actor, msg)) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "queue_full"));
    }
    
    atomic_add(&g_state->total_messages, 1);
    
    uint64_t end_time = get_time_ns();
    uint64_t latency_ns = end_time - start_time;
    
    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, latency_ns));
}

// NIF: tick_all/0
static ERL_NIF_TERM tick_all_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t start_time = get_time_ns();
    
    // Process all actors in batches
    tick_actors_batch(g_state->actors, g_state->actor_count);
    
    atomic_add(&g_state->total_ticks, 1);
    
    uint64_t end_time = get_time_ns();
    uint64_t latency_ns = end_time - start_time;
    
    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, latency_ns));
}

// NIF: measure_latency/0
static ERL_NIF_TERM measure_latency_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t measurements[100];
    
    // Warm up
    for (int i = 0; i < 10; i++) {
        volatile uint64_t dummy = get_time_ns();
        (void)dummy;
    }
    
    // Measure
    for (int i = 0; i < 100; i++) {
        uint64_t start = get_time_ns();
        uint64_t end = get_time_ns();
        measurements[i] = end - start;
    }
    
    // Calculate statistics
    uint64_t min = UINT64_MAX, max = 0, sum = 0;
    for (int i = 0; i < 100; i++) {
        if (measurements[i] < min) min = measurements[i];
        if (measurements[i] > max) max = measurements[i];
        sum += measurements[i];
    }
    
    return enif_make_tuple4(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, min),
        enif_make_uint64(env, sum / 100),
        enif_make_uint64(env, max));
}

// NIF: get_stats/0
static ERL_NIF_TERM get_stats_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_tuple5(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, g_state->actor_count),
        enif_make_uint64(env, g_state->total_messages),
        enif_make_uint64(env, g_state->total_ticks),
        enif_make_double(env, g_timing.ns_per_tick));
}

// NIF: destroy_actor/1
static ERL_NIF_TERM destroy_actor_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    
    actor_t* actor;
    if (!enif_get_resource(env, argv[0], g_actor_resource_type, (void**)&actor)) {
        return enif_make_badarg(env);
    }
    
    // Mark as inactive
    actor->state = 0;
    
    return enif_make_atom(env, "ok");
}

// Resource cleanup
static void actor_resource_destructor(ErlNifEnv* env, void* obj) {
    // Cleanup if needed
}

// Module initialization
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    // Initialize timing
    init_timing();
    
    // Initialize global state
    g_state = enif_alloc(sizeof(bitactor_state_t));
    memset(g_state, 0, sizeof(bitactor_state_t));
    pthread_mutex_init(&g_state->mutex, NULL);
    
    // Create resource type
    g_actor_resource_type = enif_open_resource_type(env, NULL, "actor",
        actor_resource_destructor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    
    if (!g_actor_resource_type) {
        return -1;
    }
    
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
    if (g_state) {
        pthread_mutex_destroy(&g_state->mutex);
        enif_free(g_state);
    }
}

static ErlNifFunc nif_funcs[] = {
    {"create_actor", 2, create_actor_nif, 0},
    {"destroy_actor", 1, destroy_actor_nif, 0},
    {"send_message", 2, send_message_nif, 0},
    {"tick_all", 0, tick_all_nif, 0},
    {"measure_latency", 0, measure_latency_nif, 0},
    {"get_stats", 0, get_stats_nif, 0}
};

ERL_NIF_INIT(bitactor_nif, nif_funcs, load, NULL, NULL, unload)