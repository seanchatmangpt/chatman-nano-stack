/*
 * BitActor NIF - Ultra-High-Frequency Trading Engine
 * Copyright (C) 2025 CNS - Chatman Nano Stack
 * 
 * CRITICAL PATH: Sub-microsecond latency for UHFT
 */

#include <erl_nif.h>
#include <stdint.h>
#include <string.h>
#include <immintrin.h>  // SIMD intrinsics
#include <pthread.h>
#include <stdbool.h>
#include <time.h>

#define MAX_ACTORS 10000
#define CACHE_LINE_SIZE 64
#define RDTSC_OVERHEAD 40  // Calibrated rdtsc overhead

// Aligned to cache line to prevent false sharing
typedef struct __attribute__((aligned(CACHE_LINE_SIZE))) {
    uint64_t id;
    uint64_t type;
    volatile uint64_t state;
    volatile uint64_t message_count;
    volatile uint64_t last_tick;
    
    // Lock-free message queue
    volatile uint64_t head;
    volatile uint64_t tail;
    void* messages[1024];  // Power of 2 for fast modulo
    
    // Performance metrics
    uint64_t total_latency_ns;
    uint64_t tick_count;
    
    char padding[CACHE_LINE_SIZE - 80];  // Ensure cache alignment
} actor_t;

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

// High-precision timing using RDTSC
static inline uint64_t rdtsc() {
    unsigned int lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
}

// Convert TSC to nanoseconds (calibrated at runtime)
static double g_tsc_to_ns = 0.0;

static void calibrate_rdtsc() {
    struct timespec start, end;
    uint64_t tsc_start, tsc_end;
    
    clock_gettime(CLOCK_MONOTONIC, &start);
    tsc_start = rdtsc();
    
    // Busy wait for 10ms
    volatile int sum = 0;
    for (int i = 0; i < 10000000; i++) sum += i;
    
    tsc_end = rdtsc();
    clock_gettime(CLOCK_MONOTONIC, &end);
    
    uint64_t ns_elapsed = (end.tv_sec - start.tv_sec) * 1000000000ULL + 
                         (end.tv_nsec - start.tv_nsec);
    uint64_t tsc_elapsed = tsc_end - tsc_start;
    
    g_tsc_to_ns = (double)ns_elapsed / (double)tsc_elapsed;
}

// SIMD-optimized tick processing
static void tick_actors_simd(actor_t** actors, size_t count) {
    __m256i tick_increment = _mm256_set1_epi64x(1);
    
    for (size_t i = 0; i < count; i += 4) {
        // Process 4 actors at once with AVX2
        __m256i ticks = _mm256_loadu_si256((__m256i*)&actors[i]->tick_count);
        ticks = _mm256_add_epi64(ticks, tick_increment);
        _mm256_storeu_si256((__m256i*)&actors[i]->tick_count, ticks);
    }
}

// Lock-free message enqueue
static bool enqueue_message(actor_t* actor, void* msg) {
    uint64_t head = actor->head;
    uint64_t next_head = (head + 1) & 1023;  // Fast modulo
    
    if (next_head == actor->tail) {
        return false;  // Queue full
    }
    
    actor->messages[head] = msg;
    __atomic_store_n(&actor->head, next_head, __ATOMIC_RELEASE);
    __atomic_add_fetch(&actor->message_count, 1, __ATOMIC_RELAXED);
    
    return true;
}

// NIF: create_actor/2
static ERL_NIF_TERM create_actor_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t start_tsc = rdtsc();
    
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
    
    actor_t* actor = enif_alloc_resource(g_actor_resource_type, sizeof(actor_t));
    memset(actor, 0, sizeof(actor_t));
    
    actor->id = g_state->actor_count;
    actor->type = type;
    actor->state = 1;  // Active
    actor->head = 0;
    actor->tail = 0;
    
    g_state->actors[g_state->actor_count++] = actor;
    
    pthread_mutex_unlock(&g_state->mutex);
    
    uint64_t end_tsc = rdtsc();
    uint64_t latency_ns = (uint64_t)((end_tsc - start_tsc - RDTSC_OVERHEAD) * g_tsc_to_ns);
    
    ERL_NIF_TERM result = enif_make_resource(env, actor);
    enif_release_resource(actor);
    
    return enif_make_tuple3(env,
        enif_make_atom(env, "ok"),
        result,
        enif_make_uint64(env, latency_ns));
}

// NIF: send_message/2
static ERL_NIF_TERM send_message_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t start_tsc = rdtsc();
    
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    
    actor_t* actor;
    if (!enif_get_resource(env, argv[0], g_actor_resource_type, (void**)&actor)) {
        return enif_make_badarg(env);
    }
    
    // For UHFT, we store the term reference directly
    void* msg = (void*)(uintptr_t)argv[1];
    
    if (!enqueue_message(actor, msg)) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "queue_full"));
    }
    
    __atomic_add_fetch(&g_state->total_messages, 1, __ATOMIC_RELAXED);
    
    uint64_t end_tsc = rdtsc();
    uint64_t latency_ns = (uint64_t)((end_tsc - start_tsc - RDTSC_OVERHEAD) * g_tsc_to_ns);
    
    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, latency_ns));
}

// NIF: tick_all/0 - Process all actors
static ERL_NIF_TERM tick_all_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t start_tsc = rdtsc();
    
    // Use SIMD for batch processing
    if (g_state->actor_count >= 4) {
        tick_actors_simd(g_state->actors, g_state->actor_count);
    } else {
        // Fallback for small counts
        for (size_t i = 0; i < g_state->actor_count; i++) {
            g_state->actors[i]->tick_count++;
            g_state->actors[i]->last_tick = rdtsc();
        }
    }
    
    __atomic_add_fetch(&g_state->total_ticks, 1, __ATOMIC_RELAXED);
    
    uint64_t end_tsc = rdtsc();
    uint64_t latency_ns = (uint64_t)((end_tsc - start_tsc - RDTSC_OVERHEAD) * g_tsc_to_ns);
    
    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, latency_ns));
}

// NIF: measure_latency/0 - Get timing overhead
static ERL_NIF_TERM measure_latency_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t measurements[100];
    
    // Warm up
    for (int i = 0; i < 10; i++) {
        volatile uint64_t dummy = rdtsc();
        (void)dummy;
    }
    
    // Measure
    for (int i = 0; i < 100; i++) {
        uint64_t start = rdtsc();
        uint64_t end = rdtsc();
        measurements[i] = end - start;
    }
    
    // Calculate stats
    uint64_t min = UINT64_MAX, max = 0, sum = 0;
    for (int i = 0; i < 100; i++) {
        if (measurements[i] < min) min = measurements[i];
        if (measurements[i] > max) max = measurements[i];
        sum += measurements[i];
    }
    
    return enif_make_tuple4(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, (uint64_t)(min * g_tsc_to_ns)),
        enif_make_uint64(env, (uint64_t)(sum * g_tsc_to_ns / 100)),
        enif_make_uint64(env, (uint64_t)(max * g_tsc_to_ns)));
}

// NIF: get_stats/0
static ERL_NIF_TERM get_stats_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_tuple5(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, g_state->actor_count),
        enif_make_uint64(env, g_state->total_messages),
        enif_make_uint64(env, g_state->total_ticks),
        enif_make_double(env, g_tsc_to_ns));
}

// Resource cleanup
static void actor_resource_destructor(ErlNifEnv* env, void* obj) {
    // Cleanup if needed
}

// Module initialization
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
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
    
    // Calibrate timing
    calibrate_rdtsc();
    
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
    {"send_message", 2, send_message_nif, 0},
    {"tick_all", 0, tick_all_nif, 0},
    {"measure_latency", 0, measure_latency_nif, 0},
    {"get_stats", 0, get_stats_nif, 0}
};

ERL_NIF_INIT(bitactor_nif, nif_funcs, load, NULL, NULL, unload)