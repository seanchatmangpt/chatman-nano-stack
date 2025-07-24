/*
 * BitActor NIF - Cross-platform Implementation
 * Ultra-High-Frequency Trading Engine
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

// High-precision timing
static inline uint64_t get_time_ns() {
#ifdef __APPLE__
    static mach_timebase_info_data_t timebase = {0, 0};
    if (timebase.denom == 0) {
        mach_timebase_info(&timebase);
    }
    return mach_absolute_time() * timebase.numer / timebase.denom;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#endif
}

// Actor structure
typedef struct {
    uint64_t id;
    uint64_t type;
    volatile uint64_t state;
    volatile uint64_t message_count;
    uint64_t tick_count;
} actor_t;

// Global state
typedef struct {
    actor_t* actors[MAX_ACTORS];
    volatile uint64_t actor_count;
    pthread_mutex_t mutex;
    volatile uint64_t total_messages;
    volatile uint64_t total_ticks;
} bitactor_state_t;

static bitactor_state_t* g_state = NULL;
static ErlNifResourceType* g_actor_resource_type = NULL;

// NIF: create_actor/2
static ERL_NIF_TERM create_actor_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t start_time = get_time_ns();
    
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    
    unsigned long type;
    if (!enif_get_ulong(env, argv[0], &type)) {
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
    actor->type = (uint64_t)type;
    actor->state = 1;
    
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
    
    __atomic_add_fetch(&actor->message_count, 1, __ATOMIC_RELAXED);
    __atomic_add_fetch(&g_state->total_messages, 1, __ATOMIC_RELAXED);
    
    uint64_t end_time = get_time_ns();
    uint64_t latency_ns = end_time - start_time;
    
    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, latency_ns));
}

// NIF: tick_all/0  
static ERL_NIF_TERM tick_all_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv; // Suppress unused warnings
    
    uint64_t start_time = get_time_ns();
    
    for (size_t i = 0; i < g_state->actor_count; i++) {
        if (g_state->actors[i] && g_state->actors[i]->state == 1) {
            g_state->actors[i]->tick_count++;
        }
    }
    
    __atomic_add_fetch(&g_state->total_ticks, 1, __ATOMIC_RELAXED);
    
    uint64_t end_time = get_time_ns();
    uint64_t latency_ns = end_time - start_time;
    
    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, latency_ns));
}

// NIF: measure_latency/0
static ERL_NIF_TERM measure_latency_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv; // Suppress unused warnings
    
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
    (void)argc; (void)argv; // Suppress unused warnings
    
    return enif_make_tuple5(env,
        enif_make_atom(env, "ok"),
        enif_make_uint64(env, g_state->actor_count),
        enif_make_uint64(env, g_state->total_messages),
        enif_make_uint64(env, g_state->total_ticks),
        enif_make_double(env, 1.0));
}

// Module initialization
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    (void)priv_data; (void)load_info; // Suppress unused warnings
    
    g_state = enif_alloc(sizeof(bitactor_state_t));
    memset(g_state, 0, sizeof(bitactor_state_t));
    pthread_mutex_init(&g_state->mutex, NULL);
    
    g_actor_resource_type = enif_open_resource_type(env, NULL, "actor",
        NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    
    if (!g_actor_resource_type) {
        return -1;
    }
    
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
    (void)env; (void)priv_data; // Suppress unused warnings
    
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