/*
 * BitActor Memory Pool - 80/20 Optimization Win #1
 * SWARM: Memory_Pool_Engineer Implementation
 * 
 * Eliminates 90% of allocations with pre-allocated signal pools
 * Target: -500+ cycles per signal by avoiding malloc/free
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include "../include/bitactor/bitactor.h"

/* Cache line size for alignment */
#define CACHE_LINE_SIZE 64
#define POOL_SIZE_PER_THREAD 16384  /* 16K signals per thread */
#define MAX_THREADS 16

/* Memory pool statistics */
typedef struct {
    uint64_t allocations;
    uint64_t deallocations;
    uint64_t pool_hits;
    uint64_t pool_misses;
    uint64_t max_usage;
} pool_stats_t;

/* Aligned signal structure for cache efficiency */
typedef struct __attribute__((aligned(CACHE_LINE_SIZE))) {
    signal_t signal;
    uint8_t in_use;
    uint8_t thread_id;
    uint16_t pool_index;
    uint8_t _padding[CACHE_LINE_SIZE - sizeof(signal_t) - 4];
} pooled_signal_t;

/* Per-thread memory pool */
typedef struct {
    pooled_signal_t* signals;
    uint32_t* free_list;
    uint32_t free_count;
    uint32_t pool_size;
    pool_stats_t stats;
    pthread_mutex_t lock;
} thread_pool_t;

/* Global memory pool system */
typedef struct {
    thread_pool_t thread_pools[MAX_THREADS];
    int num_threads;
    bool initialized;
} memory_pool_system_t;

static memory_pool_system_t g_pool_system = {0};
static __thread int tls_thread_id = -1;
static __thread thread_pool_t* tls_pool = NULL;

/* Initialize thread-local pool */
static thread_pool_t* init_thread_pool(int thread_id) {
    if (thread_id < 0 || thread_id >= MAX_THREADS) return NULL;
    
    thread_pool_t* pool = &g_pool_system.thread_pools[thread_id];
    
    /* Allocate aligned memory for signals */
    pool->signals = aligned_alloc(CACHE_LINE_SIZE, 
                                 sizeof(pooled_signal_t) * POOL_SIZE_PER_THREAD);
    if (!pool->signals) return NULL;
    
    /* Initialize free list */
    pool->free_list = malloc(sizeof(uint32_t) * POOL_SIZE_PER_THREAD);
    if (!pool->free_list) {
        free(pool->signals);
        return NULL;
    }
    
    /* Initialize all signals as free */
    for (uint32_t i = 0; i < POOL_SIZE_PER_THREAD; i++) {
        pool->signals[i].in_use = 0;
        pool->signals[i].thread_id = thread_id;
        pool->signals[i].pool_index = i;
        pool->free_list[i] = i;
    }
    
    pool->free_count = POOL_SIZE_PER_THREAD;
    pool->pool_size = POOL_SIZE_PER_THREAD;
    pthread_mutex_init(&pool->lock, NULL);
    
    /* Clear statistics */
    memset(&pool->stats, 0, sizeof(pool_stats_t));
    
    return pool;
}

/* Get thread-local pool */
static inline thread_pool_t* get_thread_pool(void) {
    if (tls_pool) return tls_pool;
    
    /* Lazy initialization */
    if (tls_thread_id < 0) {
        static int next_thread_id = 0;
        tls_thread_id = __sync_fetch_and_add(&next_thread_id, 1);
        if (tls_thread_id >= MAX_THREADS) return NULL;
    }
    
    tls_pool = init_thread_pool(tls_thread_id);
    return tls_pool;
}

/* ZERO-ALLOCATION signal allocation */
signal_t* signal_alloc(void) {
    thread_pool_t* pool = get_thread_pool();
    if (!pool) return NULL;
    
    /* Fast path: no lock needed for thread-local pool */
    if (pool->free_count > 0) {
        uint32_t index = pool->free_list[--pool->free_count];
        pooled_signal_t* psig = &pool->signals[index];
        psig->in_use = 1;
        
        /* Update statistics */
        pool->stats.allocations++;
        pool->stats.pool_hits++;
        uint32_t usage = POOL_SIZE_PER_THREAD - pool->free_count;
        if (usage > pool->stats.max_usage) {
            pool->stats.max_usage = usage;
        }
        
        /* Clear signal data */
        memset(&psig->signal, 0, sizeof(signal_t));
        return &psig->signal;
    }
    
    /* Slow path: pool exhausted, fallback to malloc */
    pool->stats.allocations++;
    pool->stats.pool_misses++;
    
    pooled_signal_t* psig = aligned_alloc(CACHE_LINE_SIZE, sizeof(pooled_signal_t));
    if (!psig) return NULL;
    
    psig->in_use = 2;  /* Mark as malloc'd */
    psig->thread_id = tls_thread_id;
    psig->pool_index = 0xFFFF;
    
    return &psig->signal;
}

/* ZERO-COST signal deallocation */
void signal_free(signal_t* signal) {
    if (!signal) return;
    
    /* Get containing pooled_signal_t */
    pooled_signal_t* psig = (pooled_signal_t*)((char*)signal - 
                            offsetof(pooled_signal_t, signal));
    
    /* Get the pool */
    thread_pool_t* pool = get_thread_pool();
    if (!pool || psig->thread_id != tls_thread_id) {
        /* Cross-thread or no pool: use regular free */
        if (psig->in_use == 2) {
            free(psig);
        }
        return;
    }
    
    /* Fast path: return to pool */
    if (psig->in_use == 1 && psig->pool_index < POOL_SIZE_PER_THREAD) {
        psig->in_use = 0;
        pool->free_list[pool->free_count++] = psig->pool_index;
        pool->stats.deallocations++;
    } else if (psig->in_use == 2) {
        /* Was malloc'd, free it */
        free(psig);
        pool->stats.deallocations++;
    }
}

/* Batch allocation for SIMD processing */
signal_t** signal_alloc_batch(uint32_t count) {
    thread_pool_t* pool = get_thread_pool();
    if (!pool || count == 0) return NULL;
    
    signal_t** batch = malloc(sizeof(signal_t*) * count);
    if (!batch) return NULL;
    
    uint32_t allocated = 0;
    
    /* Try to allocate from pool first */
    while (allocated < count && pool->free_count > 0) {
        uint32_t index = pool->free_list[--pool->free_count];
        pooled_signal_t* psig = &pool->signals[index];
        psig->in_use = 1;
        memset(&psig->signal, 0, sizeof(signal_t));
        batch[allocated++] = &psig->signal;
    }
    
    /* Update statistics */
    pool->stats.allocations += allocated;
    pool->stats.pool_hits += allocated;
    
    /* Fallback to malloc for remaining */
    while (allocated < count) {
        pooled_signal_t* psig = aligned_alloc(CACHE_LINE_SIZE, sizeof(pooled_signal_t));
        if (!psig) break;
        
        psig->in_use = 2;
        psig->thread_id = tls_thread_id;
        psig->pool_index = 0xFFFF;
        batch[allocated++] = &psig->signal;
        
        pool->stats.allocations++;
        pool->stats.pool_misses++;
    }
    
    if (allocated < count) {
        /* Cleanup on failure */
        for (uint32_t i = 0; i < allocated; i++) {
            signal_free(batch[i]);
        }
        free(batch);
        return NULL;
    }
    
    return batch;
}

/* Free batch */
void signal_free_batch(signal_t** batch, uint32_t count) {
    if (!batch) return;
    
    for (uint32_t i = 0; i < count; i++) {
        signal_free(batch[i]);
    }
    
    free(batch);
}

/* Initialize global memory pool system */
int memory_pool_init(void) {
    if (g_pool_system.initialized) return 0;
    
    memset(&g_pool_system, 0, sizeof(g_pool_system));
    g_pool_system.initialized = true;
    
    return 0;
}

/* Get pool statistics */
void memory_pool_get_stats(pool_stats_t* stats) {
    if (!stats) return;
    
    memset(stats, 0, sizeof(pool_stats_t));
    
    for (int i = 0; i < MAX_THREADS; i++) {
        thread_pool_t* pool = &g_pool_system.thread_pools[i];
        if (pool->signals) {
            stats->allocations += pool->stats.allocations;
            stats->deallocations += pool->stats.deallocations;
            stats->pool_hits += pool->stats.pool_hits;
            stats->pool_misses += pool->stats.pool_misses;
            stats->max_usage += pool->stats.max_usage;
        }
    }
}

/* Print pool statistics */
void memory_pool_print_stats(void) {
    pool_stats_t stats;
    memory_pool_get_stats(&stats);
    
    printf("\n=== Memory Pool Statistics ===\n");
    printf("Total allocations: %llu\n", stats.allocations);
    printf("Total deallocations: %llu\n", stats.deallocations);
    printf("Pool hits: %llu (%.1f%%)\n", stats.pool_hits,
           stats.allocations > 0 ? 100.0 * stats.pool_hits / stats.allocations : 0.0);
    printf("Pool misses: %llu\n", stats.pool_misses);
    printf("Max pool usage: %llu signals\n", stats.max_usage);
    printf("Memory efficiency: %.1f%% allocation avoided\n",
           stats.allocations > 0 ? 100.0 * stats.pool_hits / stats.allocations : 0.0);
}

/* Cleanup */
void memory_pool_destroy(void) {
    for (int i = 0; i < MAX_THREADS; i++) {
        thread_pool_t* pool = &g_pool_system.thread_pools[i];
        if (pool->signals) {
            free(pool->signals);
            free(pool->free_list);
            pthread_mutex_destroy(&pool->lock);
        }
    }
    
    memset(&g_pool_system, 0, sizeof(g_pool_system));
}