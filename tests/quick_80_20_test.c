/*
 * Quick 80/20 Optimization Test
 * Reduced workload for fast validation
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/time.h>
#include "../bitactor/include/bitactor/bitactor.h"
#include "../bitactor/include/bitactor/bitactor_cache_aligned.h"

/* Reduced test size */
#define QUICK_TEST_SIZE 10000

/* External functions */
extern int memory_pool_init(void);
extern signal_t* signal_alloc(void);
extern void signal_free(signal_t* signal);
extern void memory_pool_print_stats(void);
extern int fast_dispatch_init(void);
extern int process_signals_fast(const signal_opt_t* signals, 
                               result_opt_t* results, uint32_t count);
extern void fast_dispatch_get_stats(void);

/* Get time in microseconds */
static uint64_t get_time_us(void) {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec * 1000000ULL + tv.tv_usec;
}

/* Generate test signal */
static void generate_signal(signal_opt_t* sig, uint32_t id) {
    sig->id = id;
    sig->type = (id % 10 < 3) ? 0xFF : 0x10;  /* 30% heartbeat, 70% market */
    sig->payload = 0x1234567890ABCDEF;
    sig->flags = 0;
    sig->timestamp = get_time_us();
    sig->priority = 1;
    sig->kind = sig->type;
    sig->context = 0;
}

int main(void) {
    printf("=== Quick 80/20 Optimization Test ===\\n");
    printf("Test size: %d signals\\n\\n", QUICK_TEST_SIZE);
    
    /* Initialize optimizations */
    memory_pool_init();
    fast_dispatch_init();
    
    /* Test 1: Baseline (malloc) */
    printf("1. BASELINE Test (malloc):\\n");
    uint64_t start = get_time_us();
    
    signal_opt_t* baseline_signals = malloc(sizeof(signal_opt_t) * QUICK_TEST_SIZE);
    result_opt_t* baseline_results = malloc(sizeof(result_opt_t) * QUICK_TEST_SIZE);
    
    for (int i = 0; i < QUICK_TEST_SIZE; i++) {
        generate_signal(&baseline_signals[i], i);
        /* Simple processing */
        baseline_results[i].signal_id = baseline_signals[i].id;
        baseline_results[i].ticks = 2;
    }
    
    uint64_t baseline_time = get_time_us() - start;
    printf("   Time: %llu us\\n", baseline_time);
    printf("   Per signal: %.2f us\\n", (double)baseline_time / QUICK_TEST_SIZE);
    
    free(baseline_signals);
    free(baseline_results);
    
    /* Test 2: Memory Pool */
    printf("\\n2. MEMORY POOL Test:\\n");
    start = get_time_us();
    
    signal_t** pooled_signals = malloc(sizeof(signal_t*) * QUICK_TEST_SIZE);
    for (int i = 0; i < QUICK_TEST_SIZE; i++) {
        pooled_signals[i] = signal_alloc();
        pooled_signals[i]->id = i;
        pooled_signals[i]->type = (i % 10 < 3) ? 0xFF : 0x10;
    }
    
    for (int i = 0; i < QUICK_TEST_SIZE; i++) {
        signal_free(pooled_signals[i]);
    }
    
    uint64_t pool_time = get_time_us() - start;
    printf("   Time: %llu us\\n", pool_time);
    printf("   Per signal: %.2f us\\n", (double)pool_time / QUICK_TEST_SIZE);
    printf("   Speedup: %.1fx\\n", (double)baseline_time / pool_time);
    
    free(pooled_signals);
    
    /* Test 3: Cache-aligned + Fast Dispatch */
    printf("\\n3. CACHE-ALIGNED + FAST DISPATCH Test:\\n");
    start = get_time_us();
    
    signal_opt_t* opt_signals = cache_aligned_alloc(sizeof(signal_opt_t) * QUICK_TEST_SIZE);
    result_opt_t* opt_results = cache_aligned_alloc(sizeof(result_opt_t) * QUICK_TEST_SIZE);
    
    for (int i = 0; i < QUICK_TEST_SIZE; i++) {
        generate_signal(&opt_signals[i], i);
    }
    
    /* Process in batches */
    const int BATCH_SIZE = 32;
    for (int i = 0; i < QUICK_TEST_SIZE; i += BATCH_SIZE) {
        int batch_count = (i + BATCH_SIZE <= QUICK_TEST_SIZE) ? 
                         BATCH_SIZE : (QUICK_TEST_SIZE - i);
        process_signals_fast(&opt_signals[i], &opt_results[i], batch_count);
    }
    
    uint64_t opt_time = get_time_us() - start;
    printf("   Time: %llu us\\n", opt_time);
    printf("   Per signal: %.2f us\\n", (double)opt_time / QUICK_TEST_SIZE);
    printf("   Speedup: %.1fx\\n", (double)baseline_time / opt_time);
    
    free(opt_signals);
    free(opt_results);
    
    /* Print statistics */
    printf("\\n=== Optimization Statistics ===\\n");
    memory_pool_print_stats();
    fast_dispatch_get_stats();
    
    /* Summary */
    printf("\\n=== SUMMARY ===\\n");
    printf("Baseline time: %llu us\\n", baseline_time);
    printf("Memory pool speedup: %.1fx\\n", (double)baseline_time / pool_time);
    printf("Full optimization speedup: %.1fx\\n", (double)baseline_time / opt_time);
    
    double total_improvement = (double)baseline_time / opt_time;
    printf("\\n%s 80/20 OPTIMIZATIONS: %.1fx IMPROVEMENT %s\\n",
           total_improvement > 2.0 ? "✅" : "❌",
           total_improvement,
           total_improvement > 2.0 ? "SUCCESS" : "NEEDS MORE WORK");
    
    return 0;
}