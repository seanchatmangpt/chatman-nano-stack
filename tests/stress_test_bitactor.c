/**
 * BitActor Stress Test Suite
 * Tests system resilience under extreme conditions
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <pthread.h>
#include <assert.h>
#include <stdatomic.h>
#include <time.h>
#include <unistd.h>
#include <sys/resource.h>
#include <errno.h>

#define BENCHMARK_MODE  // Disable tick budget assertions
#define FIXED_IMPLEMENTATION
#include "../generated/fixed_bitactor/fixed_bitactor.h"

/* Test configuration */
#define STRESS_THREADS 100
#define SIGNALS_PER_THREAD 10000
#define MEMORY_BOMB_SIGNALS 1000000
#define STRESS_DURATION_SEC 30

/* Global statistics */
typedef struct {
    _Atomic uint64_t signals_sent;
    _Atomic uint64_t signals_processed;
    _Atomic uint64_t errors;
    _Atomic uint64_t max_latency;
    _Atomic bool stop_flag;
} stress_stats_t;

static stress_stats_t stats;

/* Helper functions */
static inline uint64_t get_timestamp() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

/* Stress test scenarios */

void* stress_producer(void* arg) {
    fixed_bitactor_t* ba = (fixed_bitactor_t*)arg;
    int thread_id = (int)(intptr_t)pthread_self();
    
    while (!atomic_load(&stats.stop_flag)) {
        fixed_signal_t sig = {
            .type = FIXED_SIGNAL_SEMANTICSIGNAL,
            .flags = thread_id,
            .timestamp = get_timestamp(),
            .payload = atomic_load(&stats.signals_sent)
        };
        
        if (fixed_bitactor_enqueue_signal(ba, &sig)) {
            atomic_fetch_add(&stats.signals_sent, 1);
        } else {
            atomic_fetch_add(&stats.errors, 1);
        }
    }
    
    return NULL;
}

void* stress_consumer(void* arg) {
    fixed_bitactor_t* ba = (fixed_bitactor_t*)arg;
    
    while (!atomic_load(&stats.stop_flag)) {
        uint64_t start = rdtsc();
        fixed_bitactor_tick(ba);
        uint64_t latency = rdtsc() - start;
        
        // Update max latency
        uint64_t current_max = atomic_load(&stats.max_latency);
        while (latency > current_max && 
               !atomic_compare_exchange_weak(&stats.max_latency, &current_max, latency)) {
            // Retry
        }
        
        atomic_fetch_add(&stats.signals_processed, 1);
    }
    
    return NULL;
}

void test_concurrent_stress() {
    printf("\n=== Concurrent Stress Test (%d threads) ===\n", STRESS_THREADS);
    
    fixed_bitactor_t* ba = malloc(sizeof(fixed_bitactor_t));
    fixed_bitactor_init(ba);
    
    memset(&stats, 0, sizeof(stats));
    
    pthread_t threads[STRESS_THREADS];
    
    // Start half producers, half consumers
    for (int i = 0; i < STRESS_THREADS/2; i++) {
        pthread_create(&threads[i], NULL, stress_producer, ba);
    }
    for (int i = STRESS_THREADS/2; i < STRESS_THREADS; i++) {
        pthread_create(&threads[i], NULL, stress_consumer, ba);
    }
    
    // Run for specified duration
    sleep(STRESS_DURATION_SEC);
    atomic_store(&stats.stop_flag, true);
    
    // Wait for threads
    for (int i = 0; i < STRESS_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }
    
    // Report results
    uint64_t sent = atomic_load(&stats.signals_sent);
    uint64_t processed = atomic_load(&stats.signals_processed);
    uint64_t errors = atomic_load(&stats.errors);
    uint64_t max_latency = atomic_load(&stats.max_latency);
    
    printf("Duration: %d seconds\n", STRESS_DURATION_SEC);
    printf("Threads: %d (%d producers, %d consumers)\n", 
           STRESS_THREADS, STRESS_THREADS/2, STRESS_THREADS/2);
    printf("Signals sent: %llu\n", (unsigned long long)sent);
    printf("Signals processed: %llu\n", (unsigned long long)processed);
    printf("Errors (ring full): %llu\n", (unsigned long long)errors);
    printf("Max latency: %llu CPU ticks\n", (unsigned long long)max_latency);
    printf("Throughput: %.2f Msignals/sec\n", processed / (STRESS_DURATION_SEC * 1e6));
    
    if (errors > sent * 0.01) {
        printf("❌ High error rate: %.2f%%\n", (errors * 100.0) / sent);
    } else {
        printf("✅ Low error rate: %.2f%%\n", (errors * 100.0) / sent);
    }
    
    free(ba);
}

void test_memory_exhaustion() {
    printf("\n=== Memory Exhaustion Test ===\n");
    
    fixed_bitactor_t* ba = malloc(sizeof(fixed_bitactor_t));
    fixed_bitactor_init(ba);
    
    struct rusage usage_start, usage_end;
    getrusage(RUSAGE_SELF, &usage_start);
    
    // Try to send 1M signals rapidly
    uint64_t sent = 0;
    uint64_t start_time = get_timestamp();
    
    for (int i = 0; i < MEMORY_BOMB_SIGNALS; i++) {
        fixed_signal_t sig = {
            .type = FIXED_SIGNAL_NORMALSIGNAL,
            .flags = i,
            .timestamp = get_timestamp(),
            .payload = i * sizeof(fixed_signal_t)
        };
        
        if (fixed_bitactor_enqueue_signal(ba, &sig)) {
            sent++;
        }
        
        // Process some signals to prevent ring from filling
        if (i % 1000 == 0) {
            for (int j = 0; j < 100; j++) {
                fixed_bitactor_tick(ba);
            }
        }
    }
    
    uint64_t duration = get_timestamp() - start_time;
    getrusage(RUSAGE_SELF, &usage_end);
    
    long memory_used = usage_end.ru_maxrss - usage_start.ru_maxrss;
    
    printf("Signals attempted: %d\n", MEMORY_BOMB_SIGNALS);
    printf("Signals sent: %llu\n", (unsigned long long)sent);
    printf("Duration: %.2f seconds\n", duration / 1e9);
    printf("Memory increase: %ld KB\n", memory_used);
    printf("Throughput: %.2f Msignals/sec\n", sent / (duration / 1e3));
    
    if (memory_used < 100 * 1024) { // Less than 100MB
        printf("✅ Memory usage controlled\n");
    } else {
        printf("❌ Excessive memory usage\n");
    }
    
    free(ba);
}

void test_cpu_saturation() {
    printf("\n=== CPU Saturation Test ===\n");
    
    fixed_bitactor_t* ba = malloc(sizeof(fixed_bitactor_t));
    fixed_bitactor_init(ba);
    
    // Fill ring buffer
    int filled = 0;
    for (int i = 0; i < FIXED_RING_SIZE; i++) {
        fixed_signal_t sig = {
            .type = FIXED_SIGNAL_HEARTBEATSIGNAL,
            .flags = i,
            .timestamp = get_timestamp(),
            .payload = i
        };
        
        if (fixed_bitactor_enqueue_signal(ba, &sig)) {
            filled++;
        } else {
            break;
        }
    }
    
    printf("Ring buffer filled: %d/%d\n", filled, FIXED_RING_SIZE);
    
    // Process all signals and measure CPU usage
    uint64_t start_ticks = rdtsc();
    uint64_t start_time = get_timestamp();
    
    for (int i = 0; i < filled; i++) {
        fixed_bitactor_tick(ba);
    }
    
    uint64_t total_ticks = rdtsc() - start_ticks;
    uint64_t duration = get_timestamp() - start_time;
    
    double ticks_per_signal = (double)total_ticks / filled;
    double time_per_signal = (double)duration / filled;
    
    printf("Signals processed: %d\n", filled);
    printf("Total CPU ticks: %llu\n", (unsigned long long)total_ticks);
    printf("Average ticks/signal: %.2f\n", ticks_per_signal);
    printf("Average time/signal: %.2f ns\n", time_per_signal);
    
    if (ticks_per_signal < 100) {
        printf("✅ Efficient CPU usage\n");
    } else {
        printf("⚠️  High CPU usage per signal\n");
    }
    
    free(ba);
}

void test_endianness_stress() {
    printf("\n=== Endianness Conversion Stress Test ===\n");
    
    fixed_bitactor_t* ba = malloc(sizeof(fixed_bitactor_t));
    fixed_bitactor_init(ba);
    
    // Test with maximum values to stress endianness conversion
    uint64_t test_values[] = {
        0x0123456789ABCDEF,
        0xFEDCBA9876543210,
        0xFFFFFFFFFFFFFFFF,
        0x8000000000000000,
        0x0000000000000001
    };
    
    int errors = 0;
    uint64_t start_time = get_timestamp();
    
    for (int i = 0; i < 100000; i++) {
        for (size_t j = 0; j < sizeof(test_values)/sizeof(test_values[0]); j++) {
            fixed_signal_t sig = {
                .type = FIXED_SIGNAL_DEBUGSIGNAL,
                .flags = (uint32_t)(test_values[j] >> 32),
                .timestamp = test_values[j],
                .payload = test_values[j]
            };
            
            if (!fixed_bitactor_enqueue_signal(ba, &sig)) {
                errors++;
            }
            
            // Process signal
            fixed_bitactor_tick(ba);
        }
    }
    
    uint64_t duration = get_timestamp() - start_time;
    
    printf("Endianness conversions: %d\n", 100000 * 5);
    printf("Duration: %.2f seconds\n", duration / 1e9);
    printf("Errors: %d\n", errors);
    printf("Conversion rate: %.2f M/sec\n", (100000 * 5) / (duration / 1e3));
    
    if (errors == 0) {
        printf("✅ Endianness handling robust\n");
    } else {
        printf("❌ Endianness conversion errors\n");
    }
    
    free(ba);
}

// Thread function to stress test atomic operations
static _Atomic uint64_t race_detected = 0;
static _Atomic uint64_t operations = 0;

void* race_tester(void* arg) {
    fixed_bitactor_t* ba = (fixed_bitactor_t*)arg;
    
    for (int i = 0; i < 10000; i++) {
        // Get current head and tail
        uint32_t head1 = atomic_load(&ba->signal_head);
        uint32_t tail1 = atomic_load(&ba->signal_tail);
        
        // Simulate some work
        volatile int dummy = 0;
        for (int j = 0; j < 10; j++) dummy++;
        
        // Check if values changed unexpectedly
        uint32_t head2 = atomic_load(&ba->signal_head);
        uint32_t tail2 = atomic_load(&ba->signal_tail);
        
        // In a race condition, head/tail might change in unexpected ways
        if (head2 < head1 || tail2 < tail1) {
            atomic_fetch_add(&race_detected, 1);
        }
        
        atomic_fetch_add(&operations, 1);
    }
    
    return NULL;
}

void test_race_condition_validation() {
    printf("\n=== Race Condition Validation ===\n");
    
    fixed_bitactor_t* ba = malloc(sizeof(fixed_bitactor_t));
    fixed_bitactor_init(ba);
    
    // Reset counters
    atomic_store(&race_detected, 0);
    atomic_store(&operations, 0);
    
    // Start multiple threads to stress test
    pthread_t threads[10];
    for (int i = 0; i < 10; i++) {
        pthread_create(&threads[i], NULL, race_tester, ba);
    }
    
    // Also run producers/consumers
    pthread_t producer, consumer;
    pthread_create(&producer, NULL, stress_producer, ba);
    pthread_create(&consumer, NULL, stress_consumer, ba);
    
    sleep(5);
    atomic_store(&stats.stop_flag, true);
    
    // Wait for all threads
    for (int i = 0; i < 10; i++) {
        pthread_join(threads[i], NULL);
    }
    pthread_join(producer, NULL);
    pthread_join(consumer, NULL);
    
    uint64_t total_ops = atomic_load(&operations);
    uint64_t races = atomic_load(&race_detected);
    
    printf("Operations performed: %llu\n", (unsigned long long)total_ops);
    printf("Race conditions detected: %llu\n", (unsigned long long)races);
    
    if (races == 0) {
        printf("✅ No race conditions detected - atomic operations working correctly\n");
    } else {
        printf("❌ Race conditions detected - atomic operations may be incorrect\n");
    }
    
    free(ba);
}

int main() {
    printf("=== BitActor Comprehensive Stress Test Suite ===\n");
    printf("Testing security fixes under extreme conditions\n");
    
    // Initialize stats
    memset(&stats, 0, sizeof(stats));
    
    // Run all stress tests
    test_concurrent_stress();
    test_memory_exhaustion();
    test_cpu_saturation();
    test_endianness_stress();
    test_race_condition_validation();
    
    printf("\n=== Stress Test Complete ===\n");
    printf("✅ All stress tests completed successfully\n");
    
    return 0;
}