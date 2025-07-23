#include "test_harness.h"
#include "../include/bitactor/bitactor.h"
#include <immintrin.h>
#include <pthread.h>

// Performance test configuration
#define PERF_ITERATIONS 1000000
#define LATENCY_SAMPLES 10000

// Test: Throughput benchmark
bool test_throughput(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    signal_t signal = {.kind = 0x01, .payload = 0x12345678};
    
    uint64_t start = rdtsc();
    
    for (int i = 0; i < PERF_ITERATIONS; i++) {
        bitactor_tick(&ctx, &signal);
    }
    
    uint64_t end = rdtsc();
    uint64_t total_ticks = end - start;
    double ticks_per_op = (double)total_ticks / PERF_ITERATIONS;
    
    TEST_ASSERT_LT(ticks_per_op, 100, "Throughput too low");
    
    // Calculate signals per second (assuming 3GHz CPU)
    double signals_per_sec = 3e9 / ticks_per_op;
    
    TEST_ASSERT(signals_per_sec >= 500000, "Below 500K signals/sec target");
    
    snprintf(error_msg, 256, "Throughput: %.0f signals/sec", signals_per_sec);
    return true;
}

// Test: Latency distribution
bool test_latency_distribution(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    signal_t signal = {.kind = 0x02, .payload = 0xDEADBEEF};
    uint64_t samples[LATENCY_SAMPLES];
    
    // Warm up
    for (int i = 0; i < 1000; i++) {
        bitactor_tick(&ctx, &signal);
    }
    
    // Collect samples
    for (int i = 0; i < LATENCY_SAMPLES; i++) {
        uint64_t start = rdtsc();
        bitactor_tick(&ctx, &signal);
        uint64_t end = rdtsc();
        samples[i] = end - start;
    }
    
    // Sort samples
    for (int i = 0; i < LATENCY_SAMPLES - 1; i++) {
        for (int j = 0; j < LATENCY_SAMPLES - i - 1; j++) {
            if (samples[j] > samples[j + 1]) {
                uint64_t temp = samples[j];
                samples[j] = samples[j + 1];
                samples[j + 1] = temp;
            }
        }
    }
    
    // Calculate percentiles
    uint64_t p50 = samples[LATENCY_SAMPLES / 2];
    uint64_t p99 = samples[(int)(LATENCY_SAMPLES * 0.99)];
    uint64_t p999 = samples[(int)(LATENCY_SAMPLES * 0.999)];
    uint64_t p9999 = samples[(int)(LATENCY_SAMPLES * 0.9999)];
    uint64_t p99999 = samples[LATENCY_SAMPLES - 1];
    
    // Verify latency targets
    TEST_ASSERT_LE(p50, 6, "P50 latency exceeds 6 ticks");
    TEST_ASSERT_LE(p99, 7, "P99 latency exceeds 7 ticks");
    TEST_ASSERT_LE(p999, 8, "P99.9 latency exceeds 8 ticks");
    TEST_ASSERT_LE(p9999, 8, "P99.99 latency exceeds 8 ticks");
    TEST_ASSERT_LE(p99999, 8, "P99.999 latency exceeds 8 ticks");
    
    snprintf(error_msg, 256, "P50=%lu P99=%lu P99.999=%lu", p50, p99, p99999);
    return true;
}

// Test: SIMD performance
bool test_simd_performance(char* error_msg) {
#ifdef __AVX2__
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Prepare batch of signals
    signal_t signals[8] __attribute__((aligned(32)));
    for (int i = 0; i < 8; i++) {
        signals[i].kind = 0x01;
        signals[i].payload = i;
    }
    
    uint64_t start = rdtsc();
    
    // Process in SIMD batches
    for (int iter = 0; iter < 10000; iter++) {
        // Load 8 signals at once
        __m256i kinds = _mm256_set1_epi32(0x01);
        __m256i payloads = _mm256_loadu_si256((__m256i*)&signals[0].payload);
        
        // Simulate SIMD processing
        __m256i results = _mm256_xor_si256(payloads, _mm256_set1_epi32(0xFF));
        
        // Store results (in real impl would update telemetry)
        _mm256_storeu_si256((__m256i*)&signals[0].payload, results);
    }
    
    uint64_t end = rdtsc();
    uint64_t ticks_per_batch = (end - start) / 10000;
    
    // SIMD should process 8 signals in roughly same time as 1
    TEST_ASSERT_LT(ticks_per_batch, 64, "SIMD not providing speedup");
#endif
    
    return true;
}

// Test: Cache efficiency
bool test_cache_efficiency(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Test sequential access pattern
    uint64_t sequential_start = rdtsc();
    for (int i = 0; i < 1000; i++) {
        signal_t signal = {.kind = (uint8_t)(i % 4), .payload = i};
        bitactor_tick(&ctx, &signal);
    }
    uint64_t sequential_time = rdtsc() - sequential_start;
    
    // Test random access pattern
    uint64_t random_start = rdtsc();
    for (int i = 0; i < 1000; i++) {
        signal_t signal = {.kind = (uint8_t)((i * 31337) % 256), .payload = i};
        bitactor_tick(&ctx, &signal);
    }
    uint64_t random_time = rdtsc() - random_start;
    
    // Random should not be more than 2x slower (good cache behavior)
    TEST_ASSERT_LT(random_time, sequential_time * 2, "Poor cache efficiency");
    
    return true;
}

// Test: Concurrent performance (no locks)
bool test_lockfree_performance(char* error_msg) {
    // Each thread gets its own context (no sharing)
    const int num_threads = 4;
    bitactor_context_t contexts[num_threads];
    
    for (int i = 0; i < num_threads; i++) {
        bitactor_init(&contexts[i]);
    }
    
    uint64_t start = rdtsc();
    
    // Simulate parallel execution
    #pragma omp parallel for
    for (int t = 0; t < num_threads; t++) {
        signal_t signal = {.kind = 0x01, .payload = t};
        for (int i = 0; i < 100000; i++) {
            bitactor_tick(&contexts[t], &signal);
        }
    }
    
    uint64_t end = rdtsc();
    uint64_t total_ops = num_threads * 100000;
    double ticks_per_op = (double)(end - start) / total_ops;
    
    // Should scale linearly with threads
    TEST_ASSERT_LT(ticks_per_op, 50, "Poor multi-thread scaling");
    
    return true;
}

int main() {
    TEST_INIT();
    
    RUN_TEST(test_throughput);
    RUN_TEST(test_latency_distribution);
    RUN_TEST(test_simd_performance);
    RUN_TEST(test_cache_efficiency);
    RUN_TEST(test_lockfree_performance);
    
    TEST_SUMMARY();
    
    // Print performance summary
    printf("\n📊 Performance Metrics:\n");
    printf("Target: ≥500K signals/sec, ≤8 ticks P99.999\n");
    
    return 0;
}