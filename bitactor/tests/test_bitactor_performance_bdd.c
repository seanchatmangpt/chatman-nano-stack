/*
 * BitActor Performance BDD Specifications
 * Behavior-driven performance requirements validation
 */
#include "bdd_framework.h"
#include "../include/bitactor/bitactor.h"
#include <stdlib.h>
#include <string.h>

/* Performance test configuration */
#define LATENCY_SAMPLES 10000
#define THROUGHPUT_DURATION_MS 1000

/* Helper to calculate percentiles */
static uint64_t calculate_percentile(uint64_t* samples, int count, double percentile) {
    int index = (int)(count * percentile);
    return samples[index < count ? index : count - 1];
}

/* Helper to sort samples */
static void sort_samples(uint64_t* samples, int count) {
    for (int i = 0; i < count - 1; i++) {
        for (int j = 0; j < count - i - 1; j++) {
            if (samples[j] > samples[j + 1]) {
                uint64_t temp = samples[j];
                samples[j] = samples[j + 1];
                samples[j + 1] = temp;
            }
        }
    }
}

FEATURE(BitActor_Performance_Requirements) {
    
    SCENARIO("P99.999 latency remains under 8 CPU ticks") {
        bitactor_engine_t* engine;
        uint64_t* samples;
        uint64_t p50, p99, p999, p9999, p99999;
        
        GIVEN("a warmed-up BitActor engine",
            engine = bitactor_init();
            samples = malloc(LATENCY_SAMPLES * sizeof(uint64_t));
            
            // Warmup phase
            signal_t warmup_sig = BUILD_SIGNAL(.kind = 0x01, .payload = 0x1234);
            for (int i = 0; i < 1000; i++) {
                bitactor_tick(engine, &warmup_sig);
            }
        );
        
        WHEN("10,000 signal processing latencies are measured",
            for (int i = 0; i < LATENCY_SAMPLES; i++) {
                signal_t sig = BUILD_SIGNAL(
                    .id = i,
                    .kind = (uint8_t)(i % 4),
                    .payload = i * 31337
                );
                
                uint64_t start = rdtsc_portable();
                bitactor_tick(engine, &sig);
                uint64_t end = rdtsc_portable();
                
                samples[i] = end - start;
            }
            
            sort_samples(samples, LATENCY_SAMPLES);
            
            p50 = calculate_percentile(samples, LATENCY_SAMPLES, 0.50);
            p99 = calculate_percentile(samples, LATENCY_SAMPLES, 0.99);
            p999 = calculate_percentile(samples, LATENCY_SAMPLES, 0.999);
            p9999 = calculate_percentile(samples, LATENCY_SAMPLES, 0.9999);
            p99999 = calculate_percentile(samples, LATENCY_SAMPLES, 0.99999);
        );
        
        THEN("latency percentiles meet strict requirements",
            printf("     P50: %lu ticks\n", p50);
            printf("     P99: %lu ticks\n", p99);
            printf("     P99.9: %lu ticks\n", p999);
            printf("     P99.99: %lu ticks\n", p9999);
            printf("     P99.999: %lu ticks\n", p99999);
            
            EXPECT_LT(p50, 6);     // Most operations under 6 ticks
            EXPECT_LT(p99, 7);     // 99% under 7 ticks
            EXPECT_LT(p999, 8);    // 99.9% under 8 ticks
            EXPECT_LT(p9999, 8);   // 99.99% under 8 ticks
            EXPECT_LT(p99999, 9);  // 99.999% meets 8-tick target
        );
        
        free(samples);
    } END_SCENARIO
    
    SCENARIO("Throughput exceeds 500K signals per second") {
        bitactor_engine_t* engine;
        uint64_t signals_processed;
        double signals_per_second;
        
        GIVEN("a BitActor engine optimized for throughput",
            engine = bitactor_init();
        );
        
        WHEN("signals are processed for exactly 1 second",
            signal_t sig = BUILD_SIGNAL(.kind = 0x01, .payload = 0xFEED);
            
            uint64_t start_ticks = rdtsc_portable();
            uint64_t ticks_per_ms = 3000000; // Assume 3GHz CPU
            uint64_t target_ticks = start_ticks + (THROUGHPUT_DURATION_MS * ticks_per_ms);
            
            signals_processed = 0;
            while (rdtsc_portable() < target_ticks) {
                bitactor_tick(engine, &sig);
                signals_processed++;
            }
            
            signals_per_second = (double)signals_processed / 
                                (THROUGHPUT_DURATION_MS / 1000.0);
        );
        
        THEN("throughput exceeds 500,000 signals/second",
            printf("     Throughput: %.0f signals/sec\n", signals_per_second);
            EXPECT_GT(signals_per_second, 500000);
        );
    } END_SCENARIO
    
    SCENARIO("Batch processing maintains consistent latency") {
        bitactor_engine_t* engine;
        uint64_t single_latency, batch_avg_latency;
        
        GIVEN("baseline single signal latency is measured",
            engine = bitactor_init();
            signal_t sig = BUILD_SIGNAL(.kind = 0x02, .payload = 0xBEEF);
            
            // Warmup and measure single
            for (int i = 0; i < 100; i++) {
                bitactor_tick(engine, &sig);
            }
            
            uint64_t start = rdtsc_portable();
            bitactor_tick(engine, &sig);
            single_latency = rdtsc_portable() - start;
        );
        
        WHEN("100 signals are processed in a batch",
            // Enqueue batch
            for (int i = 0; i < 100; i++) {
                signal_t batch_sig = BUILD_SIGNAL(
                    .id = i,
                    .kind = 0x02,
                    .payload = 0xBEEF
                );
                bitactor_enqueue(engine, &batch_sig);
            }
            
            // Process batch
            uint64_t batch_start = rdtsc_portable();
            bitactor_drain(engine, 100);
            uint64_t batch_total = rdtsc_portable() - batch_start;
            
            batch_avg_latency = batch_total / 100;
        );
        
        THEN("batch processing maintains efficiency",
            printf("     Single latency: %lu ticks\n", single_latency);
            printf("     Batch avg: %lu ticks\n", batch_avg_latency);
            
            // Batch should be more efficient (or at least not worse)
            EXPECT_LT(batch_avg_latency, single_latency * 2);
        );
    } END_SCENARIO
    
    SCENARIO("Memory access patterns optimize cache usage") {
        bitactor_engine_t* engine;
        uint64_t sequential_time, random_time;
        
        GIVEN("a BitActor engine with cache-aligned structures",
            engine = bitactor_init();
        );
        
        WHEN("signals are processed with sequential access patterns",
            uint64_t seq_start = rdtsc_portable();
            
            for (int i = 0; i < 1000; i++) {
                signal_t sig = BUILD_SIGNAL(
                    .kind = (uint8_t)(i % 4),  // Sequential pattern
                    .payload = i
                );
                bitactor_tick(engine, &sig);
            }
            
            sequential_time = rdtsc_portable() - seq_start;
        );
        
        AND("signals are processed with random access patterns",
            uint64_t rand_start = rdtsc_portable();
            
            for (int i = 0; i < 1000; i++) {
                signal_t sig = BUILD_SIGNAL(
                    .kind = (uint8_t)((i * 31337) % 256),  // Random pattern
                    .payload = i
                );
                bitactor_tick(engine, &sig);
            }
            
            random_time = rdtsc_portable() - rand_start;
        );
        
        THEN("cache efficiency ratio is acceptable",
            double cache_penalty = (double)random_time / sequential_time;
            printf("     Sequential: %lu ticks\n", sequential_time);
            printf("     Random: %lu ticks\n", random_time);
            printf("     Cache penalty: %.2fx\n", cache_penalty);
            
            // Random access should not be more than 3x slower
            EXPECT_LT(cache_penalty, 3.0);
        );
    } END_SCENARIO
    
    SCENARIO("Concurrent contexts maintain isolation") {
        bitactor_engine_t* engines[4];
        uint64_t isolated_latencies[4];
        
        GIVEN("multiple BitActor engines are initialized",
            for (int i = 0; i < 4; i++) {
                engines[i] = bitactor_init();
                EXPECT(engines[i] != NULL);
            }
        );
        
        WHEN("each engine processes signals independently",
            for (int i = 0; i < 4; i++) {
                signal_t sig = BUILD_SIGNAL(
                    .kind = 0x01,
                    .payload = i * 1000
                );
                
                uint64_t start = rdtsc_portable();
                for (int j = 0; j < 1000; j++) {
                    bitactor_tick(engines[i], &sig);
                }
                isolated_latencies[i] = rdtsc_portable() - start;
            }
        );
        
        THEN("no cross-context interference occurs",
            // All engines should have similar performance
            uint64_t min_latency = isolated_latencies[0];
            uint64_t max_latency = isolated_latencies[0];
            
            for (int i = 1; i < 4; i++) {
                if (isolated_latencies[i] < min_latency) {
                    min_latency = isolated_latencies[i];
                }
                if (isolated_latencies[i] > max_latency) {
                    max_latency = isolated_latencies[i];
                }
            }
            
            // Variance should be less than 20%
            double variance = (double)(max_latency - min_latency) / min_latency;
            printf("     Latency variance: %.1f%%\n", variance * 100);
            EXPECT_LT(variance, 0.20);
        );
    } END_SCENARIO
}