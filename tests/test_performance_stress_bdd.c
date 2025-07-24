/*
 * Performance Stress Test
 * EXTREME PERFORMANCE testing - 8-tick constraint enforcement under pressure
 * Boundary violations, latency spikes, timing regression analysis
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../bitactor/include/bitactor_public.h"
#include "../bitactor/src/bitactor.h"
#include "../bitactor/src/bitfiber.h"
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <pthread.h>
#include <stdatomic.h>

// Performance stress configuration
#define PERFORMANCE_STRESS_ITERATIONS 100000
#define TIMING_PRECISION_TESTS 50000
#define LATENCY_SPIKE_THRESHOLD 12 // ticks
#define BOUNDARY_VIOLATION_TESTS 10000
#define REGRESSION_ANALYSIS_SAMPLES 1000
#define SUSTAINED_LOAD_DURATION_SEC 10

// Hardware cycle measurement precision
#define RDTSC_OVERHEAD_CALIBRATION_CYCLES 1000

// Performance metrics structure
typedef struct {
    uint64_t total_operations;
    uint64_t tick_violations;
    uint64_t latency_spikes;
    uint64_t boundary_violations;
    uint64_t min_execution_time;
    uint64_t max_execution_time;
    uint64_t total_execution_time;
    uint64_t measurement_overhead;
    double average_execution_time;
    double standard_deviation;
    double percentile_95;
    double percentile_99;
    double violation_rate;
    bool timing_consistency_maintained;
} performance_metrics_t;

// Global performance tracking
static performance_metrics_t g_perf_metrics = {0};
static uint64_t g_execution_times[PERFORMANCE_STRESS_ITERATIONS];
static atomic_bool g_performance_test_active = false;

// Calibrate RDTSC overhead
static uint64_t calibrate_rdtsc_overhead(void) {
    uint64_t total_overhead = 0;
    
    for (int i = 0; i < RDTSC_OVERHEAD_CALIBRATION_CYCLES; i++) {
        uint64_t start = rdtsc_portable();
        uint64_t end = rdtsc_portable();
        total_overhead += (end - start);
    }
    
    return total_overhead / RDTSC_OVERHEAD_CALIBRATION_CYCLES;
}

// Statistical analysis functions
static void calculate_statistics(uint64_t* times, uint32_t count, performance_metrics_t* metrics) {
    if (count == 0) return;
    
    // Sort times for percentile calculation
    for (uint32_t i = 0; i < count - 1; i++) {
        for (uint32_t j = i + 1; j < count; j++) {
            if (times[i] > times[j]) {
                uint64_t temp = times[i];
                times[i] = times[j];
                times[j] = temp;
            }
        }
    }
    
    metrics->min_execution_time = times[0];
    metrics->max_execution_time = times[count - 1];
    
    // Calculate average
    uint64_t sum = 0;
    for (uint32_t i = 0; i < count; i++) {
        sum += times[i];
    }
    metrics->average_execution_time = (double)sum / count;
    
    // Calculate standard deviation
    double variance_sum = 0;
    for (uint32_t i = 0; i < count; i++) {
        double diff = times[i] - metrics->average_execution_time;
        variance_sum += diff * diff;
    }
    metrics->standard_deviation = sqrt(variance_sum / count);
    
    // Calculate percentiles
    uint32_t p95_index = (uint32_t)(count * 0.95);
    uint32_t p99_index = (uint32_t)(count * 0.99);
    metrics->percentile_95 = times[p95_index];
    metrics->percentile_99 = times[p99_index];
}

// Performance stress handlers
static result_t ultra_fast_handler(signal_t* signal, void* scratch) {
    result_t result = {0};
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    
    // Minimal processing - just bit manipulation
    result.result = signal->payload ^ 0xDEADBEEF;
    return result;
}

static result_t timing_critical_handler(signal_t* signal, void* scratch) {
    result_t result = {0};
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    
    // Slightly more work but still under 8 ticks
    uint64_t* data = (uint64_t*)scratch;
    data[0] = signal->payload;
    data[1] = data[0] << 1;
    data[2] = data[1] ^ data[0];
    
    result.result = data[2];
    return result;
}

static result_t boundary_test_handler(signal_t* signal, void* scratch) {
    result_t result = {0};
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    
    // Designed to be close to 8-tick boundary
    volatile uint64_t* data = (uint64_t*)scratch;
    for (int i = 0; i < 7; i++) {  // Calibrated to approach 8 ticks
        data[i] = signal->payload + i;
        data[i] = data[i] * 13 + 7;  // Some computation
    }
    
    result.result = data[6];
    return result;
}

// Thread worker for sustained performance testing
static void* performance_stress_worker(void* arg) {
    bitactor_engine* engine = (bitactor_engine*)arg;
    uint64_t thread_operations = 0;
    
    while (atomic_load(&g_performance_test_active)) {
        signal_t signal = {
            .id = (uint32_t)(thread_operations + pthread_self()),
            .kind = SIGNAL_KIND_DATA,
            .priority = 255,  // Maximum priority
            .flags = 0x1000,  // Performance test flag
            .payload = thread_operations,
            .timestamp = rdtsc_portable(),
            .context = pthread_self()
        };
        
        uint64_t start = rdtsc_portable();
        result_t result = bitactor_tick(engine, &signal);
        uint64_t end = rdtsc_portable();
        
        uint64_t execution_time = end - start - g_perf_metrics.measurement_overhead;
        
        if (result.ticks > 8) {
            atomic_fetch_add((atomic_uint_fast64_t*)&g_perf_metrics.tick_violations, 1);
        }
        
        if (execution_time > LATENCY_SPIKE_THRESHOLD) {
            atomic_fetch_add((atomic_uint_fast64_t*)&g_perf_metrics.latency_spikes, 1);
        }
        
        thread_operations++;
        atomic_fetch_add((atomic_uint_fast64_t*)&g_perf_metrics.total_operations, 1);
    }
    
    return NULL;
}

FEATURE(Performance_Stress_Testing) {
    
    SCENARIO("RDTSC measurement overhead calibration") {
        uint64_t overhead;
        
        GIVEN("system requiring precise timing measurement",
            overhead = 0;
        );
        
        WHEN("calibrating RDTSC measurement overhead",
            overhead = calibrate_rdtsc_overhead();
            g_perf_metrics.measurement_overhead = overhead;
        );
        
        THEN("measurement overhead should be minimal and consistent",
            printf("       📏 RDTSC MEASUREMENT CALIBRATION 📏\\n");
            printf("       Measurement overhead: %llu cycles\\n", (unsigned long long)overhead);
            printf("       Overhead per measurement: %.2f cycles\\n", (double)overhead);
            
            // Overhead should be very small (typically 1-3 cycles)
            EXPECT_LE(overhead, 10);
            EXPECT_GT(overhead, 0);
        );
    } END_SCENARIO
    
    SCENARIO("8-tick boundary violation stress test") {
        bitactor_engine* engine = bitactor_init();
        uint64_t violation_times[BOUNDARY_VIOLATION_TESTS];
        uint32_t violation_count = 0;
        
        GIVEN("engine with boundary test handler",
            EXPECT_NE(engine, NULL);
            
            // Register boundary test handler
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x2000, 
                            boundary_test_handler);
        );
        
        WHEN("executing signals designed to test 8-tick boundary",
            for (int i = 0; i < BOUNDARY_VIOLATION_TESTS; i++) {
                signal_t signal = {
                    .id = 100000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 255,
                    .flags = 0x2000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                uint64_t start = rdtsc_portable();
                result_t result = bitactor_tick(engine, &signal);
                uint64_t end = rdtsc_portable();
                
                uint64_t execution_time = end - start - g_perf_metrics.measurement_overhead;
                
                if (result.ticks > 8 || execution_time > 8) {
                    violation_times[violation_count] = execution_time;
                    violation_count++;
                    g_perf_metrics.boundary_violations++;
                }
                
                EXPECT_EQ(result.status, BITACTOR_OK);
            }
            
            g_perf_metrics.violation_rate = (double)violation_count / BOUNDARY_VIOLATION_TESTS;
        );
        
        THEN("8-tick boundary violations should be minimal",
            printf("       ⚖️ 8-TICK BOUNDARY VIOLATION STRESS ⚖️\\n");
            printf("       Boundary tests: %d\\n", BOUNDARY_VIOLATION_TESTS);
            printf("       Boundary violations: %u\\n", violation_count);
            printf("       Violation rate: %.4f%% \\n", g_perf_metrics.violation_rate * 100);
            
            if (violation_count > 0) {
                printf("       Violation times (cycles): ");
                for (uint32_t i = 0; i < min(violation_count, 10); i++) {
                    printf("%llu ", (unsigned long long)violation_times[i]);
                }
                printf("\\n");
            }
            
            // Should have very low violation rate
            EXPECT_LT(g_perf_metrics.violation_rate, 0.01); // Less than 1%
            
            // Total violations should be minimal
            EXPECT_LT(violation_count, BOUNDARY_VIOLATION_TESTS / 100);
        );
    } END_SCENARIO
    
    SCENARIO("Latency spike detection under load") {
        bitactor_engine* engine = bitactor_init();
        uint64_t spike_times[1000];
        uint32_t spike_count = 0;
        
        GIVEN("engine with ultra-fast handler for spike detection",
            // Register ultra-fast handler
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x3000, 
                            ultra_fast_handler);
        );
        
        WHEN("executing operations to detect latency spikes",
            for (int i = 0; i < TIMING_PRECISION_TESTS; i++) {
                signal_t signal = {
                    .id = 200000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 255,
                    .flags = 0x3000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                uint64_t start = rdtsc_portable();
                result_t result = bitactor_tick(engine, &signal);
                uint64_t end = rdtsc_portable();
                
                uint64_t execution_time = end - start - g_perf_metrics.measurement_overhead;
                
                if (execution_time > LATENCY_SPIKE_THRESHOLD) {
                    if (spike_count < 1000) {
                        spike_times[spike_count] = execution_time;
                        spike_count++;
                    }
                    g_perf_metrics.latency_spikes++;
                }
                
                if (i < PERFORMANCE_STRESS_ITERATIONS) {
                    g_execution_times[i] = execution_time;
                }
            }
        );
        
        THEN("latency spikes should be rare and quantified",
            printf("       🌊 LATENCY SPIKE DETECTION 🌊\\n");
            printf("       Timing tests: %d\\n", TIMING_PRECISION_TESTS);
            printf("       Latency spikes: %llu\\n", (unsigned long long)g_perf_metrics.latency_spikes);
            printf("       Spike threshold: %d cycles\\n", LATENCY_SPIKE_THRESHOLD);
            
            double spike_rate = (double)g_perf_metrics.latency_spikes / TIMING_PRECISION_TESTS;
            printf("       Spike rate: %.4f%%\\n", spike_rate * 100);
            
            if (spike_count > 0) {
                printf("       Sample spike times: ");
                for (uint32_t i = 0; i < min(spike_count, 10); i++) {
                    printf("%llu ", (unsigned long long)spike_times[i]);
                }
                printf("\\n");
            }
            
            // Should have very low spike rate
            EXPECT_LT(spike_rate, 0.05); // Less than 5%
            
            // Individual spikes should not be excessive
            for (uint32_t i = 0; i < min(spike_count, 100); i++) {
                EXPECT_LT(spike_times[i], 100); // Should not exceed 100 cycles
            }
        );
    } END_SCENARIO
    
    SCENARIO("Statistical timing analysis stress test") {
        bitactor_engine* engine = bitactor_init();
        performance_metrics_t stats;
        
        GIVEN("engine with timing-critical handler",
            // Register timing critical handler
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x4000, 
                            timing_critical_handler);
            
            memset(&stats, 0, sizeof(stats));
        );
        
        WHEN("collecting statistical timing data",
            for (int i = 0; i < REGRESSION_ANALYSIS_SAMPLES; i++) {
                signal_t signal = {
                    .id = 300000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 255,
                    .flags = 0x4000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                uint64_t start = rdtsc_portable();
                result_t result = bitactor_tick(engine, &signal);
                uint64_t end = rdtsc_portable();
                
                uint64_t execution_time = end - start - g_perf_metrics.measurement_overhead;
                g_execution_times[i] = execution_time;
                
                stats.total_execution_time += execution_time;
                stats.total_operations++;
                
                if (result.ticks > 8) {
                    stats.tick_violations++;
                }
            }
            
            calculate_statistics(g_execution_times, REGRESSION_ANALYSIS_SAMPLES, &stats);
        );
        
        THEN("statistical distribution should show consistent performance",
            printf("       📊 STATISTICAL TIMING ANALYSIS 📊\\n");
            printf("       Samples: %llu\\n", (unsigned long long)stats.total_operations);
            printf("       Min: %llu cycles\\n", (unsigned long long)stats.min_execution_time);
            printf("       Max: %llu cycles\\n", (unsigned long long)stats.max_execution_time);
            printf("       Average: %.2f cycles\\n", stats.average_execution_time);
            printf("       Std Dev: %.2f cycles\\n", stats.standard_deviation);
            printf("       95th percentile: %.0f cycles\\n", stats.percentile_95);
            printf("       99th percentile: %.0f cycles\\n", stats.percentile_99);
            printf("       Violations: %llu (%.4f%%)\\n", 
                   (unsigned long long)stats.tick_violations,
                   (double)stats.tick_violations / stats.total_operations * 100);
            
            // Performance consistency checks
            EXPECT_LE(stats.average_execution_time, 8.0);
            EXPECT_LE(stats.percentile_95, 12.0);  // 95% should be close to 8 ticks
            EXPECT_LE(stats.percentile_99, 16.0);  // 99% should be reasonable
            EXPECT_LT(stats.standard_deviation, 5.0);  // Low variance
            
            // Violation rate should be minimal
            double violation_rate = (double)stats.tick_violations / stats.total_operations;
            EXPECT_LT(violation_rate, 0.02);  // Less than 2%
        );
    } END_SCENARIO
    
    SCENARIO("Performance regression detection stress test") {
        bitactor_engine* engine = bitactor_init();
        uint64_t baseline_times[500];
        uint64_t loaded_times[500];
        
        GIVEN("baseline performance measurement",
            // Register ultra-fast handler for baseline
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x5000, 
                            ultra_fast_handler);
            
            // Measure baseline performance
            for (int i = 0; i < 500; i++) {
                signal_t signal = {
                    .id = 400000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 255,
                    .flags = 0x5000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                uint64_t start = rdtsc_portable();
                bitactor_tick(engine, &signal);
                uint64_t end = rdtsc_portable();
                
                baseline_times[i] = end - start - g_perf_metrics.measurement_overhead;
            }
        );
        
        WHEN("measuring performance under system load",
            // Create system load (additional fibers, signals)
            for (int load = 0; load < 100; load++) {
                fiber_create(engine->fiber_sched, NULL, NULL);
            }
            
            // Add background signals to create load
            for (int bg = 0; bg < 1000; bg++) {
                signal_t bg_signal = {
                    .id = 500000 + bg,
                    .kind = SIGNAL_KIND_CONTROL,
                    .priority = 128,
                    .flags = 0,
                    .payload = bg,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                bitactor_enqueue(engine, bg_signal);
            }
            
            // Measure loaded performance
            for (int i = 0; i < 500; i++) {
                signal_t signal = {
                    .id = 600000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 255,
                    .flags = 0x5000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                uint64_t start = rdtsc_portable();
                bitactor_tick(engine, &signal);
                uint64_t end = rdtsc_portable();
                
                loaded_times[i] = end - start - g_perf_metrics.measurement_overhead;
            }
        );
        
        THEN("performance degradation should be within acceptable limits",
            // Calculate baseline statistics
            performance_metrics_t baseline_stats = {0};
            calculate_statistics(baseline_times, 500, &baseline_stats);
            
            // Calculate loaded statistics
            performance_metrics_t loaded_stats = {0};
            calculate_statistics(loaded_times, 500, &loaded_stats);
            
            double performance_degradation = 
                (loaded_stats.average_execution_time - baseline_stats.average_execution_time) / 
                baseline_stats.average_execution_time * 100;
            
            printf("       📈 PERFORMANCE REGRESSION ANALYSIS 📈\\n");
            printf("       Baseline avg: %.2f cycles\\n", baseline_stats.average_execution_time);
            printf("       Loaded avg: %.2f cycles\\n", loaded_stats.average_execution_time);
            printf("       Performance degradation: %.2f%%\\n", performance_degradation);
            printf("       Baseline 95th: %.0f cycles\\n", baseline_stats.percentile_95);
            printf("       Loaded 95th: %.0f cycles\\n", loaded_stats.percentile_95);
            
            // Performance degradation should be minimal
            EXPECT_LT(performance_degradation, 50.0);  // Less than 50% degradation
            
            // Even under load, should maintain reasonable performance
            EXPECT_LE(loaded_stats.average_execution_time, 16.0);  // Should still be reasonable
            EXPECT_LE(loaded_stats.percentile_95, 24.0);
        );
    } END_SCENARIO
    
    SCENARIO("Sustained high-performance load stress test") {
        bitactor_engine* engine = bitactor_init();
        pthread_t performance_threads[4];
        struct timeval start_time, end_time;
        
        GIVEN("engine configured for sustained performance testing",
            atomic_store(&g_performance_test_active, true);
            atomic_store((atomic_uint_fast64_t*)&g_perf_metrics.total_operations, 0);
            atomic_store((atomic_uint_fast64_t*)&g_perf_metrics.tick_violations, 0);
            atomic_store((atomic_uint_fast64_t*)&g_perf_metrics.latency_spikes, 0);
            
            // Register performance handler
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x1000, 
                            timing_critical_handler);
        );
        
        WHEN("running sustained high-performance load",
            gettimeofday(&start_time, NULL);
            
            // Launch performance stress threads
            for (int i = 0; i < 4; i++) {
                int result = pthread_create(&performance_threads[i], NULL, 
                                          performance_stress_worker, engine);
                EXPECT_EQ(result, 0);
            }
            
            // Run for sustained duration
            sleep(SUSTAINED_LOAD_DURATION_SEC);
            
            // Stop performance testing
            atomic_store(&g_performance_test_active, false);
            
            // Wait for threads to complete
            for (int i = 0; i < 4; i++) {
                pthread_join(performance_threads[i], NULL);
            }
            
            gettimeofday(&end_time, NULL);
        );
        
        THEN("sustained performance should meet requirements",
            double elapsed = (end_time.tv_sec - start_time.tv_sec) + 
                           (end_time.tv_usec - start_time.tv_usec) / 1000000.0;
            
            uint64_t total_ops = atomic_load((atomic_uint_fast64_t*)&g_perf_metrics.total_operations);
            uint64_t violations = atomic_load((atomic_uint_fast64_t*)&g_perf_metrics.tick_violations);
            uint64_t spikes = atomic_load((atomic_uint_fast64_t*)&g_perf_metrics.latency_spikes);
            
            double ops_per_second = total_ops / elapsed;
            double violation_rate = (double)violations / total_ops;
            double spike_rate = (double)spikes / total_ops;
            
            printf("       🚀 SUSTAINED HIGH-PERFORMANCE LOAD 🚀\\n");
            printf("       Duration: %.2f seconds\\n", elapsed);
            printf("       Total operations: %llu\\n", (unsigned long long)total_ops);
            printf("       Operations/second: %.0f\\n", ops_per_second);
            printf("       Tick violations: %llu (%.4f%%)\\n", 
                   (unsigned long long)violations, violation_rate * 100);
            printf("       Latency spikes: %llu (%.4f%%)\\n", 
                   (unsigned long long)spikes, spike_rate * 100);
            
            // Should maintain high throughput
            EXPECT_GT(ops_per_second, 100000.0);  // At least 100K ops/sec
            
            // Should maintain low violation rates
            EXPECT_LT(violation_rate, 0.05);  // Less than 5% violations
            EXPECT_LT(spike_rate, 0.10);      // Less than 10% spikes
            
            // Should process significant number of operations
            EXPECT_GT(total_ops, 1000000);    // At least 1M operations
        );
    } END_SCENARIO
    
    SCENARIO("Timing consistency validation under stress") {
        bitactor_engine* engine = bitactor_init();
        uint64_t consistency_times[10000];
        uint32_t consistency_violations = 0;
        
        GIVEN("engine for timing consistency validation",
            // Register ultra-fast handler for consistency testing
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x6000, 
                            ultra_fast_handler);
        );
        
        WHEN("measuring timing consistency under varied load",
            for (int i = 0; i < 10000; i++) {
                // Vary the system load periodically
                if (i % 1000 == 0) {
                    // Add some background load every 1000 operations
                    for (int bg = 0; bg < 100; bg++) {
                        signal_t bg_signal = {
                            .id = 700000 + bg,
                            .kind = SIGNAL_KIND_CONTROL,
                            .priority = 64,
                            .flags = 0,
                            .payload = bg,
                            .timestamp = rdtsc_portable(),
                            .context = 0
                        };
                        bitactor_enqueue(engine, bg_signal);
                    }
                }
                
                signal_t signal = {
                    .id = 800000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 255,
                    .flags = 0x6000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                uint64_t start = rdtsc_portable();
                result_t result = bitactor_tick(engine, &signal);
                uint64_t end = rdtsc_portable();
                
                uint64_t execution_time = end - start - g_perf_metrics.measurement_overhead;
                consistency_times[i] = execution_time;
                
                // Check for consistency violations (large timing variations)
                if (i > 0) {
                    uint64_t prev_time = consistency_times[i-1];
                    uint64_t time_diff = execution_time > prev_time ? 
                                       execution_time - prev_time : 
                                       prev_time - execution_time;
                    
                    if (time_diff > 10) {  // Large variation threshold
                        consistency_violations++;
                    }
                }
                
                EXPECT_EQ(result.status, BITACTOR_OK);
            }
        );
        
        THEN("timing should remain consistent despite load variations",
            performance_metrics_t consistency_stats = {0};
            calculate_statistics(consistency_times, 10000, &consistency_stats);
            
            double consistency_rate = (double)consistency_violations / 10000;
            
            printf("       ⏱️ TIMING CONSISTENCY VALIDATION ⏱️\\n");
            printf("       Consistency tests: 10000\\n");
            printf("       Timing variations: %u\\n", consistency_violations);
            printf("       Consistency rate: %.4f%%\\n", consistency_rate * 100);
            printf("       Average: %.2f cycles\\n", consistency_stats.average_execution_time);
            printf("       Std Dev: %.2f cycles\\n", consistency_stats.standard_deviation);
            printf("       Min-Max range: %llu - %llu cycles\\n",
                   (unsigned long long)consistency_stats.min_execution_time,
                   (unsigned long long)consistency_stats.max_execution_time);
            
            // Should maintain timing consistency
            EXPECT_LT(consistency_rate, 0.20);  // Less than 20% large variations
            EXPECT_LT(consistency_stats.standard_deviation, 8.0);  // Low variance
            
            // Should still maintain performance bounds
            EXPECT_LE(consistency_stats.average_execution_time, 10.0);
            EXPECT_LE(consistency_stats.percentile_99, 20.0);
        );
    } END_SCENARIO
}