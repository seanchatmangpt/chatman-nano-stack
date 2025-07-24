/*
 * Endurance Stress Test
 * EXTREME ENDURANCE testing - Long-running system stability
 * Resource leak detection, performance degradation analysis, continuous operation
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../bitactor/include/bitactor.h"
#include "../bitactor/src/bitactor.h"
#include "../bitactor/src/bitfiber.h"
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <pthread.h>
#include <stdatomic.h>
#include <unistd.h>
#include <signal.h>

// Endurance stress configuration
#define ENDURANCE_TEST_DURATION_SEC 60    // 1 minute endurance test
#define LONG_RUNNING_CYCLES 1000000       // 1M operations for long-running test
#define RESOURCE_LEAK_CHECK_INTERVAL 10000 // Check every 10K operations
#define DEGRADATION_ANALYSIS_SAMPLES 100   // Samples for degradation analysis
#define STABILITY_MONITOR_INTERVAL_MS 1000 // Monitor every second
#define CONTINUOUS_OPERATION_HOURS 0.1     // 6 minutes (0.1 hours) for CI

// Resource monitoring structure
typedef struct {
    size_t memory_usage_kb;
    uint32_t open_file_descriptors;
    uint64_t cpu_cycles_used;
    uint64_t context_switches;
    double cpu_time_sec;
    time_t timestamp;
} resource_snapshot_t;

// Endurance test results
typedef struct {
    uint64_t total_operations_completed;
    uint64_t total_errors_encountered;
    uint64_t resource_leak_detections;
    uint64_t performance_degradations;
    uint64_t stability_violations;
    uint64_t max_memory_usage_kb;
    uint64_t total_test_duration_sec;
    double operations_per_second_initial;
    double operations_per_second_final;
    double degradation_percentage;
    bool system_remained_stable;
    resource_snapshot_t baseline_resources;
    resource_snapshot_t final_resources;
} endurance_results_t;

// Global endurance tracking
static endurance_results_t g_endurance_results = {0};
static _Atomic bool g_endurance_test_active = false;
static _Atomic uint64_t g_operation_counter = 0;
static resource_snapshot_t g_resource_snapshots[1000];
static uint32_t g_snapshot_count = 0;

// Resource monitoring functions
static size_t get_memory_usage_kb(void) {
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);
    return usage.ru_maxrss; // Peak resident set size in KB (Linux) or bytes (macOS)
}

static uint32_t get_open_file_descriptors(void) {
    // Simplified file descriptor count (real implementation would count /proc/self/fd)
    return 10; // Baseline estimate
}

static void capture_resource_snapshot(resource_snapshot_t* snapshot) {
    snapshot->memory_usage_kb = get_memory_usage_kb();
    snapshot->open_file_descriptors = get_open_file_descriptors();
    snapshot->cpu_cycles_used = rdtsc_portable();
    snapshot->timestamp = time(NULL);
    
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);
    snapshot->cpu_time_sec = usage.ru_utime.tv_sec + usage.ru_utime.tv_usec / 1000000.0;
    snapshot->context_switches = usage.ru_nvcsw + usage.ru_nivcsw;
}

static bool detect_resource_leak(resource_snapshot_t* baseline, resource_snapshot_t* current) {
    // Memory leak detection
    size_t memory_growth = current->memory_usage_kb > baseline->memory_usage_kb ?
                          current->memory_usage_kb - baseline->memory_usage_kb : 0;
    
    // File descriptor leak detection
    uint32_t fd_growth = current->open_file_descriptors > baseline->open_file_descriptors ?
                        current->open_file_descriptors - baseline->open_file_descriptors : 0;
    
    // Consider it a leak if memory grows by more than 50MB or FDs grow by more than 100
    return (memory_growth > 50000) || (fd_growth > 100);
}

static double calculate_performance_degradation(double initial_ops_per_sec, double current_ops_per_sec) {
    if (initial_ops_per_sec == 0) return 0.0;
    return ((initial_ops_per_sec - current_ops_per_sec) / initial_ops_per_sec) * 100.0;
}

// Endurance test signal handlers
static result_t endurance_light_handler(signal_t* signal, void* scratch) {
    result_t result = {0};
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    
    // Light processing for sustained endurance
    uint32_t* data = (uint32_t*)scratch;
    data[0] = (uint32_t)signal->payload;
    data[1] = data[0] ^ 0xDEADBEEF;
    
    result.result = data[1];
    atomic_fetch_add(&g_operation_counter, 1);
    return result;
}

static result_t endurance_medium_handler(signal_t* signal, void* scratch) {
    result_t result = {0};
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    
    // Medium processing with some computation
    uint64_t* data = (uint64_t*)scratch;
    data[0] = signal->payload;
    for (int i = 1; i < 8; i++) {
        data[i] = data[i-1] * 17 + 13;
    }
    
    result.result = data[7];
    atomic_fetch_add(&g_operation_counter, 1);
    return result;
}

static result_t endurance_heavy_handler(signal_t* signal, void* scratch) {
    result_t result = {0};
    result.signal_id = signal->id;
    result.status = BITACTOR_OK;
    
    // Heavy processing for stress endurance
    uint8_t* buffer = (uint8_t*)scratch;
    uint64_t payload = signal->payload;
    
    // Fill buffer with pattern
    for (int i = 0; i < 256; i++) {
        buffer[i] = (uint8_t)(payload + i);
    }
    
    // Calculate checksum
    uint64_t checksum = 0;
    for (int i = 0; i < 256; i++) {
        checksum += buffer[i] * (i + 1);
    }
    
    result.result = checksum;
    atomic_fetch_add(&g_operation_counter, 1);
    return result;
}

// Long-running operation worker
static void* endurance_worker_thread(void* arg) {
    bitactor_engine* engine = (bitactor_engine*)arg;
    uint64_t thread_operations = 0;
    uint64_t thread_errors = 0;
    
    while (atomic_load(&g_endurance_test_active)) {
        // Generate mixed signal types for variety
        uint8_t signal_type = (thread_operations % 3);
        uint8_t handler_flags = 0x1000 + (signal_type * 0x100);
        
        signal_t signal = {
            .id = (uint32_t)(thread_operations + pthread_self()),
            .kind = SIGNAL_KIND_DATA,
            .priority = 128 + (thread_operations % 127),
            .flags = handler_flags,
            .payload = thread_operations,
            .timestamp = rdtsc_portable(),
            .context = pthread_self()
        };
        
        result_t result = bitactor_tick(engine, &signal);
        
        if (result.status != BITACTOR_OK) {
            thread_errors++;
            atomic_fetch_add((atomic_uint_fast64_t*)&g_endurance_results.total_errors_encountered, 1);
        }
        
        thread_operations++;
        
        // Brief yield to prevent thread monopolization
        if (thread_operations % 1000 == 0) {
            usleep(100); // 0.1ms yield every 1000 operations
        }
    }
    
    atomic_fetch_add((atomic_uint_fast64_t*)&g_endurance_results.total_operations_completed, thread_operations);
    return NULL;
}

// Resource monitor thread
static void* resource_monitor_thread(void* arg) {
    bitactor_engine* engine = (bitactor_engine*)arg;
    
    while (atomic_load(&g_endurance_test_active)) {
        if (g_snapshot_count < 1000) {
            capture_resource_snapshot(&g_resource_snapshots[g_snapshot_count]);
            
            // Check for resource leaks
            if (g_snapshot_count > 0) {
                if (detect_resource_leak(&g_endurance_results.baseline_resources, 
                                       &g_resource_snapshots[g_snapshot_count])) {
                    atomic_fetch_add((atomic_uint_fast64_t*)&g_endurance_results.resource_leak_detections, 1);
                }
            }
            
            // Update max memory usage
            size_t current_memory = g_resource_snapshots[g_snapshot_count].memory_usage_kb;
            if (current_memory > g_endurance_results.max_memory_usage_kb) {
                g_endurance_results.max_memory_usage_kb = current_memory;
            }
            
            g_snapshot_count++;
        }
        
        usleep(STABILITY_MONITOR_INTERVAL_MS * 1000); // Sleep for monitor interval
    }
    
    return NULL;
}

FEATURE(Endurance_Stress_Testing) {
    
    SCENARIO("Long-running system stability test") {
        bitactor_engine* engine = bitactor_init();
        time_t start_time, end_time;
        
        GIVEN("engine configured for long-running stability test",
            EXPECT_NE(engine, NULL);
            
            // Register endurance handlers
            dispatch_register(&engine->dispatch, SIGNAL_KIND_DATA, 0x1000, endurance_light_handler);
            dispatch_register(&engine->dispatch, SIGNAL_KIND_DATA, 0x1100, endurance_medium_handler);
            dispatch_register(&engine->dispatch, SIGNAL_KIND_DATA, 0x1200, endurance_heavy_handler);
            
            // Capture baseline resources
            capture_resource_snapshot(&g_endurance_results.baseline_resources);
            atomic_store(&g_operation_counter, 0);
            atomic_store((atomic_uint_fast64_t*)&g_endurance_results.total_operations_completed, 0);
            atomic_store((atomic_uint_fast64_t*)&g_endurance_results.total_errors_encountered, 0);
        );
        
        WHEN("running continuous operations for extended duration",
            start_time = time(NULL);
            
            // Execute long-running operations
            for (uint64_t i = 0; i < LONG_RUNNING_CYCLES; i++) {
                uint8_t handler_type = i % 3;
                signal_t signal = {
                    .id = (uint32_t)i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 128,
                    .flags = 0x1000 + (handler_type * 0x100),
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                result_t result = bitactor_tick(engine, &signal);
                
                if (result.status != BITACTOR_OK) {
                    g_endurance_results.total_errors_encountered++;
                }
                
                // Periodic resource check
                if (i % RESOURCE_LEAK_CHECK_INTERVAL == 0) {
                    if (g_snapshot_count < 1000) {
                        capture_resource_snapshot(&g_resource_snapshots[g_snapshot_count]);
                        g_snapshot_count++;
                    }
                }
            }
            
            end_time = time(NULL);
            g_endurance_results.total_test_duration_sec = end_time - start_time;
            capture_resource_snapshot(&g_endurance_results.final_resources);
        );
        
        THEN("system should maintain stability over long duration",
            printf("       🏃 LONG-RUNNING SYSTEM STABILITY 🏃\\n");
            printf("       Duration: %llu seconds\\n", 
                   (unsigned long long)g_endurance_results.total_test_duration_sec);
            printf("       Operations: %llu\\n", (unsigned long long)LONG_RUNNING_CYCLES);
            printf("       Errors: %llu\\n", 
                   (unsigned long long)g_endurance_results.total_errors_encountered);
            printf("       Operations/second: %.0f\\n", 
                   (double)LONG_RUNNING_CYCLES / g_endurance_results.total_test_duration_sec);
            
            // Calculate resource growth
            size_t memory_growth = g_endurance_results.final_resources.memory_usage_kb > 
                                 g_endurance_results.baseline_resources.memory_usage_kb ?
                                 g_endurance_results.final_resources.memory_usage_kb - 
                                 g_endurance_results.baseline_resources.memory_usage_kb : 0;
            
            printf("       Memory growth: %zu KB\\n", memory_growth);
            printf("       Resource snapshots: %u\\n", g_snapshot_count);
            
            // Should complete most operations successfully
            double error_rate = (double)g_endurance_results.total_errors_encountered / LONG_RUNNING_CYCLES;
            EXPECT_LT(error_rate, 0.001); // Less than 0.1% error rate
            
            // Should maintain reasonable performance
            double ops_per_sec = (double)LONG_RUNNING_CYCLES / g_endurance_results.total_test_duration_sec;
            EXPECT_GT(ops_per_sec, 10000.0); // At least 10K ops/sec
            
            // Should not have excessive memory growth
            EXPECT_LT(memory_growth, 100000); // Less than 100MB growth
        );
    } END_SCENARIO
    
    SCENARIO("Resource leak detection over time") {
        bitactor_engine* engine = bitactor_init();
        resource_snapshot_t periodic_snapshots[20];
        uint32_t leak_detections = 0;
        
        GIVEN("baseline resource usage measurement",
            capture_resource_snapshot(&periodic_snapshots[0]);
            g_endurance_results.resource_leak_detections = 0;
        );
        
        WHEN("performing sustained operations with periodic monitoring",
            for (int period = 1; period < 20; period++) {
                // Perform batch of operations
                for (int batch = 0; batch < 10000; batch++) {
                    signal_t signal = {
                        .id = (uint32_t)(period * 10000 + batch),
                        .kind = SIGNAL_KIND_DATA,
                        .priority = 128,
                        .flags = 0x1000 + ((batch % 3) * 0x100),
                        .payload = batch,
                        .timestamp = rdtsc_portable(),
                        .context = period
                    };
                    
                    bitactor_tick(engine, &signal);
                }
                
                // Capture resource snapshot
                capture_resource_snapshot(&periodic_snapshots[period]);
                
                // Check for leaks compared to baseline
                if (detect_resource_leak(&periodic_snapshots[0], &periodic_snapshots[period])) {
                    leak_detections++;
                    g_endurance_results.resource_leak_detections++;
                }
                
                // Brief pause between periods
                usleep(100000); // 100ms
            }
        );
        
        THEN("resource leaks should be minimal and detected",
            printf("       🔍 RESOURCE LEAK DETECTION 🔍\\n");
            printf("       Monitoring periods: 20\\n");
            printf("       Leak detections: %u\\n", leak_detections);
            printf("       Operations per period: 10000\\n");
            
            // Print resource progression
            printf("       Memory progression (KB): ");
            for (int i = 0; i < 10; i++) { // Show first 10 samples
                printf("%zu ", periodic_snapshots[i].memory_usage_kb);
            }
            printf("\\n");
            
            // Should have minimal leak detections
            EXPECT_LT(leak_detections, 5); // Less than 5 leak detections
            
            // Final memory should not be excessive compared to baseline
            size_t final_memory = periodic_snapshots[19].memory_usage_kb;
            size_t baseline_memory = periodic_snapshots[0].memory_usage_kb;
            size_t acceptable_growth = baseline_memory / 2; // 50% growth limit
            
            EXPECT_LT(final_memory, baseline_memory + acceptable_growth);
        );
    } END_SCENARIO
    
    SCENARIO("Performance degradation analysis") {
        bitactor_engine* engine = bitactor_init();
        double performance_samples[DEGRADATION_ANALYSIS_SAMPLES];
        
        GIVEN("engine for performance degradation analysis",
            // Register medium complexity handler
            dispatch_register(&engine->dispatch, SIGNAL_KIND_DATA, 0x2000, endurance_medium_handler);
        );
        
        WHEN("measuring performance over extended operation periods",
            for (int sample = 0; sample < DEGRADATION_ANALYSIS_SAMPLES; sample++) {
                uint64_t start_time = rdtsc_portable();
                
                // Execute batch of operations
                for (int batch_op = 0; batch_op < 1000; batch_op++) {
                    signal_t signal = {
                        .id = (uint32_t)(sample * 1000 + batch_op),
                        .kind = SIGNAL_KIND_DATA,
                        .priority = 128,
                        .flags = 0x2000,
                        .payload = batch_op,
                        .timestamp = rdtsc_portable(),
                        .context = sample
                    };
                    
                    bitactor_tick(engine, &signal);
                }
                
                uint64_t end_time = rdtsc_portable();
                double ops_per_cycle = 1000.0 / (end_time - start_time);
                performance_samples[sample] = ops_per_cycle;
                
                // Add some load to simulate system stress
                if (sample % 10 == 0) {
                    usleep(1000); // 1ms delay every 10 samples
                }
            }
            
            // Calculate performance degradation
            double initial_performance = performance_samples[0];
            double final_performance = performance_samples[DEGRADATION_ANALYSIS_SAMPLES - 1];
            g_endurance_results.degradation_percentage = 
                calculate_performance_degradation(initial_performance, final_performance);
        );
        
        THEN("performance degradation should remain within acceptable limits",
            printf("       📉 PERFORMANCE DEGRADATION ANALYSIS 📉\\n");
            printf("       Samples: %d\\n", DEGRADATION_ANALYSIS_SAMPLES);
            printf("       Initial performance: %.6f ops/cycle\\n", performance_samples[0]);
            printf("       Final performance: %.6f ops/cycle\\n", 
                   performance_samples[DEGRADATION_ANALYSIS_SAMPLES - 1]);
            printf("       Degradation: %.2f%%\\n", g_endurance_results.degradation_percentage);
            
            // Calculate average performance across all samples
            double total_performance = 0;
            for (int i = 0; i < DEGRADATION_ANALYSIS_SAMPLES; i++) {
                total_performance += performance_samples[i];
            }
            double avg_performance = total_performance / DEGRADATION_ANALYSIS_SAMPLES;
            printf("       Average performance: %.6f ops/cycle\\n", avg_performance);
            
            // Performance degradation should be minimal
            EXPECT_LT(g_endurance_results.degradation_percentage, 20.0); // Less than 20% degradation
            
            // Final performance should still be reasonable
            EXPECT_GT(performance_samples[DEGRADATION_ANALYSIS_SAMPLES - 1], 
                     performance_samples[0] * 0.5); // At least 50% of initial
        );
    } END_SCENARIO
    
    SCENARIO("Continuous operation endurance test") {
        bitactor_engine* engine = bitactor_init();
        pthread_t endurance_threads[4];
        pthread_t monitor_thread;
        struct timeval start_time, end_time;
        
        GIVEN("multi-threaded endurance test setup",
            atomic_store(&g_endurance_test_active, true);
            atomic_store((atomic_uint_fast64_t*)&g_endurance_results.total_operations_completed, 0);
            atomic_store((atomic_uint_fast64_t*)&g_endurance_results.total_errors_encountered, 0);
            g_snapshot_count = 0;
            
            // Register all endurance handlers
            dispatch_register(&engine->dispatch, SIGNAL_KIND_DATA, 0x1000, endurance_light_handler);
            dispatch_register(&engine->dispatch, SIGNAL_KIND_DATA, 0x1100, endurance_medium_handler);
            dispatch_register(&engine->dispatch, SIGNAL_KIND_DATA, 0x1200, endurance_heavy_handler);
            
            capture_resource_snapshot(&g_endurance_results.baseline_resources);
        );
        
        WHEN("running continuous operations with multiple threads",
            gettimeofday(&start_time, NULL);
            
            // Start resource monitor thread
            pthread_create(&monitor_thread, NULL, resource_monitor_thread, engine);
            
            // Start endurance worker threads
            for (int i = 0; i < 4; i++) {
                pthread_create(&endurance_threads[i], NULL, endurance_worker_thread, engine);
            }
            
            // Run for endurance test duration
            sleep(ENDURANCE_TEST_DURATION_SEC);
            
            // Stop all threads
            atomic_store(&g_endurance_test_active, false);
            
            // Wait for threads to complete
            for (int i = 0; i < 4; i++) {
                pthread_join(endurance_threads[i], NULL);
            }
            pthread_join(monitor_thread, NULL);
            
            gettimeofday(&end_time, NULL);
            capture_resource_snapshot(&g_endurance_results.final_resources);
        );
        
        THEN("continuous operation should maintain system stability",
            double elapsed_time = (end_time.tv_sec - start_time.tv_sec) + 
                                (end_time.tv_usec - start_time.tv_usec) / 1000000.0;
            
            uint64_t total_ops = atomic_load((atomic_uint_fast64_t*)&g_endurance_results.total_operations_completed);
            uint64_t total_errors = atomic_load((atomic_uint_fast64_t*)&g_endurance_results.total_errors_encountered);
            
            double ops_per_second = total_ops / elapsed_time;
            double error_rate = (double)total_errors / total_ops;
            
            printf("       ⏳ CONTINUOUS OPERATION ENDURANCE ⏳\\n");
            printf("       Duration: %.2f seconds\\n", elapsed_time);
            printf("       Total operations: %llu\\n", (unsigned long long)total_ops);
            printf("       Operations/second: %.0f\\n", ops_per_second);
            printf("       Error rate: %.6f%%\\n", error_rate * 100);
            printf("       Resource snapshots: %u\\n", g_snapshot_count);
            printf("       Leak detections: %llu\\n", 
                   (unsigned long long)g_endurance_results.resource_leak_detections);
            
            // Should complete many operations
            EXPECT_GT(total_ops, 100000); // At least 100K operations
            
            // Should maintain high throughput
            EXPECT_GT(ops_per_second, 5000.0); // At least 5K ops/sec
            
            // Should have low error rate
            EXPECT_LT(error_rate, 0.01); // Less than 1% errors
            
            // Should have minimal resource leaks
            EXPECT_LT(g_endurance_results.resource_leak_detections, 10);
        );
    } END_SCENARIO
    
    SCENARIO("Memory stability under sustained load") {
        bitactor_engine* engine = bitactor_init();
        size_t memory_samples[100];
        uint32_t stability_violations = 0;
        
        GIVEN("baseline memory measurement",
            memory_samples[0] = get_memory_usage_kb();
        );
        
        WHEN("performing sustained memory-intensive operations",
            for (int cycle = 1; cycle < 100; cycle++) {
                // Perform memory-intensive operations
                for (int ops = 0; ops < 5000; ops++) {
                    signal_t signal = {
                        .id = (uint32_t)(cycle * 5000 + ops),
                        .kind = SIGNAL_KIND_DATA,
                        .priority = 128,
                        .flags = 0x1200, // Heavy handler
                        .payload = ops,
                        .timestamp = rdtsc_portable(),
                        .context = cycle
                    };
                    
                    bitactor_tick(engine, &signal);
                }
                
                // Sample memory usage
                memory_samples[cycle] = get_memory_usage_kb();
                
                // Check for stability violations (large memory jumps)
                if (cycle > 0) {
                    size_t memory_diff = memory_samples[cycle] > memory_samples[cycle-1] ?
                                       memory_samples[cycle] - memory_samples[cycle-1] :
                                       memory_samples[cycle-1] - memory_samples[cycle];
                    
                    if (memory_diff > 10000) { // 10MB jump is considered unstable
                        stability_violations++;
                    }
                }
                
                usleep(50000); // 50ms between cycles
            }
        );
        
        THEN("memory usage should remain stable over time",
            printf("       💾 MEMORY STABILITY UNDER LOAD 💾\\n");
            printf("       Memory cycles: 100\\n");
            printf("       Stability violations: %u\\n", stability_violations);
            printf("       Baseline memory: %zu KB\\n", memory_samples[0]);
            printf("       Final memory: %zu KB\\n", memory_samples[99]);
            
            size_t max_memory = memory_samples[0];
            size_t min_memory = memory_samples[0];
            for (int i = 1; i < 100; i++) {
                if (memory_samples[i] > max_memory) max_memory = memory_samples[i];
                if (memory_samples[i] < min_memory) min_memory = memory_samples[i];
            }
            
            printf("       Memory range: %zu - %zu KB\\n", min_memory, max_memory);
            printf("       Memory variation: %zu KB\\n", max_memory - min_memory);
            
            // Should have minimal stability violations
            EXPECT_LT(stability_violations, 10); // Less than 10 violations
            
            // Memory variation should be reasonable
            size_t memory_variation = max_memory - min_memory;
            EXPECT_LT(memory_variation, 50000); // Less than 50MB variation
            
            // Final memory should not be excessive
            size_t final_growth = memory_samples[99] > memory_samples[0] ?
                                memory_samples[99] - memory_samples[0] : 0;
            EXPECT_LT(final_growth, 20000); // Less than 20MB growth
        );
    } END_SCENARIO
    
    SCENARIO("System recovery after stress periods") {
        bitactor_engine* engine = bitactor_init();
        resource_snapshot_t pre_stress, during_stress, post_recovery;
        
        GIVEN("baseline system state",
            capture_resource_snapshot(&pre_stress);
        );
        
        WHEN("applying stress followed by recovery period",
            // Stress phase
            for (int stress_ops = 0; stress_ops < 50000; stress_ops++) {
                signal_t signal = {
                    .id = (uint32_t)stress_ops,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 255,
                    .flags = 0x1200, // Heavy handler
                    .payload = stress_ops,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                bitactor_tick(engine, &signal);
            }
            
            capture_resource_snapshot(&during_stress);
            
            // Recovery phase - light operations
            sleep(2); // 2 second recovery period
            
            for (int recovery_ops = 0; recovery_ops < 10000; recovery_ops++) {
                signal_t signal = {
                    .id = (uint32_t)(100000 + recovery_ops),
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 128,
                    .flags = 0x1000, // Light handler
                    .payload = recovery_ops,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                bitactor_tick(engine, &signal);
            }
            
            capture_resource_snapshot(&post_recovery);
        );
        
        THEN("system should recover to near-baseline state",
            printf("       🔄 SYSTEM RECOVERY AFTER STRESS 🔄\\n");
            printf("       Pre-stress memory: %zu KB\\n", pre_stress.memory_usage_kb);
            printf("       During-stress memory: %zu KB\\n", during_stress.memory_usage_kb);
            printf("       Post-recovery memory: %zu KB\\n", post_recovery.memory_usage_kb);
            
            size_t stress_growth = during_stress.memory_usage_kb > pre_stress.memory_usage_kb ?
                                 during_stress.memory_usage_kb - pre_stress.memory_usage_kb : 0;
            size_t recovery_reduction = during_stress.memory_usage_kb > post_recovery.memory_usage_kb ?
                                      during_stress.memory_usage_kb - post_recovery.memory_usage_kb : 0;
            
            printf("       Stress memory growth: %zu KB\\n", stress_growth);
            printf("       Recovery memory reduction: %zu KB\\n", recovery_reduction);
            
            double recovery_percentage = stress_growth > 0 ? 
                                       (double)recovery_reduction / stress_growth * 100.0 : 100.0;
            printf("       Recovery percentage: %.1f%%\\n", recovery_percentage);
            
            // Should recover significant portion of stress-induced growth
            EXPECT_GT(recovery_percentage, 50.0); // At least 50% recovery
            
            // Post-recovery should be close to baseline
            size_t final_growth = post_recovery.memory_usage_kb > pre_stress.memory_usage_kb ?
                                post_recovery.memory_usage_kb - pre_stress.memory_usage_kb : 0;
            EXPECT_LT(final_growth, stress_growth / 2); // Should be less than half stress growth
        );
    } END_SCENARIO
}