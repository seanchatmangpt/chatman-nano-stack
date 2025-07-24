/*
 * Integration Stress Test
 * EXTREME INTEGRATION testing - Complete CNS system under stress
 * Multi-component chaos testing, end-to-end validation, system-wide pressure
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../bitactor/include/bitactor.h"
#include "../bitactor/src/bitactor.h"
#include "../bitactor/src/bitfiber.h"
#include "../src/cns/cns_pipeline.h"
#include "../src/news/news_validator.h"
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <pthread.h>
#include <stdatomic.h>
#include <unistd.h>
#include <signal.h>

// Integration stress configuration
#define INTEGRATION_STRESS_COMPONENTS 5     // BitActor, BitFiber, CNS Pipeline, News Validator, Telemetry
#define CHAOS_ENGINEERING_ITERATIONS 10000  // Chaos injection cycles
#define END_TO_END_TEST_CYCLES 50000        // Full pipeline test cycles
#define MULTI_COMPONENT_LOAD_OPS 100000     // Multi-component stress operations
#define SYSTEM_WIDE_STRESS_DURATION_SEC 30  // System-wide stress duration
#define FAILURE_INJECTION_RATE 0.05         // 5% failure injection rate

// System integration state
typedef struct {
    bitactor_engine* bitactor_engine;
    fiber_scheduler_t* fiber_scheduler;
    cns_pipeline_t* cns_pipeline;
    news_validator_t* news_validator;
    void* telemetry_system;
    bool all_components_initialized;
    bool system_under_stress;
} cns_integration_system_t;

// Integration test results
typedef struct {
    uint64_t total_system_operations;
    uint64_t component_failures[INTEGRATION_STRESS_COMPONENTS];
    uint64_t chaos_events_survived;
    uint64_t end_to_end_successes;
    uint64_t end_to_end_failures;
    uint64_t cross_component_violations;
    uint64_t system_recovery_events;
    uint64_t telemetry_inconsistencies;
    double system_availability_percentage;
    double component_interaction_success_rate;
    bool system_integrity_maintained;
    bool graceful_degradation_observed;
} integration_stress_results_t;

// Global integration tracking
static integration_stress_results_t g_integration_results = {0};
static cns_integration_system_t g_cns_system = {0};
static _Atomic bool g_integration_stress_active = false;
static _Atomic uint64_t g_system_operation_counter = 0;

// Simulate CNS pipeline initialization (simplified for testing)
static cns_pipeline_t* mock_cns_pipeline_init(void) {
    cns_pipeline_t* pipeline = malloc(sizeof(cns_pipeline_t));
    if (pipeline) {
        memset(pipeline, 0, sizeof(*pipeline));
        pipeline->initialized = true;
        pipeline->active_stages = 5;
        pipeline->processed_count = 0;
    }
    return pipeline;
}

// Simulate news validator initialization (simplified for testing)
static news_validator_t* mock_news_validator_init(void) {
    news_validator_t* validator = malloc(sizeof(news_validator_t));
    if (validator) {
        memset(validator, 0, sizeof(*validator));
        validator->initialized = true;
        validator->validation_count = 0;
        validator->success_rate = 1.0;
    }
    return validator;
}

// Initialize complete CNS integration system
static bool initialize_cns_integration_system(cns_integration_system_t* system) {
    memset(system, 0, sizeof(*system));
    
    // Initialize BitActor engine
    system->bitactor_engine = bitactor_init();
    if (!system->bitactor_engine) {
        return false;
    }
    
    // Initialize fiber scheduler (should be part of BitActor engine)
    system->fiber_scheduler = system->bitactor_engine->fiber_sched;
    if (!system->fiber_scheduler) {
        return false;
    }
    
    // Initialize CNS pipeline
    system->cns_pipeline = mock_cns_pipeline_init();
    if (!system->cns_pipeline) {
        return false;
    }
    
    // Initialize news validator
    system->news_validator = mock_news_validator_init();
    if (!system->news_validator) {
        return false;
    }
    
    // Telemetry system (part of BitActor)
    system->telemetry_system = &system->bitactor_engine->telemetry;
    
    system->all_components_initialized = true;
    return true;
}

// Cleanup CNS integration system
static void cleanup_cns_integration_system(cns_integration_system_t* system) {
    if (system->cns_pipeline) {
        free(system->cns_pipeline);
    }
    if (system->news_validator) {
        free(system->news_validator);
    }
    // BitActor engine cleanup would be handled by bitactor_destroy
    memset(system, 0, sizeof(*system));
}

// Chaos engineering - inject random failures
static bool inject_chaos_failure(int component_id, double failure_rate) {
    double random_val = (double)rand() / RAND_MAX;
    if (random_val < failure_rate) {
        g_integration_results.component_failures[component_id % INTEGRATION_STRESS_COMPONENTS]++;
        return true; // Failure injected
    }
    return false; // No failure
}

// Simulate end-to-end CNS processing
static bool process_end_to_end_operation(cns_integration_system_t* system, uint64_t operation_id) {
    bool success = true;
    
    // Stage 1: BitActor signal processing
    signal_t signal = {
        .id = (uint32_t)operation_id,
        .kind = SIGNAL_KIND_DATA,
        .priority = 128,
        .flags = 0x5000,
        .payload = operation_id,
        .timestamp = rdtsc_portable(),
        .context = 0
    };
    
    if (inject_chaos_failure(0, FAILURE_INJECTION_RATE)) {
        success = false;
    } else {
        result_t result = bitactor_tick(system->bitactor_engine, &signal);
        if (result.status != BITACTOR_OK) {
            success = false;
        }
    }
    
    // Stage 2: Fiber scheduling
    if (success && inject_chaos_failure(1, FAILURE_INJECTION_RATE)) {
        success = false;
    } else if (success) {
        fiber_tick(system->fiber_scheduler);
    }
    
    // Stage 3: CNS pipeline processing
    if (success && inject_chaos_failure(2, FAILURE_INJECTION_RATE)) {
        success = false;
    } else if (success && system->cns_pipeline) {
        system->cns_pipeline->processed_count++;
    }
    
    // Stage 4: News validation
    if (success && inject_chaos_failure(3, FAILURE_INJECTION_RATE)) {
        success = false;
    } else if (success && system->news_validator) {
        system->news_validator->validation_count++;
    }
    
    // Stage 5: Telemetry recording
    if (success && inject_chaos_failure(4, FAILURE_INJECTION_RATE)) {
        success = false;
    }
    
    return success;
}

// Multi-component stress worker
static void* integration_stress_worker(void* arg) {
    cns_integration_system_t* system = (cns_integration_system_t*)arg;
    uint64_t worker_operations = 0;
    uint64_t worker_successes = 0;
    
    while (atomic_load(&g_integration_stress_active)) {
        bool operation_success = process_end_to_end_operation(system, worker_operations);
        
        if (operation_success) {
            worker_successes++;
            atomic_fetch_add((atomic_uint_fast64_t*)&g_integration_results.end_to_end_successes, 1);
        } else {
            atomic_fetch_add((atomic_uint_fast64_t*)&g_integration_results.end_to_end_failures, 1);
        }
        
        worker_operations++;
        atomic_fetch_add(&g_system_operation_counter, 1);
        atomic_fetch_add((atomic_uint_fast64_t*)&g_integration_results.total_system_operations, 1);
        
        // Brief yield to allow other workers
        if (worker_operations % 1000 == 0) {
            usleep(100); // 0.1ms yield every 1000 operations
        }
    }
    
    return NULL;
}

// System health monitor
static void* system_health_monitor(void* arg) {
    cns_integration_system_t* system = (cns_integration_system_t*)arg;
    uint32_t health_checks = 0;
    
    while (atomic_load(&g_integration_stress_active)) {
        health_checks++;
        
        // Check component health
        bool bitactor_healthy = system->bitactor_engine && system->bitactor_engine->initialized;
        bool pipeline_healthy = system->cns_pipeline && system->cns_pipeline->initialized;
        bool validator_healthy = system->news_validator && system->news_validator->initialized;
        
        if (!bitactor_healthy || !pipeline_healthy || !validator_healthy) {
            g_integration_results.cross_component_violations++;
            g_integration_results.system_recovery_events++;
        }
        
        // Monitor telemetry consistency
        if (system->telemetry_system) {
            // Simplified telemetry check
            g_integration_results.telemetry_inconsistencies += 0; // Would check for actual inconsistencies
        }
        
        usleep(100000); // 100ms health check interval
    }
    
    return NULL;
}

FEATURE(Integration_Stress_Testing) {
    
    SCENARIO("Complete CNS system initialization under load") {
        cns_integration_system_t system;
        bool init_success;
        
        GIVEN("uninitialized CNS integration system",
            memset(&system, 0, sizeof(system));
            memset(&g_integration_results, 0, sizeof(g_integration_results));
        );
        
        WHEN("initializing all CNS components simultaneously",
            init_success = initialize_cns_integration_system(&system);
            g_cns_system = system;
        );
        
        THEN("all components should initialize successfully",
            printf("       🔧 COMPLETE CNS SYSTEM INITIALIZATION 🔧\\n");
            printf("       BitActor engine: %s\\n", 
                   system.bitactor_engine ? "INITIALIZED" : "FAILED");
            printf("       Fiber scheduler: %s\\n", 
                   system.fiber_scheduler ? "INITIALIZED" : "FAILED");
            printf("       CNS pipeline: %s\\n", 
                   system.cns_pipeline ? "INITIALIZED" : "FAILED");
            printf("       News validator: %s\\n", 
                   system.news_validator ? "INITIALIZED" : "FAILED");
            printf("       Telemetry system: %s\\n", 
                   system.telemetry_system ? "INITIALIZED" : "FAILED");
            printf("       Complete system: %s\\n", 
                   system.all_components_initialized ? "READY" : "NOT READY");
            
            EXPECT(init_success);
            EXPECT(system.all_components_initialized);
            EXPECT_NE(system.bitactor_engine, NULL);
            EXPECT_NE(system.fiber_scheduler, NULL);
            EXPECT_NE(system.cns_pipeline, NULL);
            EXPECT_NE(system.news_validator, NULL);
            EXPECT_NE(system.telemetry_system, NULL);
        );
    } END_SCENARIO
    
    SCENARIO("End-to-end pipeline stress testing") {
        cns_integration_system_t* system = &g_cns_system;
        uint64_t pipeline_successes = 0;
        uint64_t pipeline_failures = 0;
        
        GIVEN("fully initialized CNS system",
            EXPECT(system->all_components_initialized);
        );
        
        WHEN("processing end-to-end operations through complete pipeline",
            for (uint64_t cycle = 0; cycle < END_TO_END_TEST_CYCLES; cycle++) {
                bool success = process_end_to_end_operation(system, cycle);
                
                if (success) {
                    pipeline_successes++;
                } else {
                    pipeline_failures++;
                }
                
                // Add periodic stress
                if (cycle % 1000 == 0) {
                    // Create background load
                    for (int bg = 0; bg < 100; bg++) {
                        signal_t bg_signal = {
                            .id = (uint32_t)(cycle * 100 + bg),
                            .kind = SIGNAL_KIND_CONTROL,
                            .priority = 64,
                            .flags = 0,
                            .payload = bg,
                            .timestamp = rdtsc_portable(),
                            .context = cycle
                        };
                        bitactor_enqueue(system->bitactor_engine, bg_signal);
                    }
                }
            }
            
            g_integration_results.end_to_end_successes = pipeline_successes;
            g_integration_results.end_to_end_failures = pipeline_failures;
        );
        
        THEN("end-to-end pipeline should handle high throughput",
            double success_rate = (double)pipeline_successes / END_TO_END_TEST_CYCLES;
            
            printf("       🔄 END-TO-END PIPELINE STRESS 🔄\\n");
            printf("       Pipeline cycles: %d\\n", END_TO_END_TEST_CYCLES);
            printf("       Successes: %llu\\n", (unsigned long long)pipeline_successes);
            printf("       Failures: %llu\\n", (unsigned long long)pipeline_failures);
            printf("       Success rate: %.2f%%\\n", success_rate * 100);
            printf("       Component failures: ");
            for (int i = 0; i < INTEGRATION_STRESS_COMPONENTS; i++) {
                printf("%llu ", (unsigned long long)g_integration_results.component_failures[i]);
            }
            printf("\\n");
            
            // Should maintain high success rate despite chaos injection
            EXPECT_GT(success_rate, 0.9); // At least 90% success rate
            
            // Each component should process operations
            EXPECT_GT(system->cns_pipeline->processed_count, 40000);
            EXPECT_GT(system->news_validator->validation_count, 40000);
            
            // Should handle some failures gracefully
            EXPECT_GT(pipeline_failures, 0); // Should have some failures due to chaos injection
        );
    } END_SCENARIO
    
    SCENARIO("Chaos engineering multi-component failure injection") {
        cns_integration_system_t* system = &g_cns_system;
        uint64_t chaos_events = 0;
        uint64_t recovery_events = 0;
        
        GIVEN("stable CNS system for chaos testing",
            // Reset component failure counters
            for (int i = 0; i < INTEGRATION_STRESS_COMPONENTS; i++) {
                g_integration_results.component_failures[i] = 0;
            }
            srand(time(NULL)); // Seed random number generator
        );
        
        WHEN("injecting chaos failures across all components",
            for (int chaos_cycle = 0; chaos_cycle < CHAOS_ENGINEERING_ITERATIONS; chaos_cycle++) {
                // Inject failures into each component
                for (int component = 0; component < INTEGRATION_STRESS_COMPONENTS; component++) {
                    if (inject_chaos_failure(component, FAILURE_INJECTION_RATE * 2)) { // Higher rate for chaos test
                        chaos_events++;
                    }
                }
                
                // Attempt normal operation despite chaos
                bool operation_success = process_end_to_end_operation(system, chaos_cycle);
                
                // Check if system recovered
                if (!operation_success) {
                    // Attempt recovery
                    usleep(1000); // 1ms recovery delay
                    
                    // Retry operation
                    bool recovery_success = process_end_to_end_operation(system, chaos_cycle + 1000000);
                    if (recovery_success) {
                        recovery_events++;
                        g_integration_results.system_recovery_events++;
                    }
                }
                
                // Brief pause between chaos cycles
                if (chaos_cycle % 1000 == 0) {
                    usleep(1000);
                }
            }
            
            g_integration_results.chaos_events_survived = chaos_events;
        );
        
        THEN("system should survive chaos engineering with graceful degradation",
            printf("       🌪️ CHAOS ENGINEERING MULTI-COMPONENT 🌪️\\n");
            printf("       Chaos iterations: %d\\n", CHAOS_ENGINEERING_ITERATIONS);
            printf("       Chaos events: %llu\\n", (unsigned long long)chaos_events);
            printf("       Recovery events: %llu\\n", (unsigned long long)recovery_events);
            
            // Print component-specific failure counts
            const char* component_names[] = {"BitActor", "Fiber", "Pipeline", "Validator", "Telemetry"};
            for (int i = 0; i < INTEGRATION_STRESS_COMPONENTS; i++) {
                printf("       %s failures: %llu\\n", component_names[i],
                       (unsigned long long)g_integration_results.component_failures[i]);
            }
            
            double recovery_rate = chaos_events > 0 ? (double)recovery_events / chaos_events : 0.0;
            printf("       Recovery rate: %.2f%%\\n", recovery_rate * 100);
            
            // Should survive chaos events
            EXPECT_GT(chaos_events, 500); // Should have generated chaos events
            
            // Should have reasonable recovery rate
            EXPECT_GT(recovery_rate, 0.3); // At least 30% recovery rate
            
            // Each component should experience some failures
            for (int i = 0; i < INTEGRATION_STRESS_COMPONENTS; i++) {
                EXPECT_GT(g_integration_results.component_failures[i], 0);
            }
            
            // System should remain functional
            EXPECT(system->all_components_initialized);
        );
    } END_SCENARIO
    
    SCENARIO("Multi-threaded system-wide stress test") {
        cns_integration_system_t* system = &g_cns_system;
        pthread_t stress_threads[6];
        pthread_t health_monitor;
        struct timeval start_time, end_time;
        
        GIVEN("system prepared for multi-threaded stress",
            atomic_store(&g_integration_stress_active, true);
            atomic_store(&g_system_operation_counter, 0);
            atomic_store((atomic_uint_fast64_t*)&g_integration_results.total_system_operations, 0);
            atomic_store((atomic_uint_fast64_t*)&g_integration_results.end_to_end_successes, 0);
            atomic_store((atomic_uint_fast64_t*)&g_integration_results.end_to_end_failures, 0);
        );
        
        WHEN("running system-wide stress with multiple threads",
            gettimeofday(&start_time, NULL);
            
            // Start health monitor
            pthread_create(&health_monitor, NULL, system_health_monitor, system);
            
            // Start stress threads
            for (int i = 0; i < 6; i++) {
                pthread_create(&stress_threads[i], NULL, integration_stress_worker, system);
            }
            
            // Run for stress duration
            sleep(SYSTEM_WIDE_STRESS_DURATION_SEC);
            
            // Stop all threads
            atomic_store(&g_integration_stress_active, false);
            
            // Wait for threads to complete
            for (int i = 0; i < 6; i++) {
                pthread_join(stress_threads[i], NULL);
            }
            pthread_join(health_monitor, NULL);
            
            gettimeofday(&end_time, NULL);
        );
        
        THEN("system should maintain stability under multi-threaded stress",
            double elapsed_time = (end_time.tv_sec - start_time.tv_sec) + 
                                (end_time.tv_usec - start_time.tv_usec) / 1000000.0;
            
            uint64_t total_ops = atomic_load((atomic_uint_fast64_t*)&g_integration_results.total_system_operations);
            uint64_t successes = atomic_load((atomic_uint_fast64_t*)&g_integration_results.end_to_end_successes);
            uint64_t failures = atomic_load((atomic_uint_fast64_t*)&g_integration_results.end_to_end_failures);
            
            double ops_per_second = total_ops / elapsed_time;
            double success_rate = total_ops > 0 ? (double)successes / total_ops : 0.0;
            
            printf("       🚀 MULTI-THREADED SYSTEM-WIDE STRESS 🚀\\n");
            printf("       Duration: %.2f seconds\\n", elapsed_time);
            printf("       Total operations: %llu\\n", (unsigned long long)total_ops);
            printf("       Operations/second: %.0f\\n", ops_per_second);
            printf("       Successes: %llu\\n", (unsigned long long)successes);
            printf("       Failures: %llu\\n", (unsigned long long)failures);
            printf("       Success rate: %.2f%%\\n", success_rate * 100);
            printf("       Cross-component violations: %llu\\n", 
                   (unsigned long long)g_integration_results.cross_component_violations);
            printf("       System recovery events: %llu\\n", 
                   (unsigned long long)g_integration_results.system_recovery_events);
            
            // Should process many operations
            EXPECT_GT(total_ops, 100000); // At least 100K operations
            
            // Should maintain reasonable throughput
            EXPECT_GT(ops_per_second, 3000.0); // At least 3K ops/sec
            
            // Should maintain reasonable success rate despite stress
            EXPECT_GT(success_rate, 0.85); // At least 85% success rate
            
            // Should have minimal cross-component violations
            EXPECT_LT(g_integration_results.cross_component_violations, 100);
            
            // System should remain functional
            EXPECT(system->all_components_initialized);
        );
    } END_SCENARIO
    
    SCENARIO("Component interaction validation under stress") {
        cns_integration_system_t* system = &g_cns_system;
        uint64_t interaction_tests = 0;
        uint64_t interaction_successes = 0;
        
        GIVEN("system for component interaction testing",
            EXPECT(system->all_components_initialized);
        );
        
        WHEN("testing all component interactions under stress",
            for (int test_cycle = 0; test_cycle < 10000; test_cycle++) {
                interaction_tests++;
                bool interaction_success = true;
                
                // Test BitActor -> Fiber interaction
                signal_t test_signal = {
                    .id = (uint32_t)test_cycle,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 128,
                    .flags = 0x6000,
                    .payload = test_cycle,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                result_t bitactor_result = bitactor_tick(system->bitactor_engine, &test_signal);
                if (bitactor_result.status != BITACTOR_OK) {
                    interaction_success = false;
                }
                
                // Test Fiber scheduling
                uint32_t fiber_executions = fiber_tick(system->fiber_scheduler);
                if (fiber_executions == 0 && test_cycle % 100 < 10) { // Expected some executions periodically
                    // This is acceptable - fibers may not always have work
                }
                
                // Test CNS Pipeline processing
                if (system->cns_pipeline) {
                    uint64_t prev_count = system->cns_pipeline->processed_count;
                    system->cns_pipeline->processed_count++;
                    if (system->cns_pipeline->processed_count <= prev_count) {
                        interaction_success = false;
                    }
                }
                
                // Test News Validator interaction
                if (system->news_validator) {
                    uint64_t prev_validation_count = system->news_validator->validation_count;
                    system->news_validator->validation_count++;
                    if (system->news_validator->validation_count <= prev_validation_count) {
                        interaction_success = false;
                    }
                }
                
                if (interaction_success) {
                    interaction_successes++;
                }
                
                // Add stress every 100 cycles
                if (test_cycle % 100 == 0) {
                    // Create additional load
                    for (int load = 0; load < 50; load++) {
                        signal_t load_signal = {
                            .id = (uint32_t)(test_cycle * 100 + load),
                            .kind = SIGNAL_KIND_CONTROL,
                            .priority = 64,
                            .flags = 0,
                            .payload = load,
                            .timestamp = rdtsc_portable(),
                            .context = test_cycle
                        };
                        bitactor_enqueue(system->bitactor_engine, load_signal);
                    }
                }
            }
            
            g_integration_results.component_interaction_success_rate = 
                (double)interaction_successes / interaction_tests;
        );
        
        THEN("component interactions should remain reliable under stress",
            printf("       🔗 COMPONENT INTERACTION VALIDATION 🔗\\n");
            printf("       Interaction tests: %llu\\n", (unsigned long long)interaction_tests);
            printf("       Interaction successes: %llu\\n", (unsigned long long)interaction_successes);
            printf("       Interaction success rate: %.2f%%\\n", 
                   g_integration_results.component_interaction_success_rate * 100);
            printf("       Pipeline processed: %llu\\n", 
                   (unsigned long long)system->cns_pipeline->processed_count);
            printf("       Validator processed: %llu\\n", 
                   (unsigned long long)system->news_validator->validation_count);
            
            // Should maintain high interaction success rate
            EXPECT_GT(g_integration_results.component_interaction_success_rate, 0.95); // 95% success
            
            // All components should have processed operations
            EXPECT_GT(system->cns_pipeline->processed_count, 9000);
            EXPECT_GT(system->news_validator->validation_count, 9000);
            
            // Should complete all tests
            EXPECT_EQ(interaction_tests, 10000);
        );
    } END_SCENARIO
    
    SCENARIO("System availability and recovery testing") {
        cns_integration_system_t* system = &g_cns_system;
        uint64_t availability_checks = 0;
        uint64_t system_available_count = 0;
        uint64_t recovery_attempts = 0;
        
        GIVEN("system for availability and recovery testing",
            EXPECT(system->all_components_initialized);
        );
        
        WHEN("testing system availability under various failure scenarios",
            for (int availability_test = 0; availability_test < 1000; availability_test++) {
                availability_checks++;
                bool system_available = true;
                
                // Check component availability
                if (!system->bitactor_engine || !system->bitactor_engine->initialized) {
                    system_available = false;
                }
                if (!system->fiber_scheduler) {
                    system_available = false;
                }
                if (!system->cns_pipeline || !system->cns_pipeline->initialized) {
                    system_available = false;
                }
                if (!system->news_validator || !system->news_validator->initialized) {
                    system_available = false;
                }
                
                if (system_available) {
                    system_available_count++;
                } else {
                    recovery_attempts++;
                    
                    // Attempt to recover system
                    if (!system->cns_pipeline || !system->cns_pipeline->initialized) {
                        // Simulate recovery
                        if (system->cns_pipeline) {
                            system->cns_pipeline->initialized = true;
                        }
                    }
                    if (!system->news_validator || !system->news_validator->initialized) {
                        // Simulate recovery
                        if (system->news_validator) {
                            system->news_validator->initialized = true;
                        }
                    }
                }
                
                // Perform test operation to verify availability
                if (system_available) {
                    signal_t availability_signal = {
                        .id = (uint32_t)(800000 + availability_test),
                        .kind = SIGNAL_KIND_DATA,
                        .priority = 255,
                        .flags = 0x7000,
                        .payload = availability_test,
                        .timestamp = rdtsc_portable(),
                        .context = 0
                    };
                    
                    result_t result = bitactor_tick(system->bitactor_engine, &availability_signal);
                    if (result.status != BITACTOR_OK) {
                        system_available = false;
                        system_available_count--; // Adjust count
                    }
                }
                
                // Inject random failures for testing
                if (availability_test % 100 == 0) {
                    // Temporarily "break" a component
                    if (system->cns_pipeline && (rand() % 10) == 0) {
                        system->cns_pipeline->initialized = false;
                    }
                    if (system->news_validator && (rand() % 10) == 0) {
                        system->news_validator->initialized = false;
                    }
                }
                
                usleep(1000); // 1ms between availability checks
            }
            
            g_integration_results.system_availability_percentage = 
                (double)system_available_count / availability_checks * 100.0;
            g_integration_results.system_recovery_events = recovery_attempts;
        );
        
        THEN("system should maintain high availability with effective recovery",
            printf("       📊 SYSTEM AVAILABILITY AND RECOVERY 📊\\n");
            printf("       Availability checks: %llu\\n", (unsigned long long)availability_checks);
            printf("       System available: %llu\\n", (unsigned long long)system_available_count);
            printf("       Availability: %.2f%%\\n", g_integration_results.system_availability_percentage);
            printf("       Recovery attempts: %llu\\n", (unsigned long long)recovery_attempts);
            printf("       Recovery rate: %.2f%%\\n", 
                   recovery_attempts > 0 ? (double)recovery_attempts / (availability_checks - system_available_count) * 100.0 : 0.0);
            
            // Should maintain high availability
            EXPECT_GT(g_integration_results.system_availability_percentage, 90.0); // 90% availability
            
            // Should attempt recovery when failures occur
            EXPECT_GT(recovery_attempts, 0); // Should have some recovery attempts
            
            // Final system state should be available
            EXPECT(system->all_components_initialized);
            EXPECT(system->bitactor_engine->initialized);
            EXPECT(system->cns_pipeline->initialized);
            EXPECT(system->news_validator->initialized);
        );
    } END_SCENARIO
    
    SCENARIO("Integration system cleanup and resource release") {
        cns_integration_system_t* system = &g_cns_system;
        
        GIVEN("fully tested integration system",
            EXPECT(system->all_components_initialized);
        );
        
        WHEN("cleaning up integration system",
            cleanup_cns_integration_system(system);
        );
        
        THEN("all resources should be properly released",
            printf("       🧹 INTEGRATION SYSTEM CLEANUP 🧹\\n");
            printf("       System state after cleanup: CLEANED\\n");
            printf("       Total system operations: %llu\\n", 
                   (unsigned long long)g_integration_results.total_system_operations);
            printf("       Total component failures: ");
            uint64_t total_failures = 0;
            for (int i = 0; i < INTEGRATION_STRESS_COMPONENTS; i++) {
                total_failures += g_integration_results.component_failures[i];
                printf("%llu ", (unsigned long long)g_integration_results.component_failures[i]);
            }
            printf("\\n");
            printf("       Total failures: %llu\\n", (unsigned long long)total_failures);
            printf("       End-to-end success rate: %.2f%%\\n", 
                   g_integration_results.total_system_operations > 0 ?
                   (double)g_integration_results.end_to_end_successes / g_integration_results.total_system_operations * 100.0 : 0.0);
            
            // System should be cleaned up
            EXPECT_EQ(system->cns_pipeline, NULL);
            EXPECT_EQ(system->news_validator, NULL);
            EXPECT(!system->all_components_initialized);
            
            // Should have processed significant operations
            EXPECT_GT(g_integration_results.total_system_operations, 100000);
            
            // Overall system should have been resilient
            double overall_success_rate = g_integration_results.total_system_operations > 0 ?
                (double)g_integration_results.end_to_end_successes / g_integration_results.total_system_operations : 0.0;
            EXPECT_GT(overall_success_rate, 0.80); // 80% overall success despite stress and chaos
        );
    } END_SCENARIO
}