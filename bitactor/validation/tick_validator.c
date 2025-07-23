/*
 * BitActor 8-Tick Deterministic Execution Validator
 * Comprehensive validation suite for tick budget compliance
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include <assert.h>

#include "../include/bitactor/bitactor.h"
#include "../runtime/bytecode_loader.h"
#include "../integration/cns_integration.h"

// Validation configuration
#define VALIDATION_SAMPLES        10000
#define STRESS_TEST_DURATION      60      // seconds
#define MAX_CONCURRENT_SIGNALS    1000
#define TICK_BUDGET_THRESHOLD     8
#define DETERMINISM_TOLERANCE     0.001   // 0.1% variance allowed
#define PERFORMANCE_BASELINE      500000  // 500K signals/sec baseline

// Test signal patterns
typedef enum {
    PATTERN_SEQUENTIAL,     // Sequential signal IDs
    PATTERN_RANDOM,         // Random signal data
    PATTERN_WORST_CASE,     // Pathological cases
    PATTERN_REAL_WORLD,     // Realistic market data patterns
    PATTERN_ADVERSARIAL     // Designed to break timing
} test_pattern_t;

// Validation statistics
typedef struct {
    uint64_t total_executions;
    uint64_t tick_violations;
    uint64_t determinism_failures;
    uint64_t performance_failures;
    
    // Timing statistics
    uint32_t tick_min;
    uint32_t tick_max;
    double tick_mean;
    double tick_stddev;
    
    // Performance statistics
    double throughput_min;
    double throughput_max;
    double throughput_mean;
    
    // Determinism statistics
    uint32_t hash_collisions;
    uint32_t execution_variants;
    
    bool overall_pass;
} validation_stats_t;

// Forward declarations
static int validate_tick_budget(void);
static int validate_determinism(void);
static int validate_performance(void);
static int validate_stress_conditions(void);
static void generate_test_signal(signal_t* signal, test_pattern_t pattern, uint32_t index);
static double calculate_throughput(uint64_t signals, uint64_t nanoseconds);
static void print_validation_report(const validation_stats_t* stats);

/**
 * Main validation entry point
 */
int bitactor_validate_8tick_guarantee(void) {
    printf("BitActor 8-Tick Deterministic Execution Validation\n");
    printf("==================================================\n\n");
    
    // Initialize CNS BitActor integration
    if (cns_bitactor_init() != 0) {
        printf("ERROR: Failed to initialize CNS BitActor integration\n");
        return -1;
    }
    
    // Load test ontology
    if (cns_bitactor_load_ontology("test/fixtures/test_ontology.ttl", 
                                   "test/fixtures/test_shapes.shacl") != 0) {
        printf("WARNING: Could not load test ontology, using built-in handlers\n");
    }
    
    validation_stats_t stats = {0};
    stats.tick_min = UINT32_MAX;
    stats.throughput_min = INFINITY;
    
    int result = 0;
    
    // Test 1: Tick Budget Compliance
    printf("Test 1: Tick Budget Compliance (≤8 ticks)\n");
    printf("-----------------------------------------\n");
    if (validate_tick_budget() == 0) {
        printf("✓ PASSED\n\n");
    } else {
        printf("✗ FAILED\n\n");
        result = -1;
    }
    
    // Test 2: Deterministic Execution
    printf("Test 2: Deterministic Execution Validation\n");
    printf("------------------------------------------\n");
    if (validate_determinism() == 0) {
        printf("✓ PASSED\n\n");
    } else {
        printf("✗ FAILED\n\n");
        result = -1;
    }
    
    // Test 3: Performance Validation
    printf("Test 3: Performance Validation (≥500K signals/sec)\n");
    printf("--------------------------------------------------\n");
    if (validate_performance() == 0) {
        printf("✓ PASSED\n\n");
    } else {
        printf("✗ FAILED\n\n");
        result = -1;
    }
    
    // Test 4: Stress Testing
    printf("Test 4: Stress Test Validation\n");
    printf("------------------------------\n");
    if (validate_stress_conditions() == 0) {
        printf("✓ PASSED\n\n");
    } else {
        printf("✗ FAILED\n\n");
        result = -1;
    }
    
    // Generate final report
    cns_bitactor_performance_report(stdout);
    
    // Cleanup
    cns_bitactor_cleanup();
    
    if (result == 0) {
        printf("🎉 ALL VALIDATION TESTS PASSED\n");
        printf("BitActor system is validated for 8-tick deterministic execution\n");
    } else {
        printf("❌ VALIDATION FAILED\n");
        printf("BitActor system does not meet 8-tick guarantee requirements\n");
    }
    
    return result;
}

/**
 * Validate tick budget compliance across all signal patterns
 */
static int validate_tick_budget(void) {
    uint32_t violations = 0;
    uint64_t total_ticks = 0;
    uint32_t min_ticks = UINT32_MAX;
    uint32_t max_ticks = 0;
    
    // Test all signal patterns
    test_pattern_t patterns[] = {
        PATTERN_SEQUENTIAL,
        PATTERN_RANDOM,
        PATTERN_WORST_CASE,
        PATTERN_REAL_WORLD,
        PATTERN_ADVERSARIAL
    };
    
    for (int p = 0; p < 5; p++) {
        printf("  Testing pattern %d/%d: ", p + 1, 5);
        
        uint32_t pattern_violations = 0;
        
        for (uint32_t i = 0; i < VALIDATION_SAMPLES / 5; i++) {
            signal_t test_signal;
            generate_test_signal(&test_signal, patterns[p], i);
            
            result_t result = cns_bitactor_process_signal(&test_signal);
            
            if (result.ticks > TICK_BUDGET_THRESHOLD) {
                violations++;
                pattern_violations++;
            }
            
            total_ticks += result.ticks;
            if (result.ticks < min_ticks) min_ticks = result.ticks;
            if (result.ticks > max_ticks) max_ticks = result.ticks;
        }
        
        printf("%d violations\n", pattern_violations);
    }
    
    double avg_ticks = (double)total_ticks / VALIDATION_SAMPLES;
    double violation_rate = (double)violations * 100.0 / VALIDATION_SAMPLES;
    
    printf("\n  Statistics:\n");
    printf("    Samples:        %d\n", VALIDATION_SAMPLES);
    printf("    Violations:     %d (%.3f%%)\n", violations, violation_rate);
    printf("    Ticks (min/avg/max): %d/%.2f/%d\n", min_ticks, avg_ticks, max_ticks);
    printf("    Budget threshold: %d ticks\n", TICK_BUDGET_THRESHOLD);
    
    return (violations == 0) ? 0 : -1;
}

/**
 * Validate deterministic execution (same input -> same output)
 */
static int validate_determinism(void) {
    uint32_t determinism_failures = 0;
    uint32_t test_cases = 1000;
    
    printf("  Running %d determinism test cases...\n", test_cases);
    
    for (uint32_t i = 0; i < test_cases; i++) {
        signal_t test_signal;
        generate_test_signal(&test_signal, PATTERN_SEQUENTIAL, i);
        
        // Execute same signal multiple times
        result_t results[5];
        for (int run = 0; run < 5; run++) {
            results[run] = cns_bitactor_process_signal(&test_signal);
        }
        
        // Check for consistency
        bool consistent = true;
        for (int run = 1; run < 5; run++) {
            if (results[run].result != results[0].result ||
                results[run].exec_hash != results[0].exec_hash ||
                results[run].ticks != results[0].ticks) {
                consistent = false;
                break;
            }
        }
        
        if (!consistent) {
            determinism_failures++;
            if (determinism_failures <= 5) {  // Show first 5 failures
                printf("    DETERMINISM FAILURE #%d: signal_id=%d\n", 
                       determinism_failures, test_signal.id);
                for (int run = 0; run < 5; run++) {
                    printf("      Run %d: result=0x%08lX, hash=0x%08X, ticks=%d\n",
                           run, results[run].result, results[run].exec_hash, results[run].ticks);
                }
            }
        }
    }
    
    double failure_rate = (double)determinism_failures * 100.0 / test_cases;
    
    printf("  Statistics:\n");
    printf("    Test cases:     %d\n", test_cases);
    printf("    Failures:       %d (%.3f%%)\n", determinism_failures, failure_rate);
    printf("    Tolerance:      %.3f%%\n", DETERMINISM_TOLERANCE * 100.0);
    
    return (failure_rate <= DETERMINISM_TOLERANCE * 100.0) ? 0 : -1;
}

/**
 * Validate performance requirements (≥500K signals/sec)
 */
static int validate_performance(void) {
    uint32_t warmup_signals = 10000;
    uint32_t test_signals = 100000;
    
    printf("  Warming up with %d signals...\n", warmup_signals);
    
    // Warmup phase
    for (uint32_t i = 0; i < warmup_signals; i++) {
        signal_t test_signal;
        generate_test_signal(&test_signal, PATTERN_REAL_WORLD, i);
        cns_bitactor_process_signal(&test_signal);
    }
    
    printf("  Performance test with %d signals...\n", test_signals);
    
    // Performance measurement
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    for (uint32_t i = 0; i < test_signals; i++) {
        signal_t test_signal;
        generate_test_signal(&test_signal, PATTERN_REAL_WORLD, i);
        cns_bitactor_process_signal(&test_signal);
    }
    
    clock_gettime(CLOCK_MONOTONIC, &end);
    
    // Calculate throughput
    uint64_t elapsed_ns = (end.tv_sec - start.tv_sec) * 1000000000ULL + 
                         (end.tv_nsec - start.tv_nsec);
    double throughput = calculate_throughput(test_signals, elapsed_ns);
    
    printf("  Statistics:\n");
    printf("    Signals processed: %d\n", test_signals);
    printf("    Elapsed time:      %.3f ms\n", elapsed_ns / 1000000.0);
    printf("    Throughput:        %.0f signals/sec\n", throughput);
    printf("    Baseline:          %d signals/sec\n", PERFORMANCE_BASELINE);
    
    return (throughput >= PERFORMANCE_BASELINE) ? 0 : -1;
}

/**
 * Validate under stress conditions
 */
static int validate_stress_conditions(void) {
    printf("  Running %d second stress test...\n", STRESS_TEST_DURATION);
    
    uint64_t total_signals = 0;
    uint32_t tick_violations = 0;
    uint32_t error_count = 0;
    
    struct timespec start, current;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    while (1) {
        clock_gettime(CLOCK_MONOTONIC, &current);
        uint64_t elapsed_seconds = current.tv_sec - start.tv_sec;
        
        if (elapsed_seconds >= STRESS_TEST_DURATION) {
            break;
        }
        
        // Generate burst of concurrent signals
        for (int burst = 0; burst < 100; burst++) {
            signal_t test_signal;
            generate_test_signal(&test_signal, PATTERN_ADVERSARIAL, 
                               total_signals + burst);
            
            result_t result = cns_bitactor_process_signal(&test_signal);
            
            if (result.status != BITACTOR_OK) {
                error_count++;
            }
            
            if (result.ticks > TICK_BUDGET_THRESHOLD) {
                tick_violations++;
            }
        }
        
        total_signals += 100;
        
        // Progress update every 10 seconds
        if (elapsed_seconds % 10 == 0 && elapsed_seconds > 0) {
            printf("    %ld seconds: %lu signals processed, %d violations\n",
                   elapsed_seconds, total_signals, tick_violations);
        }
    }
    
    double violation_rate = (double)tick_violations * 100.0 / total_signals;
    double error_rate = (double)error_count * 100.0 / total_signals;
    
    printf("  Statistics:\n");
    printf("    Duration:         %d seconds\n", STRESS_TEST_DURATION);
    printf("    Total signals:    %lu\n", total_signals);
    printf("    Tick violations:  %d (%.3f%%)\n", tick_violations, violation_rate);
    printf("    Errors:           %d (%.3f%%)\n", error_count, error_rate);
    
    return (tick_violations == 0 && error_count == 0) ? 0 : -1;
}

/**
 * Generate test signal with specified pattern
 */
static void generate_test_signal(signal_t* signal, test_pattern_t pattern, uint32_t index) {
    memset(signal, 0, sizeof(signal_t));
    signal->id = index;
    signal->kind = 0x01;  // Default signal kind
    signal->priority = 128;
    signal->timestamp = __rdtsc();
    
    switch (pattern) {
        case PATTERN_SEQUENTIAL:
            signal->payload = index;
            signal->context = index * 2;
            break;
            
        case PATTERN_RANDOM:
            signal->payload = ((uint64_t)rand() << 32) | rand();
            signal->context = ((uint64_t)rand() << 32) | rand();
            break;
            
        case PATTERN_WORST_CASE:
            // Pathological cases designed to stress constraints
            signal->payload = UINT64_MAX;
            signal->context = 0;
            signal->flags = 0xFFFF;
            break;
            
        case PATTERN_REAL_WORLD:
            // Simulate realistic market data patterns
            signal->payload = 100000 + (index % 50000);  // Price-like values
            signal->context = (index % 1000) * 100;      // Volume-like values
            signal->flags = index % 16;                  // Various flags
            break;
            
        case PATTERN_ADVERSARIAL:
            // Designed to trigger edge cases and timing variations
            signal->payload = (index % 2) ? 0 : UINT64_MAX;
            signal->context = (index % 3) == 0 ? 0 : UINT64_MAX;
            signal->flags = (1 << (index % 16));
            break;
    }
}

/**
 * Calculate throughput in signals per second
 */
static double calculate_throughput(uint64_t signals, uint64_t nanoseconds) {
    if (nanoseconds == 0) return 0.0;
    return (double)signals * 1000000000.0 / nanoseconds;
}

/**
 * CLI entry point for validation
 */
int main(int argc, char* argv[]) {
    printf("BitActor 8-Tick Validation Suite\n");
    printf("Version 1.0 - Artificial Hyper Intelligence Implementation\n\n");
    
    // Seed random number generator
    srand((unsigned int)time(NULL));
    
    int result = bitactor_validate_8tick_guarantee();
    
    if (result == 0) {
        printf("\n🎯 VALIDATION COMPLETE: BitActor meets 8-tick deterministic execution guarantee\n");
        return 0;
    } else {
        printf("\n⚠️  VALIDATION FAILED: BitActor does not meet requirements\n");
        return 1;
    }
}