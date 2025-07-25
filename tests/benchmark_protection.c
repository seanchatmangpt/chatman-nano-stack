#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>
#include "../src/protection/core_protection.h"

#define BENCHMARK_ITERATIONS 1000000
#define WARMUP_ITERATIONS 10000

// Benchmark results structure
typedef struct {
    uint64_t min_time_ns;
    uint64_t max_time_ns;
    double avg_time_ns;
    double p50_time_ns;
    double p95_time_ns;
    double p99_time_ns;
    uint32_t violations_prevented;
    uint32_t iterations;
} benchmark_results_t;

// High-resolution timer
static uint64_t get_ns_timestamp(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

// Compare function for qsort
int compare_uint64(const void *a, const void *b) {
    uint64_t val_a = *(uint64_t*)a;
    uint64_t val_b = *(uint64_t*)b;
    return (val_a > val_b) - (val_a < val_b);
}

// Calculate percentile
double calculate_percentile(uint64_t* sorted_times, int count, double percentile) {
    int index = (int)(percentile * count / 100.0);
    return sorted_times[index];
}

// Benchmark individual protection checks
void benchmark_position_sizing(benchmark_results_t* results) {
    printf("\n📊 Benchmarking Position Size Validation...\n");
    
    core_protection_t* protection = malloc(sizeof(core_protection_t));
    protection->max_position_risk_percent = 0.01;
    protection->current_exposure = 0;
    
    uint64_t* times = malloc(BENCHMARK_ITERATIONS * sizeof(uint64_t));
    uint64_t total_time = 0;
    
    // Warmup
    for (int i = 0; i < WARMUP_ITERATIONS; i++) {
        trade_request_t trade = {
            .symbol = "EURUSD",
            .position_size = 500 + (i % 1000),
            .entry_price = 1.1000,
            .stop_loss = 1.0890,
            .account_balance = 1000,
            .timestamp = 0
        };
        check_position_size_limit(protection, &trade);
    }
    
    // Actual benchmark
    for (int i = 0; i < BENCHMARK_ITERATIONS; i++) {
        trade_request_t trade = {
            .symbol = "EURUSD",
            .position_size = 500 + (i % 1000),
            .entry_price = 1.1000,
            .stop_loss = 1.0890,
            .account_balance = 1000,
            .timestamp = 0
        };
        
        uint64_t start = get_ns_timestamp();
        check_position_size_limit(protection, &trade);
        uint64_t end = get_ns_timestamp();
        
        times[i] = end - start;
        total_time += times[i];
    }
    
    // Sort times for percentile calculation
    qsort(times, BENCHMARK_ITERATIONS, sizeof(uint64_t), compare_uint64);
    
    results->min_time_ns = times[0];
    results->max_time_ns = times[BENCHMARK_ITERATIONS - 1];
    results->avg_time_ns = (double)total_time / BENCHMARK_ITERATIONS;
    results->p50_time_ns = calculate_percentile(times, BENCHMARK_ITERATIONS, 50);
    results->p95_time_ns = calculate_percentile(times, BENCHMARK_ITERATIONS, 95);
    results->p99_time_ns = calculate_percentile(times, BENCHMARK_ITERATIONS, 99);
    results->iterations = BENCHMARK_ITERATIONS;
    
    printf("✅ Min: %lu ns\n", results->min_time_ns);
    printf("✅ Avg: %.2f ns\n", results->avg_time_ns);
    printf("✅ P50: %.2f ns\n", results->p50_time_ns);
    printf("✅ P95: %.2f ns\n", results->p95_time_ns);
    printf("✅ P99: %.2f ns\n", results->p99_time_ns);
    printf("✅ Max: %lu ns\n", results->max_time_ns);
    
    free(times);
    free(protection);
}

// Benchmark full validation flow
void benchmark_full_validation(benchmark_results_t* results) {
    printf("\n📊 Benchmarking Full Trade Validation...\n");
    
    core_protection_t* protection = malloc(sizeof(core_protection_t));
    protection->max_position_risk_percent = 0.01;
    protection->daily_loss_limit_percent = 0.02;
    protection->current_exposure = 0;
    protection->daily_pnl = 0;
    protection->trading_halted = false;
    protection->require_stop_loss = true;
    protection->default_stop_percent = 0.02;
    protection->kill_switch_enabled = false;
    protection->kill_switch_timestamp = 0;
    protection->max_response_time_ms = 100;
    
    uint64_t* times = malloc(BENCHMARK_ITERATIONS * sizeof(uint64_t));
    uint64_t total_time = 0;
    
    // Warmup
    for (int i = 0; i < WARMUP_ITERATIONS; i++) {
        trade_request_t trade = {
            .symbol = "EURUSD",
            .position_size = 500,
            .entry_price = 1.1000,
            .stop_loss = 1.0890,
            .account_balance = 1000,
            .timestamp = 0
        };
        validate_trade_protection(protection, &trade);
    }
    
    // Actual benchmark with varied scenarios
    for (int i = 0; i < BENCHMARK_ITERATIONS; i++) {
        trade_request_t trade = {
            .symbol = "EURUSD",
            .position_size = 500 + (i % 500),
            .entry_price = 1.1000,
            .stop_loss = (i % 10 == 0) ? 0 : 1.0890,  // 10% have no stop
            .account_balance = 1000,
            .timestamp = 0
        };
        
        uint64_t start = get_ns_timestamp();
        protection_result_t result = validate_trade_protection(protection, &trade);
        uint64_t end = get_ns_timestamp();
        
        times[i] = end - start;
        total_time += times[i];
        
        if (!result.approved) {
            results->violations_prevented++;
        }
    }
    
    // Sort times for percentile calculation
    qsort(times, BENCHMARK_ITERATIONS, sizeof(uint64_t), compare_uint64);
    
    results->min_time_ns = times[0];
    results->max_time_ns = times[BENCHMARK_ITERATIONS - 1];
    results->avg_time_ns = (double)total_time / BENCHMARK_ITERATIONS;
    results->p50_time_ns = calculate_percentile(times, BENCHMARK_ITERATIONS, 50);
    results->p95_time_ns = calculate_percentile(times, BENCHMARK_ITERATIONS, 95);
    results->p99_time_ns = calculate_percentile(times, BENCHMARK_ITERATIONS, 99);
    results->iterations = BENCHMARK_ITERATIONS;
    
    printf("✅ Min: %lu ns\n", results->min_time_ns);
    printf("✅ Avg: %.2f ns\n", results->avg_time_ns);
    printf("✅ P50: %.2f ns\n", results->p50_time_ns);
    printf("✅ P95: %.2f ns\n", results->p95_time_ns);
    printf("✅ P99: %.2f ns\n", results->p99_time_ns);
    printf("✅ Max: %lu ns\n", results->max_time_ns);
    printf("✅ Violations prevented: %u\n", results->violations_prevented);
    
    free(times);
    free(protection);
}

// Benchmark kill switch response time
void benchmark_kill_switch(benchmark_results_t* results) {
    printf("\n📊 Benchmarking Emergency Kill Switch...\n");
    
    core_protection_t* protection = malloc(sizeof(core_protection_t));
    memset(protection, 0, sizeof(core_protection_t));
    
    uint64_t* times = malloc(BENCHMARK_ITERATIONS * sizeof(uint64_t));
    uint64_t total_time = 0;
    
    for (int i = 0; i < BENCHMARK_ITERATIONS; i++) {
        // Reset state
        protection->kill_switch_enabled = false;
        protection->trading_halted = false;
        
        uint64_t start = get_ns_timestamp();
        activate_kill_switch(protection);
        uint64_t end = get_ns_timestamp();
        
        times[i] = end - start;
        total_time += times[i];
    }
    
    // Sort times for percentile calculation
    qsort(times, BENCHMARK_ITERATIONS, sizeof(uint64_t), compare_uint64);
    
    results->min_time_ns = times[0];
    results->max_time_ns = times[BENCHMARK_ITERATIONS - 1];
    results->avg_time_ns = (double)total_time / BENCHMARK_ITERATIONS;
    results->p50_time_ns = calculate_percentile(times, BENCHMARK_ITERATIONS, 50);
    results->p95_time_ns = calculate_percentile(times, BENCHMARK_ITERATIONS, 95);
    results->p99_time_ns = calculate_percentile(times, BENCHMARK_ITERATIONS, 99);
    
    printf("✅ Min: %lu ns\n", results->min_time_ns);
    printf("✅ Avg: %.2f ns\n", results->avg_time_ns);
    printf("✅ P50: %.2f ns\n", results->p50_time_ns);
    printf("✅ P95: %.2f ns\n", results->p95_time_ns);
    printf("✅ P99: %.2f ns\n", results->p99_time_ns);
    printf("✅ Max: %lu ns\n", results->max_time_ns);
    
    free(times);
    free(protection);
}

// Verify 100ms response time requirement
void verify_response_time_requirement() {
    printf("\n🎯 Verifying 100ms Response Time Requirement...\n");
    
    // Get actual metrics from the protection system
    protection_metrics_t* metrics = get_protection_metrics();
    
    if (metrics->response_time_us < 100000) {  // 100ms = 100,000 μs
        printf("✅ PASS: Response time %u μs < 100,000 μs (100ms)\n", 
               metrics->response_time_us);
    } else {
        printf("❌ FAIL: Response time %u μs exceeds 100ms requirement!\n", 
               metrics->response_time_us);
        exit(1);
    }
}

// Main benchmark runner
int main(void) {
    printf("=== 80/20 Core Protection Performance Benchmarks ===\n");
    printf("Iterations: %d\n", BENCHMARK_ITERATIONS);
    
    benchmark_results_t position_results = {0};
    benchmark_results_t validation_results = {0};
    benchmark_results_t killswitch_results = {0};
    
    // Run benchmarks
    benchmark_position_sizing(&position_results);
    benchmark_full_validation(&validation_results);
    benchmark_kill_switch(&killswitch_results);
    
    // Summary
    printf("\n📈 BENCHMARK SUMMARY\n");
    printf("==================\n");
    
    printf("\nPosition Sizing Check:\n");
    printf("  Average: %.2f ns (%.2f μs)\n", 
           position_results.avg_time_ns, position_results.avg_time_ns / 1000.0);
    
    printf("\nFull Trade Validation:\n");
    printf("  Average: %.2f ns (%.2f μs)\n", 
           validation_results.avg_time_ns, validation_results.avg_time_ns / 1000.0);
    printf("  P99: %.2f ns (%.2f μs)\n", 
           validation_results.p99_time_ns, validation_results.p99_time_ns / 1000.0);
    
    printf("\nKill Switch Activation:\n");
    printf("  Average: %.2f ns (%.2f μs)\n", 
           killswitch_results.avg_time_ns, killswitch_results.avg_time_ns / 1000.0);
    
    // Verify 100ms requirement
    verify_response_time_requirement();
    
    printf("\n✅ All benchmarks completed successfully!\n");
    printf("✅ System meets 100ms response time requirement\n");
    
    return 0;
}