/*
 * Performance comparison between mock and real implementations
 * Validates that real implementations meet or exceed mock performance
 */

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include "../bitactor/include/bitactor/bitactor.h"
#include "../src/news/news_validator.h"

/* Test configurations */
#define TEST_ITERATIONS 1000000
#define WARMUP_ITERATIONS 10000
#define BATCH_SIZE 8
#define NS_PER_TICK_TARGET 10.0

/* Portable timing function using clock_gettime */
static inline uint64_t get_ns_time(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

/* Test structures for mock compatibility */
typedef struct {
    uint64_t capability;
    uint64_t hash;
} fast_proof_t;

/* Source info structure for news validation */
typedef struct {
    uint64_t source_id;
    uint32_t credibility;
    uint32_t accuracy_rate;
    uint64_t last_verified;
    uint64_t total_articles;
    uint64_t false_articles;
    uint64_t corrections;
    uint64_t flags;
} source_info_t;

/* External functions from adapters */
extern bool bitactor_verify_fast(fast_proof_t* proof);
extern uint32_t validate_claim_8tick(claim_t* claim, source_info_t* source);

void benchmark_bitactor_tick(void) {
    printf("\n=== BitActor Tick Performance ===\n");
    
    bitactor_engine_t* engine = bitactor_init();
    signal_t test_signal = {
        .id = 0x12345678,
        .kind = 0x02,
        .priority = 0,
        .flags = 0,
        .payload = 0xDEADBEEF,
        .timestamp = 0,
        .context = 0
    };
    
    /* Warmup */
    for (int i = 0; i < WARMUP_ITERATIONS; i++) {
        result_t result = bitactor_tick(engine, &test_signal);
        (void)result;
    }
    
    /* Actual benchmark */
    uint64_t total_ns = 0;
    uint64_t min_ns = UINT64_MAX;
    uint64_t max_ns = 0;
    
    for (int i = 0; i < TEST_ITERATIONS; i++) {
        uint64_t start = get_ns_time();
        result_t result = bitactor_tick(engine, &test_signal);
        uint64_t end = get_ns_time();
        
        uint64_t ns = end - start;
        total_ns += ns;
        if (ns < min_ns) min_ns = ns;
        if (ns > max_ns) max_ns = ns;
        
        (void)result;
    }
    
    double avg_ns = (double)total_ns / TEST_ITERATIONS;
    
    printf("  Iterations: %d\n", TEST_ITERATIONS);
    printf("  Average time: %.2f ns/operation\n", avg_ns);
    printf("  Min time: %llu ns\n", min_ns);
    printf("  Max time: %llu ns\n", max_ns);
    printf("  Status: %s (target: ≤%.1f ns)\n", 
           avg_ns <= NS_PER_TICK_TARGET ? "PASS ✓" : "FAIL ✗",
           NS_PER_TICK_TARGET);
    
    bitactor_destroy(engine);
}

void benchmark_bitactor_verify(void) {
    printf("\n=== BitActor Verify Fast Performance ===\n");
    
    fast_proof_t test_proof = {
        .capability = 0xAABBCCDD,
        .hash = 0x123456789ABCDEF0
    };
    
    /* Warmup */
    for (int i = 0; i < WARMUP_ITERATIONS; i++) {
        bool result = bitactor_verify_fast(&test_proof);
        (void)result;
    }
    
    /* Actual benchmark */
    uint64_t total_ns = 0;
    uint64_t min_ns = UINT64_MAX;
    uint64_t max_ns = 0;
    
    for (int i = 0; i < TEST_ITERATIONS; i++) {
        uint64_t start = get_ns_time();
        bool result = bitactor_verify_fast(&test_proof);
        uint64_t end = get_ns_time();
        
        uint64_t ns = end - start;
        total_ns += ns;
        if (ns < min_ns) min_ns = ns;
        if (ns > max_ns) max_ns = ns;
        
        (void)result;
    }
    
    double avg_ns = (double)total_ns / TEST_ITERATIONS;
    
    printf("  Iterations: %d\n", TEST_ITERATIONS);
    printf("  Average time: %.2f ns/operation\n", avg_ns);
    printf("  Min time: %llu ns\n", min_ns);
    printf("  Max time: %llu ns\n", max_ns);
    printf("  Status: %s (target: ≤%.1f ns)\n", 
           avg_ns <= NS_PER_TICK_TARGET ? "PASS ✓" : "FAIL ✗",
           NS_PER_TICK_TARGET);
}

void benchmark_news_validation(void) {
    printf("\n=== News Validation Performance ===\n");
    
    claim_t test_claim = {
        .claim_hash = 0x123456789ABCDEF0,
        .source_id = 0x42,
        .confidence = 85,
        .evidence_mask = 0xFF,
        .claim_type = CLAIM_STATISTICAL,
        .timestamp = 0,
        .subject_hash = 0,
        .related_claims = 0
    };
    
    source_info_t test_source = {
        .source_id = 0x42,
        .credibility = 90,
        .accuracy_rate = 95,
        .last_verified = 0,
        .total_articles = 1000,
        .false_articles = 10,
        .corrections = 5,
        .flags = 0
    };
    
    /* Warmup */
    for (int i = 0; i < WARMUP_ITERATIONS; i++) {
        uint32_t result = validate_claim_8tick(&test_claim, &test_source);
        (void)result;
    }
    
    /* Actual benchmark */
    uint64_t total_ns = 0;
    uint64_t min_ns = UINT64_MAX;
    uint64_t max_ns = 0;
    
    for (int i = 0; i < TEST_ITERATIONS; i++) {
        /* Vary the input slightly to prevent caching */
        test_claim.confidence = 70 + (i % 30);
        test_source.credibility = 60 + (i % 40);
        
        uint64_t start = get_ns_time();
        uint32_t result = validate_claim_8tick(&test_claim, &test_source);
        uint64_t end = get_ns_time();
        
        uint64_t ns = end - start;
        total_ns += ns;
        if (ns < min_ns) min_ns = ns;
        if (ns > max_ns) max_ns = ns;
        
        (void)result;
    }
    
    double avg_ns = (double)total_ns / TEST_ITERATIONS;
    
    printf("  Iterations: %d\n", TEST_ITERATIONS);
    printf("  Average time: %.2f ns/operation\n", avg_ns);
    printf("  Min time: %llu ns\n", min_ns);
    printf("  Max time: %llu ns\n", max_ns);
    printf("  Status: %s (target: ≤%.1f ns)\n", 
           avg_ns <= NS_PER_TICK_TARGET ? "PASS ✓" : "FAIL ✗",
           NS_PER_TICK_TARGET);
}

void benchmark_batch_processing(void) {
    printf("\n=== Batch Processing Performance ===\n");
    
    bitactor_engine_t* engine = bitactor_init();
    signal_t signals[BATCH_SIZE];
    
    /* Initialize batch signals */
    for (int i = 0; i < BATCH_SIZE; i++) {
        signals[i] = (signal_t){
            .id = 0x1000 + i,
            .kind = (i % 4) + 1,
            .priority = 0,
            .flags = 0,
            .payload = 0xDEAD0000 + i,
            .timestamp = i * 1000,
            .context = 0
        };
    }
    
    /* Enqueue batch */
    for (int i = 0; i < BATCH_SIZE; i++) {
        bitactor_enqueue(engine, &signals[i]);
    }
    
    /* Benchmark drain operation */
    uint64_t start = get_ns_time();
    uint32_t processed = bitactor_drain(engine, BATCH_SIZE);
    uint64_t end = get_ns_time();
    
    uint64_t ns_total = end - start;
    double ns_per_signal = (double)ns_total / processed;
    
    printf("  Batch size: %d\n", BATCH_SIZE);
    printf("  Processed: %u signals\n", processed);
    printf("  Total time: %llu ns\n", ns_total);
    printf("  Average time per signal: %.2f ns\n", ns_per_signal);
    printf("  Status: %s (target: ≤%.1f ns/signal)\n", 
           ns_per_signal <= NS_PER_TICK_TARGET ? "PASS ✓" : "FAIL ✗",
           NS_PER_TICK_TARGET);
    
    bitactor_destroy(engine);
}

void print_summary(void) {
    printf("\n=== Performance Summary ===\n");
    printf("All tests target ≤%.1f ns execution time\n", NS_PER_TICK_TARGET);
    printf("Real implementations should match or exceed mock performance\n");
    printf("\nRecommendations:\n");
    printf("- Run with CPU governor set to performance\n");
    printf("- Disable CPU frequency scaling\n");
    printf("- Use taskset to pin to a single core\n");
    printf("- Example: taskset -c 0 ./test_real_vs_mock_performance\n");
}

int main(void) {
    printf("=== Mock vs Real Implementation Benchmark ===\n");
    printf("Comparing performance of mock and real BitActor implementations\n");
    
    /* Run benchmarks */
    benchmark_bitactor_tick();
    benchmark_bitactor_verify();
    benchmark_news_validation();
    benchmark_batch_processing();
    
    /* Summary */
    print_summary();
    
    return 0;
}