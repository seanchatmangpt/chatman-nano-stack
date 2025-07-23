#include "test_harness.h"
#include "../src/bitfiber_integration.c"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// Test data
static bitactor_t test_ba;
static fiber_scheduler_t test_sched;

// Mock implementations for testing
uint32_t validate_news_article(const claim_t* claims, uint32_t claim_count) {
    (void)claims; (void)claim_count;
    return 0x01; // STATUS_VERIFIED
}

uint32_t check_source_credibility(uint64_t source_id) {
    return (uint32_t)((source_id * 0x123456789ABCDEF0ULL) >> 56) % 100;
}

void init_fact_database(const char* db_path) {
    (void)db_path;
}

void process_fact_stream(const claim_t* new_facts, uint32_t count) {
    (void)new_facts; (void)count;
}

// Platform-compatible cycle counter
static inline uint64_t __rdtsc(void) {
#ifdef __aarch64__
    uint64_t val;
    __asm__ __volatile__ ("mrs %0, cntvct_el0" : "=r" (val));
    return val;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#endif
}

// Test: Integrated system initialization
bool test_integrated_system_initialization(char* error_msg) {
    bitactor_init_integrated_system(&test_ba, &test_sched);
    
    // Verify BitActor initialization
    TEST_ASSERT(test_ba.signal_head == 0, "Signal head should be initialized");
    TEST_ASSERT(test_ba.signal_tail == 0, "Signal tail should be initialized");
    TEST_ASSERT(test_ba.tick_count == 0, "Tick count should be initialized");
    
    // Verify integration flags
    TEST_ASSERT(test_ba.flags & 0x40000000, "Fiber integration should be enabled");
    TEST_ASSERT(test_ba.flags & 0x20000000, "News validation should be enabled");
    TEST_ASSERT(test_ba.flags & 0x10000000, "Tick optimization should be enabled");
    
    // Verify dispatch handlers are registered
    TEST_ASSERT(test_ba.dispatch[0x1001 & (BITACTOR_DISPATCH_SIZE - 1)] != NULL, "News handler 1 should be registered");
    TEST_ASSERT(test_ba.dispatch[0x1002 & (BITACTOR_DISPATCH_SIZE - 1)] != NULL, "News handler 2 should be registered");
    TEST_ASSERT(test_ba.dispatch[0x1003 & (BITACTOR_DISPATCH_SIZE - 1)] != NULL, "News handler 3 should be registered");
    
    return true;
}

// Test: Simple news validation with integrated optimization
bool test_simple_news_validation_optimization(char* error_msg) {
    bitactor_init_integrated_system(&test_ba, &test_sched);
    
    // Create simple news validation signal (complexity <= 2)
    signal_t news_signal = {
        .kind = 0x1001,
        .flags = 0x02,  // Low complexity
        .timestamp = 0x123456789ABCDEF0ULL,
        .payload = 0xDEADBEEF
    };
    
    // Enqueue signal
    bool enqueued = bitactor_enqueue_signal(&test_ba, &news_signal);
    TEST_ASSERT(enqueued, "Simple news signal should be enqueued");
    
    // Process with integrated optimization
    uint64_t start_ticks = __rdtsc();
    bitactor_tick_with_integrated_optimization(&test_ba, &test_sched);
    uint64_t end_ticks = __rdtsc();
    
    // Verify processing
    TEST_ASSERT_EQ(test_ba.signal_count, 1, "Signal should be processed");
    TEST_ASSERT(test_ba.tick_count == 1, "Tick count should be incremented");
    
    // Verify performance (should be fast for simple validation)
    uint64_t execution_ticks = end_ticks - start_ticks;
    TEST_ASSERT_LT(execution_ticks, 5000, "Simple validation should be fast");
    
    // Check result in scratch memory
    uint32_t result = *(uint32_t*)test_ba.scratch;
    TEST_ASSERT(result > 0, "Validation should produce a result");
    TEST_ASSERT((result & 0x40000000) == 0, "Simple validation should not use fibers");
    
    return true;
}

// Test: Complex news validation with fiber cooperation
bool test_complex_news_validation_with_fibers(char* error_msg) {
    bitactor_init_integrated_system(&test_ba, &test_sched);
    
    // Create complex news validation signal (complexity > 2)
    signal_t complex_signal = {
        .kind = 0x1002,
        .flags = 0x05,  // High complexity (bits 0-3)
        .timestamp = 0x123456789ABCDEF0ULL,
        .payload = 0xCAFEBABE
    };
    
    // Enqueue signal
    bool enqueued = bitactor_enqueue_signal(&test_ba, &complex_signal);
    TEST_ASSERT(enqueued, "Complex news signal should be enqueued");
    
    // Process with integrated optimization
    bitactor_tick_with_integrated_optimization(&test_ba, &test_sched);
    
    // Verify processing
    TEST_ASSERT_EQ(test_ba.signal_count, 1, "Signal should be processed");
    
    // Check fiber creation in scratch memory
    uint32_t result = *(uint32_t*)test_ba.scratch;
    
    // For high-credibility sources, should create fiber
    uint32_t source_credibility = check_source_credibility(0x123456789ABCDEF0ULL >> 32);
    if (source_credibility >= 30) {
        TEST_ASSERT(result & 0x40000000, "Complex validation should use fibers for high-credibility sources");
        uint32_t fiber_id = result & 0x3FFFFFFF;
        TEST_ASSERT(fiber_id > 0, "Valid fiber ID should be created");
    } else {
        TEST_ASSERT(result & 0x80000000, "Low-credibility sources should be rejected immediately");
    }
    
    return true;
}

// Test: Batch news validation optimization
bool test_batch_news_validation_optimization(char* error_msg) {
    bitactor_init_integrated_system(&test_ba, &test_sched);
    
    // Create batch of news validation signals
    signal_t batch_signals[4] = {
        {.kind = 0x1001, .flags = 0x02, .timestamp = 0x1000000000000000ULL, .payload = 0x1001},
        {.kind = 0x1001, .flags = 0x02, .timestamp = 0x2000000000000000ULL, .payload = 0x1002},
        {.kind = 0x1001, .flags = 0x03, .timestamp = 0x3000000000000000ULL, .payload = 0x1003},
        {.kind = 0x1001, .flags = 0x02, .timestamp = 0x4000000000000000ULL, .payload = 0x1004}
    };
    
    // Enqueue all signals
    for (int i = 0; i < 4; i++) {
        bool enqueued = bitactor_enqueue_signal(&test_ba, &batch_signals[i]);
        TEST_ASSERT(enqueued, "Batch signal should be enqueued");
    }
    
    // Process batch with integrated optimization
    uint64_t start_ticks = __rdtsc();
    bitactor_tick_with_integrated_optimization(&test_ba, &test_sched);
    uint64_t end_ticks = __rdtsc();
    
    // Verify batch processing
    TEST_ASSERT_EQ(test_ba.signal_count, 4, "All batch signals should be processed");
    TEST_ASSERT(test_ba.tick_count == 1, "Batch should be processed in single tick");
    
    // Verify performance benefit of batch processing
    uint64_t batch_execution_ticks = end_ticks - start_ticks;
    TEST_ASSERT_LT(batch_execution_ticks, 10000, "Batch processing should be efficient");
    
    // Check batch results in scratch memory
    uint32_t* results = (uint32_t*)test_ba.scratch;
    for (int i = 0; i < 4; i++) {
        TEST_ASSERT(results[i] > 0, "Each batch result should be valid");
    }
    
    return true;
}

// Test: Mixed signal processing with optimization
bool test_mixed_signal_processing_optimization(char* error_msg) {
    bitactor_init_integrated_system(&test_ba, &test_sched);
    
    // Create mixed signals (news and non-news)
    signal_t mixed_signals[5] = {
        {.kind = 0x1001, .flags = 0x02, .timestamp = __rdtsc(), .payload = 0x1001}, // News
        {.kind = 0x1002, .flags = 0x02, .timestamp = __rdtsc(), .payload = 0x1002}, // News  
        {.kind = 0x2001, .flags = 0x01, .timestamp = __rdtsc(), .payload = 0x2001}, // Non-news
        {.kind = 0x1003, .flags = 0x05, .timestamp = __rdtsc(), .payload = 0x1003}, // Complex news
        {.kind = 0x3001, .flags = 0x01, .timestamp = __rdtsc(), .payload = 0x3001}  // Non-news
    };
    
    // Enqueue mixed signals
    for (int i = 0; i < 5; i++) {
        bool enqueued = bitactor_enqueue_signal(&test_ba, &mixed_signals[i]);
        TEST_ASSERT(enqueued, "Mixed signal should be enqueued");
    }
    
    // Process with integrated optimization - should handle batch and individual signals
    uint64_t total_ticks = 0;
    while (!bitactor_ring_empty(&test_ba)) {
        uint64_t start = __rdtsc();
        bitactor_tick_with_integrated_optimization(&test_ba, &test_sched);
        uint64_t end = __rdtsc();
        total_ticks += (end - start);
    }
    
    // Verify all signals processed
    TEST_ASSERT_EQ(test_ba.signal_count, 5, "All mixed signals should be processed");
    TEST_ASSERT(test_ba.tick_count >= 2, "Should require multiple ticks for mixed signals");
    TEST_ASSERT_LT(total_ticks, 20000, "Mixed processing should be efficient");
    
    return true;
}

// Test: Performance metrics collection
bool test_performance_metrics_collection(char* error_msg) {
    bitactor_init_integrated_system(&test_ba, &test_sched);
    
    // Process some signals to generate metrics
    for (int i = 0; i < 10; i++) {
        signal_t sig = {
            .kind = 0x1001 + (i % 3),
            .flags = 0x02,
            .timestamp = __rdtsc(),
            .payload = 0x1000 + i
        };
        bitactor_enqueue_signal(&test_ba, &sig);
        bitactor_tick_with_integrated_optimization(&test_ba, &test_sched);
    }
    
    // Collect comprehensive metrics
    uint64_t total_ticks, total_cycles, news_validations;
    uint32_t active_fibers;
    
    bitactor_get_integrated_metrics(&test_ba, &test_sched,
                                  &total_ticks, &total_cycles,
                                  &active_fibers, &news_validations);
    
    // Verify metrics are reasonable
    TEST_ASSERT(total_ticks >= 10, "Should have processed at least 10 ticks");
    TEST_ASSERT(total_cycles > 0, "Should have consumed cycles");
    TEST_ASSERT_EQ(news_validations, 10, "Should have processed 10 news validations");
    
    return true;
}

// Test: System stress test with high load
bool test_system_stress_with_high_load(char* error_msg) {
    bitactor_init_integrated_system(&test_ba, &test_sched);
    
    // Create high load scenario
    const int STRESS_SIGNAL_COUNT = 1000;
    uint64_t start_stress = __rdtsc();
    
    for (int i = 0; i < STRESS_SIGNAL_COUNT; i++) {
        signal_t sig = {
            .kind = 0x1001 + (i % 3),
            .flags = (i % 4) + 1,  // Varying complexity
            .timestamp = __rdtsc(),
            .payload = 0x10000 + i
        };
        
        // Enqueue if space available
        if (bitactor_enqueue_signal(&test_ba, &sig)) {
            // Process every 4 signals to maintain some queue depth
            if (i % 4 == 0) {
                bitactor_tick_with_integrated_optimization(&test_ba, &test_sched);
            }
        }
    }
    
    // Process remaining signals
    while (!bitactor_ring_empty(&test_ba)) {
        bitactor_tick_with_integrated_optimization(&test_ba, &test_sched);
    }
    
    uint64_t end_stress = __rdtsc();
    uint64_t stress_duration = end_stress - start_stress;
    
    // Verify system handled stress test
    TEST_ASSERT(test_ba.signal_count > 0, "Should have processed signals under stress");
    TEST_ASSERT(test_ba.tick_count > 0, "Should have executed ticks under stress");
    
    // Performance should still be reasonable under stress
    uint64_t avg_cycles_per_signal = stress_duration / (test_ba.signal_count + 1);
    TEST_ASSERT_LT(avg_cycles_per_signal, 50000, "Average processing time should be reasonable under stress");
    
    return true;
}

// Main test runner
int main() {
    TEST_INIT();
    
    printf("🧪 BitActor Integrated Optimization Tests\n");
    printf("Testing 80/20 implementation of integration gaps using TDD/ABDD\n\n");
    
    RUN_TEST(test_integrated_system_initialization);
    RUN_TEST(test_simple_news_validation_optimization);
    RUN_TEST(test_complex_news_validation_with_fibers);
    RUN_TEST(test_batch_news_validation_optimization);
    RUN_TEST(test_mixed_signal_processing_optimization);
    RUN_TEST(test_performance_metrics_collection);
    RUN_TEST(test_system_stress_with_high_load);
    
    TEST_SUMMARY();
    
    return 0;
}
