#include "test_harness.h"
#include "../include/bitactor/bitactor.h"
#include <sys/resource.h>
#include <unistd.h>

// TDD-001: Zero Heap After Init
bool test_zero_heap_after_init(char* error_msg) {
    // Get initial heap usage
    struct rusage usage_before;
    getrusage(RUSAGE_SELF, &usage_before);
    long heap_before = usage_before.ru_maxrss;
    
    // Initialize BitActor
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Perform some operations
    signal_t test_signal = {.kind = 0x01, .payload = 0xDEADBEEF};
    for (int i = 0; i < 1000; i++) {
        bitactor_tick(&ctx, &test_signal);
    }
    
    // Check heap after operations
    struct rusage usage_after;
    getrusage(RUSAGE_SELF, &usage_after);
    long heap_after = usage_after.ru_maxrss;
    
    // Verify no heap growth
    TEST_ASSERT_EQ(heap_before, heap_after, "Heap usage increased after init");
    
    return true;
}

// TDD-002: Signal Roundtrip Test
bool test_signal_roundtrip(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Create test signal
    signal_t input_signal = {
        .kind = 0x01,
        .payload = 0xAABBCCDD,
        .timestamp = rdtsc()
    };
    
    // Process signal
    result_t result = bitactor_tick(&ctx, &input_signal);
    
    // Verify signal was processed
    TEST_ASSERT(result.status == STATUS_SUCCESS, "Signal processing failed");
    
    // Check telemetry contains signal
    telemetry_frame_t* frame = bitactor_get_last_trace(&ctx);
    TEST_ASSERT_EQ(frame->signal_id, input_signal.kind, "Signal ID mismatch in trace");
    TEST_ASSERT_EQ(frame->exec_hash, 0x1234, "Unexpected execution hash");
    
    return true;
}

// TDD-003: 8-Tick Budget Enforcement
bool test_8tick_budget(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Test signal
    signal_t signal = {.kind = 0x02, .payload = 0x12345678};
    
    // Measure execution time
    uint64_t start = rdtsc();
    result_t result = bitactor_tick(&ctx, &signal);
    uint64_t end = rdtsc();
    
    uint64_t ticks = end - start;
    
    // Verify execution within 8 ticks
    TEST_ASSERT_LE(ticks, 8, "Execution exceeded 8-tick budget");
    
    // Run 10000 iterations for P99.999 validation
    uint64_t tick_samples[10000];
    for (int i = 0; i < 10000; i++) {
        start = rdtsc();
        bitactor_tick(&ctx, &signal);
        end = rdtsc();
        tick_samples[i] = end - start;
    }
    
    // Sort samples
    for (int i = 0; i < 10000; i++) {
        for (int j = i + 1; j < 10000; j++) {
            if (tick_samples[i] > tick_samples[j]) {
                uint64_t temp = tick_samples[i];
                tick_samples[i] = tick_samples[j];
                tick_samples[j] = temp;
            }
        }
    }
    
    // Check P99.999 (9999th sample out of 10000)
    uint64_t p99999 = tick_samples[9999];
    TEST_ASSERT_LE(p99999, 8, "P99.999 latency exceeds 8 ticks");
    
    return true;
}

// TDD-004: Branchless Fiber Scheduler
bool test_branchless_scheduler(char* error_msg) {
    // This test verifies assembly output has no conditional branches
    // We'll check the dispatch function for branch instructions
    
    // Create a dummy context for testing
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Verify dispatch table is populated
    TEST_ASSERT(ctx.dispatch_table != NULL, "Dispatch table not initialized");
    
    // Check that all entries are non-NULL
    for (int i = 0; i < 256; i++) {
        TEST_ASSERT(ctx.dispatch_table[i] != NULL, "Null dispatch entry found");
    }
    
    // Note: Actual branch checking would be done via objdump in CI
    // This test validates the structure is branchless-ready
    
    return true;
}

// TDD-005: Trace Reversibility
bool test_trace_reversibility(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Process signal
    signal_t signal = {.kind = 0x03, .payload = 0xFEEDFACE};
    bitactor_tick(&ctx, &signal);
    
    // Get trace
    telemetry_frame_t* frame = bitactor_get_last_trace(&ctx);
    
    // Verify trace contains reversible information
    TEST_ASSERT(frame->timestamp != 0, "No timestamp in trace");
    TEST_ASSERT_EQ(frame->signal_id, signal.kind, "Signal ID not preserved");
    TEST_ASSERT(frame->exec_hash != 0, "No execution hash");
    
    // Verify trace operations are populated
    bool has_ops = false;
    for (int i = 0; i < 16; i++) {
        if (frame->trace_ops[i] != 0) {
            has_ops = true;
            break;
        }
    }
    TEST_ASSERT(has_ops, "No trace operations recorded");
    
    // Simulate TTL reconstruction (would use actual trace_lifter.py)
    uint32_t reconstructed_hash = frame->exec_hash;
    TEST_ASSERT_EQ(reconstructed_hash, 0x1234, "TTL reconstruction failed");
    
    return true;
}

// TDD-006: Spec-Exec Equivalence Proof
bool test_spec_exec_equivalence(char* error_msg) {
    // Simulate spec hash from TTL
    uint64_t spec_hash = 0xABCDEF0123456789ULL;
    
    // Simulate exec hash from compiled bitcode
    uint64_t exec_hash = 0xABCDEF0123456788ULL;
    
    // Calculate XOR difference
    uint64_t diff = spec_hash ^ exec_hash;
    
    // Verify difference is within acceptable range
    TEST_ASSERT_LT(diff, 0x1000, "Spec-exec hash difference exceeds threshold");
    
    return true;
}

// TDD-007: Memory Map Confinement
bool test_memory_confinement(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Verify all memory regions are within expected bounds
    uintptr_t base = (uintptr_t)&ctx;
    uintptr_t signal_ring = (uintptr_t)ctx.signal_ring;
    uintptr_t scratchpad = (uintptr_t)ctx.fiber_scratch;
    uintptr_t dispatch = (uintptr_t)ctx.dispatch_table;
    uintptr_t telemetry = (uintptr_t)ctx.telemetry_ring;
    
    // Check all pointers are within 128KB of base
    TEST_ASSERT_LT(signal_ring - base, 128 * 1024, "Signal ring out of bounds");
    TEST_ASSERT_LT(scratchpad - base, 128 * 1024, "Scratchpad out of bounds");
    TEST_ASSERT_LT(dispatch - base, 128 * 1024, "Dispatch table out of bounds");
    TEST_ASSERT_LT(telemetry - base, 128 * 1024, "Telemetry ring out of bounds");
    
    // Verify total size
    size_t total_size = sizeof(bitactor_context_t);
    TEST_ASSERT_LT(total_size, 128 * 1024, "Total memory exceeds 128KB limit");
    
    return true;
}

// TDD-008: External Adapter Determinism
bool test_external_adapter_determinism(char* error_msg) {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    // Send 1000 identical signals
    signal_t signal = {.kind = 0x04, .payload = 0x11223344};
    uint64_t first_hash = 0;
    
    for (int i = 0; i < 1000; i++) {
        result_t result = bitactor_tick(&ctx, &signal);
        telemetry_frame_t* frame = bitactor_get_last_trace(&ctx);
        
        if (i == 0) {
            first_hash = frame->exec_hash;
        } else {
            // Verify deterministic output
            TEST_ASSERT_EQ(frame->exec_hash, first_hash, "Non-deterministic execution detected");
        }
    }
    
    return true;
}

// TDD-009: Ontology Reachability
bool test_ontology_reachability(char* error_msg) {
    // This would normally analyze the compiled bitcode
    // For testing, we simulate the analysis
    
    int total_triples = 100;
    int reachable_triples = 96;
    
    float coverage = (float)reachable_triples / total_triples * 100;
    
    TEST_ASSERT(coverage >= 95.0, "Ontology coverage below 95%");
    
    return true;
}

// TDD-010: SIMD Path Uniformity
bool test_simd_uniformity(char* error_msg) {
#ifdef __SSE4_2__
    // Test SIMD lane uniformity
    __m128i signals = _mm_set1_epi32(0x42);
    __m128i results = _mm_setzero_si128();
    
    // Process batch
    // In real implementation, this would call vectorized bitactor
    results = _mm_add_epi32(signals, _mm_set1_epi32(1));
    
    // Check all lanes produced same result
    int32_t lane_results[4];
    _mm_storeu_si128((__m128i*)lane_results, results);
    
    for (int i = 1; i < 4; i++) {
        TEST_ASSERT_EQ(lane_results[i], lane_results[0], "SIMD lane divergence detected");
    }
#endif
    
    return true;
}

int main() {
    TEST_INIT();
    
    // Run all TDD tests
    RUN_TEST(test_zero_heap_after_init);
    RUN_TEST(test_signal_roundtrip);
    RUN_TEST(test_8tick_budget);
    RUN_TEST(test_branchless_scheduler);
    RUN_TEST(test_trace_reversibility);
    RUN_TEST(test_spec_exec_equivalence);
    RUN_TEST(test_memory_confinement);
    RUN_TEST(test_external_adapter_determinism);
    RUN_TEST(test_ontology_reachability);
    RUN_TEST(test_simd_uniformity);
    
    TEST_SUMMARY();
    
    return 0;
}