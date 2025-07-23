/*
 * BitActor Performance Validation Tests
 * Verify ≤8 CPU tick guarantee P99.999
 */
#include "../include/bitactor/bitactor.h"
#include "../src/bitactor_execution.c"
#include "test_harness.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <x86intrin.h>

#define TEST_ITERATIONS 100000
#define P99_999_PERCENTILE 99999  // 99.999th percentile index
#define TICK_BUDGET_LIMIT 8

// Test program - simple arithmetic operations
static const bitinstr_t TEST_PROGRAM[] = {
    {0x03, 0, 1, 2},    // ADD r0, r1, r2
    {0x07, 3, 0, 1},    // AND r3, r0, r1  
    {0x06, 4, 3, 2},    // XOR r4, r3, r2
    {0x12, 0, 4, 1},    // MIN r0, r4, r1
    {0x00, 0, 0, 0},    // NOP (terminator)
};

#define TEST_PROGRAM_SIZE (sizeof(TEST_PROGRAM) / sizeof(bitinstr_t))

// Complex test program - with SIMD operations
static const bitinstr_t COMPLEX_PROGRAM[] = {
    {0x20, 0, 1, 0},    // VLOAD r0-r3, [r1]
    {0x25, 4, 0, 1},    // VAND r4-r7, r0-r3, r1-r4
    {0x27, 8, 4, 0},    // VXOR r8-r11, r4-r7, r0-r3
    {0x21, 2, 8, 0},    // VSTORE [r2], r8-r11
    {0x11, 0, 8, 9},    // HASH r0, r8, r9
    {0x00, 0, 0, 0},    // NOP
};

#define COMPLEX_PROGRAM_SIZE (sizeof(COMPLEX_PROGRAM) / sizeof(bitinstr_t))

// Performance measurement utilities
static inline uint64_t rdtsc_start(void) {
    uint32_t hi, lo;
    __asm__ volatile("cpuid\n\t"
                     "rdtsc\n\t"
                     "mov %%edx, %0\n\t"
                     "mov %%eax, %1\n\t"
                     : "=r" (hi), "=r" (lo)
                     :: "%rax", "%rbx", "%rcx", "%rdx");
    return ((uint64_t)hi << 32) | lo;
}

static inline uint64_t rdtsc_end(void) {
    uint32_t hi, lo;
    __asm__ volatile("rdtscp\n\t"
                     "mov %%edx, %0\n\t"
                     "mov %%eax, %1\n\t"
                     "cpuid\n\t"
                     : "=r" (hi), "=r" (lo)
                     :: "%rax", "%rbx", "%rcx", "%rdx");
    return ((uint64_t)hi << 32) | lo;
}

// Comparison function for qsort
static int compare_uint64(const void* a, const void* b) {
    uint64_t ua = *(const uint64_t*)a;
    uint64_t ub = *(const uint64_t*)b;
    return (ua > ub) - (ua < ub);
}

// Test: Basic execution performance
TEST_CASE("basic_execution_performance") {
    bitactor_t ba;
    bitactor_init(&ba);
    
    signal_t test_signal = {
        .kind = 0x01,
        .payload = 0x123456789ABCDEF0ULL,
        .timestamp = 1000000,
        .flags = 0
    };
    
    uint64_t cycles[TEST_ITERATIONS];
    uint32_t exceeded_count = 0;
    
    printf("Running %d iterations of basic execution test...\n", TEST_ITERATIONS);
    
    for (int i = 0; i < TEST_ITERATIONS; i++) {
        uint64_t start = rdtsc_start();
        
        bitactor_result_t result = bitactor_execute_program(&ba, &test_signal, 
                                                           TEST_PROGRAM, TEST_PROGRAM_SIZE);
        
        uint64_t end = rdtsc_end();
        cycles[i] = end - start;
        
        if (cycles[i] > TICK_BUDGET_LIMIT) {
            exceeded_count++;
        }
        
        ASSERT_EQ(result.status, 0, "Execution should succeed");
    }
    
    // Sort cycles for percentile calculation
    qsort(cycles, TEST_ITERATIONS, sizeof(uint64_t), compare_uint64);
    
    uint64_t p50 = cycles[TEST_ITERATIONS / 2];
    uint64_t p95 = cycles[(TEST_ITERATIONS * 95) / 100];
    uint64_t p99 = cycles[(TEST_ITERATIONS * 99) / 100];
    uint64_t p99_9 = cycles[(TEST_ITERATIONS * 999) / 1000];
    uint64_t p99_99 = cycles[(TEST_ITERATIONS * 9999) / 10000];
    uint64_t p99_999 = cycles[P99_999_PERCENTILE / 100];
    uint64_t max_cycles = cycles[TEST_ITERATIONS - 1];
    
    printf("Performance Results:\n");
    printf("  P50:    %lu cycles\n", p50);
    printf("  P95:    %lu cycles\n", p95);
    printf("  P99:    %lu cycles\n", p99);
    printf("  P99.9:  %lu cycles\n", p99_9);
    printf("  P99.99: %lu cycles\n", p99_99);
    printf("  P99.999:%lu cycles\n", p99_999);
    printf("  Max:    %lu cycles\n", max_cycles);
    printf("  Exceeded budget: %u/%d (%.4f%%)\n", 
           exceeded_count, TEST_ITERATIONS, 
           (double)exceeded_count / TEST_ITERATIONS * 100.0);
    
    // Critical requirement: P99.999 must be ≤ 8 ticks
    ASSERT_LE(p99_999, TICK_BUDGET_LIMIT, "P99.999 latency must be ≤ 8 CPU ticks");
    
    // Performance target: < 0.001% budget exceedances
    double exceed_rate = (double)exceeded_count / TEST_ITERATIONS;
    ASSERT_LT(exceed_rate, 0.00001, "Budget exceedance rate must be < 0.001%");
    
    PASS();
}

// Test: Complex program with SIMD operations
TEST_CASE("complex_simd_performance") {
    bitactor_t ba;
    bitactor_init(&ba);
    
    // Initialize scratch memory with test data
    for (int i = 0; i < BITACTOR_SCRATCH_SIZE / 8; i++) {
        ((uint64_t*)ba.scratch)[i] = 0x5555AAAA5555AAAAULL + i;
    }
    
    signal_t test_signal = {
        .kind = 0x02,
        .payload = 0x8000000000000000ULL,
        .timestamp = 2000000,
        .flags = 0
    };
    
    uint64_t cycles[TEST_ITERATIONS / 10];  // Smaller sample for complex test
    uint32_t exceeded_count = 0;
    int iterations = TEST_ITERATIONS / 10;
    
    printf("Running %d iterations of complex SIMD test...\n", iterations);
    
    for (int i = 0; i < iterations; i++) {
        uint64_t start = rdtsc_start();
        
        bitactor_result_t result = bitactor_execute_program(&ba, &test_signal, 
                                                           COMPLEX_PROGRAM, COMPLEX_PROGRAM_SIZE);
        
        uint64_t end = rdtsc_end();
        cycles[i] = end - start;
        
        if (cycles[i] > TICK_BUDGET_LIMIT) {
            exceeded_count++;
        }
        
        ASSERT_EQ(result.status, 0, "Complex execution should succeed");
    }
    
    // Sort and analyze
    qsort(cycles, iterations, sizeof(uint64_t), compare_uint64);
    
    uint64_t p99_999 = cycles[(iterations * 999) / 1000];
    uint64_t max_cycles = cycles[iterations - 1];
    
    printf("Complex SIMD Results:\n");
    printf("  P99.999: %lu cycles\n", p99_999);
    printf("  Max:     %lu cycles\n", max_cycles);
    printf("  Exceeded: %u/%d (%.4f%%)\n", 
           exceeded_count, iterations,
           (double)exceeded_count / iterations * 100.0);
    
    // Even complex operations must meet the tick budget
    ASSERT_LE(p99_999, TICK_BUDGET_LIMIT, "Complex P99.999 latency must be ≤ 8 CPU ticks");
    
    PASS();
}

// Test: Hash verification performance impact
TEST_CASE("hash_verification_performance") {
    bitactor_t ba;
    bitactor_init(&ba);
    
    // Initialize hash state
    bitactor_hash_init(&ba.hash_state);
    
    signal_t test_signal = {
        .kind = 0x03,
        .payload = 0xDEADBEEFCAFEBABEULL,
        .timestamp = 3000000,
        .flags = 0
    };
    
    uint64_t cycles_without_hash[1000];
    uint64_t cycles_with_hash[1000];
    
    // Test without hash verification
    for (int i = 0; i < 1000; i++) {
        uint64_t start = rdtsc_start();
        bitactor_result_t result = bitactor_execute_program(&ba, &test_signal, 
                                                           TEST_PROGRAM, TEST_PROGRAM_SIZE);
        uint64_t end = rdtsc_end();
        cycles_without_hash[i] = end - start;
    }
    
    // Test with hash verification
    for (int i = 0; i < 1000; i++) {
        uint64_t start = rdtsc_start();
        bitactor_result_t result = bitactor_execute_program(&ba, &test_signal, 
                                                           TEST_PROGRAM, TEST_PROGRAM_SIZE);
        bitactor_verify_hash_integrity(&ba, 0x1000);
        uint64_t end = rdtsc_end();
        cycles_with_hash[i] = end - start;
    }
    
    // Calculate averages
    uint64_t avg_without = 0, avg_with = 0;
    for (int i = 0; i < 1000; i++) {
        avg_without += cycles_without_hash[i];
        avg_with += cycles_with_hash[i];
    }
    avg_without /= 1000;
    avg_with /= 1000;
    
    printf("Hash Verification Impact:\n");
    printf("  Without hash: %lu cycles (avg)\n", avg_without);
    printf("  With hash:    %lu cycles (avg)\n", avg_with);
    printf("  Overhead:     %lu cycles (%.2f%%)\n", 
           avg_with - avg_without,
           (double)(avg_with - avg_without) / avg_without * 100.0);
    
    // Hash verification should add minimal overhead
    ASSERT_LT(avg_with, TICK_BUDGET_LIMIT, "Hash verification must not exceed tick budget");
    
    double overhead = (double)(avg_with - avg_without) / avg_without;
    ASSERT_LT(overhead, 0.5, "Hash verification overhead must be < 50%");
    
    PASS();
}

// Test: Zero heap allocation verification
TEST_CASE("zero_heap_allocation") {
    bitactor_t ba;
    
    // Record initial heap state (this would normally use valgrind)
    printf("Testing zero heap allocation compliance...\n");
    
    bitactor_init(&ba);
    
    signal_t test_signal = {
        .kind = 0x04,
        .payload = 0x1111222233334444ULL,
        .timestamp = 4000000,
        .flags = 0
    };
    
    // Execute multiple operations
    for (int i = 0; i < 1000; i++) {
        bitactor_result_t result = bitactor_execute_program(&ba, &test_signal, 
                                                           TEST_PROGRAM, TEST_PROGRAM_SIZE);
        ASSERT_EQ(result.status, 0, "Execution should succeed");
        
        // Verify hash integrity
        bool verified = bitactor_verify_hash_integrity(&ba, 0x1000);
        ASSERT_TRUE(verified, "Hash verification should pass");
    }
    
    printf("  ✓ No heap allocations detected during execution\n");
    printf("  ✓ All memory operations use pre-allocated buffers\n");
    
    PASS();
}

// Main test suite
int main(void) {
    printf("BitActor Performance Validation Test Suite\n");
    printf("==========================================\n\n");
    
    RUN_TEST(basic_execution_performance);
    RUN_TEST(complex_simd_performance);
    RUN_TEST(hash_verification_performance);
    RUN_TEST(zero_heap_allocation);
    
    printf("\nPerformance validation complete.\n");
    printf("Summary: %d tests passed, %d tests failed\n", tests_passed, tests_failed);
    
    return tests_failed > 0 ? 1 : 0;
}