#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>
#include "../src/news_validation_optimized.c"

// Platform-compatible cycle counter
static inline uint64_t rdtsc(void) {
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

// Test data
static bitactor_t test_ba;

// Benchmark news validation targeting 10ns
void benchmark_news_validation_10ns() {
    printf("\n🚀 News Validation 10ns Performance Benchmark\n");
    printf("============================================\n");
    
    // Initialize
    bitactor_init(&test_ba);
    bitactor_init_news_10ns(&test_ba);
    
    // Warmup - important for lookup table cache
    for (int i = 0; i < 1000; i++) {
        signal_t warmup_sig = {
            .kind = 0x1001,
            .flags = 0x02,
            .timestamp = ((uint64_t)i << 40) | rdtsc(),
            .payload = 0xDEADBEEF + i
        };
        news_validation_handler_10ns(&warmup_sig, test_ba.scratch);
    }
    
    // Benchmark single validation
    const int iterations = 1000000;
    uint64_t total_cycles = 0;
    uint64_t min_cycles = UINT64_MAX;
    uint64_t max_cycles = 0;
    
    // Single signal benchmark
    printf("\n📊 Single Signal Validation:\n");
    for (int i = 0; i < iterations; i++) {
        signal_t sig = {
            .kind = 0x1001,
            .flags = 0x02,
            .timestamp = ((uint64_t)(i & 0xFF) << 40) | rdtsc(),
            .payload = 0xCAFEBABE + i
        };
        
        uint64_t start = rdtsc();
        news_validation_handler_10ns(&sig, test_ba.scratch);
        uint64_t end = rdtsc();
        
        uint64_t cycles = end - start;
        total_cycles += cycles;
        if (cycles < min_cycles) min_cycles = cycles;
        if (cycles > max_cycles) max_cycles = cycles;
    }
    
    // Calculate nanoseconds (assume 2.4GHz CPU)
    double cycles_per_ns = 2.4; // Adjust based on your CPU
    double avg_ns = (double)(total_cycles / iterations) / cycles_per_ns;
    double min_ns = (double)min_cycles / cycles_per_ns;
    double max_ns = (double)max_cycles / cycles_per_ns;
    
    printf("   Average: %.2f ns %s\n", avg_ns, avg_ns <= 10.0 ? "✅" : "❌");
    printf("   Min: %.2f ns\n", min_ns);
    printf("   Max: %.2f ns\n", max_ns);
    printf("   Ops/sec: %.2fM\n", 1000.0 / avg_ns);
    
    // Batch validation benchmark
    printf("\n📊 Batch Validation (4 signals):\n");
    signal_t batch[4];
    total_cycles = 0;
    
    for (int i = 0; i < iterations / 4; i++) {
        for (int j = 0; j < 4; j++) {
            batch[j].kind = 0x1001;
            batch[j].flags = 0x02;
            batch[j].timestamp = ((uint64_t)((i + j) & 0xFF) << 40) | rdtsc();
            batch[j].payload = 0xDEADBEEF + i + j;
        }
        
        uint64_t start = rdtsc();
        batch_validation_simd_10ns(batch, 4, test_ba.scratch);
        uint64_t end = rdtsc();
        
        total_cycles += (end - start);
    }
    
    double batch_avg_ns = (double)(total_cycles / (iterations / 4)) / cycles_per_ns / 4.0;
    printf("   Average per signal: %.2f ns %s\n", batch_avg_ns, batch_avg_ns <= 10.0 ? "✅" : "❌");
    printf("   Ops/sec: %.2fM\n", 1000.0 / batch_avg_ns);
    
    // Direct inline validation
    printf("\n📊 Inline Validation:\n");
    total_cycles = 0;
    
    for (int i = 0; i < iterations; i++) {
        uint64_t source_id = ((uint64_t)(i & 0xFF) << 40);
        
        uint64_t start = rdtsc();
        uint32_t result = validate_inline_10ns(source_id);
        uint64_t end = rdtsc();
        
        total_cycles += (end - start);
        // Prevent optimization
        if (result == 0xFFFFFFFF) printf(".");
    }
    
    double inline_avg_ns = (double)(total_cycles / iterations) / cycles_per_ns;
    printf("   Average: %.2f ns %s\n", inline_avg_ns, inline_avg_ns <= 10.0 ? "✅" : "❌");
    printf("   Ops/sec: %.2fM\n", 1000.0 / inline_avg_ns);
    
    // Summary
    printf("\n🎯 PERFORMANCE SUMMARY\n");
    printf("====================\n");
    if (avg_ns <= 10.0 && batch_avg_ns <= 10.0 && inline_avg_ns <= 10.0) {
        printf("✅ ALL VALIDATIONS MEET 10ns TARGET!\n");
    } else {
        printf("❌ Some validations exceed 10ns target\n");
    }
    
    printf("\n📈 Optimization Techniques Applied:\n");
    printf("   • Pre-computed lookup tables (O(1) access)\n");
    printf("   • Cache-line aligned data structures\n");
    printf("   • Branchless validation logic\n");
    printf("   • SIMD batch processing\n");
    printf("   • Inline functions with hot attributes\n");
    printf("   • Prefetching for sequential access\n");
}

// Correctness test
void test_correctness() {
    printf("\n🧪 Correctness Tests:\n");
    
    bitactor_init(&test_ba);
    bitactor_init_news_10ns(&test_ba);
    
    // Test low credibility source
    signal_t low_cred = {
        .kind = 0x1001,
        .flags = 0x02,
        .timestamp = ((uint64_t)0x10 << 40), // Will map to low credibility
        .payload = 0x12345678
    };
    
    news_validation_handler_10ns(&low_cred, test_ba.scratch);
    uint32_t result = *(uint32_t*)test_ba.scratch;
    
    printf("   Low credibility test: %s\n", 
           (result & 0x80000000) ? "✅ PASS (rejected)" : "❌ FAIL");
    
    // Test high credibility source
    signal_t high_cred = {
        .kind = 0x1001,
        .flags = 0x02,
        .timestamp = ((uint64_t)0xF0 << 40), // Will map to high credibility
        .payload = 0x87654321
    };
    
    news_validation_handler_10ns(&high_cred, test_ba.scratch);
    result = *(uint32_t*)test_ba.scratch;
    
    printf("   High credibility test: %s\n",
           !(result & 0x80000000) ? "✅ PASS (accepted)" : "❌ FAIL");
}

int main() {
    printf("🎯 News Validation 10ns Optimization Benchmark\n");
    printf("==============================================\n");
    
    // Run correctness tests first
    test_correctness();
    
    // Run performance benchmarks
    benchmark_news_validation_10ns();
    
    return 0;
}
