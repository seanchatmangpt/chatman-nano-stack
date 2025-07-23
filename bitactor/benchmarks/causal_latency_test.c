#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "../include/bitactor/bitactor.h"

#define ITERATIONS 100000
#define WARMUP_ITERATIONS 10000

// CPU tick counter
static inline uint64_t rdtsc(void) {
#if defined(__x86_64__) || defined(__i386__)
    unsigned int lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
#else
    return 0; // Fallback
#endif
}

int main() {
    bitactor_context_t ctx;
    bitactor_init(&ctx);
    
    signal_t signal = {.kind = 0x01, .payload = 0xDEADBEEF};
    uint64_t* latencies = malloc(ITERATIONS * sizeof(uint64_t));
    
    // Warmup
    for (int i = 0; i < WARMUP_ITERATIONS; i++) {
        bitactor_tick(&ctx, &signal);
    }
    
    // Measure latencies
    for (int i = 0; i < ITERATIONS; i++) {
        uint64_t start = rdtsc();
        bitactor_tick(&ctx, &signal);
        uint64_t end = rdtsc();
        latencies[i] = end - start;
    }
    
    // Sort latencies
    for (int i = 0; i < ITERATIONS - 1; i++) {
        for (int j = 0; j < ITERATIONS - i - 1; j++) {
            if (latencies[j] > latencies[j + 1]) {
                uint64_t temp = latencies[j];
                latencies[j] = latencies[j + 1];
                latencies[j + 1] = temp;
            }
        }
    }
    
    // Calculate percentiles
    uint64_t p50 = latencies[ITERATIONS / 2];
    uint64_t p90 = latencies[(int)(ITERATIONS * 0.90)];
    uint64_t p99 = latencies[(int)(ITERATIONS * 0.99)];
    uint64_t p999 = latencies[(int)(ITERATIONS * 0.999)];
    uint64_t p9999 = latencies[(int)(ITERATIONS * 0.9999)];
    uint64_t p99999 = latencies[ITERATIONS - 1];
    
    printf("BitActor Causal Latency Test Results:\n");
    printf("=====================================\n");
    printf("Samples: %d\n", ITERATIONS);
    printf("P50    = %lu ticks\n", p50);
    printf("P90    = %lu ticks\n", p90);
    printf("P99    = %lu ticks\n", p99);
    printf("P99.9  = %lu ticks\n", p999);
    printf("P99.99 = %lu ticks\n", p9999);
    printf("P99.999 = %lu ticks\n", p99999);
    
    // Gate check
    if (p99999 <= 8) {
        printf("\n✅ P99.999 <= 8 ticks (PASS)\n");
    } else {
        printf("\n❌ P99.999 > 8 ticks (FAIL)\n");
        free(latencies);
        return 1;
    }
    
    free(latencies);
    return 0;
}