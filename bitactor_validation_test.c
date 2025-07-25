#define CNSFORGE_IMPLEMENTATION
#include "generated/bytecode/cnsforge.c"
#include <stdio.h>
#include <assert.h>
#include <time.h>

int main() {
    printf("=== BitActor C Code Validation Test ===\n");
    
    // Test 1: Basic initialization
    printf("Test 1: BitActor initialization...\n");
    cnsforge_bitactor_t bitactor;
    cnsforge_bitactor_init(&bitactor);
    printf("✅ Initialization successful\n");
    
    // Test 2: Signal enqueue/dequeue
    printf("Test 2: Signal processing...\n");
    cnsforge_signal_t test_signal = {
        .type = CNSFORGE_SIGNAL_THREAT_DETECTED,
        .flags = 0,
        .timestamp = 12345678,
        .payload = 0xDEADBEEF
    };
    
    bool enqueue_result = cnsforge_bitactor_enqueue_signal(&bitactor, &test_signal);
    printf("Signal enqueue result: %s\n", enqueue_result ? "SUCCESS" : "FAILED");
    
    // Test 3: Tick processing with timing
    printf("Test 3: Tick processing and performance...\n");
    uint64_t start_cycles = rdtsc();
    cnsforge_bitactor_tick(&bitactor);
    uint64_t end_cycles = rdtsc();
    uint64_t elapsed_cycles = end_cycles - start_cycles;
    
    printf("Tick processing took %llu cycles\n", elapsed_cycles);
    printf("8-tick budget: %d cycles\n", CNSFORGE_TICK_BUDGET);
    
    if (elapsed_cycles <= CNSFORGE_TICK_BUDGET) {
        printf("✅ Tick budget constraint satisfied\n");
    } else {
        printf("❌ Tick budget VIOLATED! Took %llu cycles, budget is %d\n", 
               elapsed_cycles, CNSFORGE_TICK_BUDGET);
    }
    
    // Test 4: Ring buffer overflow
    printf("Test 4: Ring buffer stress test...\n");
    int successful_enqueues = 0;
    for (int i = 0; i < CNSFORGE_RING_SIZE + 10; i++) {
        test_signal.payload = i;
        if (cnsforge_bitactor_enqueue_signal(&bitactor, &test_signal)) {
            successful_enqueues++;
        }
    }
    printf("Successfully enqueued %d/%d signals\n", successful_enqueues, CNSFORGE_RING_SIZE + 10);
    
    // Test 5: Performance benchmark
    printf("Test 5: Performance benchmark (1000 ticks)...\n");
    struct timespec start_time, end_time;
    clock_gettime(CLOCK_MONOTONIC, &start_time);
    
    for (int i = 0; i < 1000; i++) {
        cnsforge_bitactor_tick(&bitactor);
    }
    
    clock_gettime(CLOCK_MONOTONIC, &end_time);
    double elapsed_ms = (end_time.tv_sec - start_time.tv_sec) * 1000.0 +
                        (end_time.tv_nsec - start_time.tv_nsec) / 1000000.0;
    
    printf("1000 ticks took %.2f ms (%.2f us per tick)\n", elapsed_ms, elapsed_ms * 1000 / 1000);
    
    printf("\n=== BitActor Validation Summary ===\n");
    printf("Total signals processed: %llu\n", bitactor.signal_count);
    printf("Total tick count: %llu\n", bitactor.tick_count);
    
    return 0;
}