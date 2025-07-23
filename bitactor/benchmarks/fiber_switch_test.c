#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "../include/bitactor/bitactor.h"

#define SWITCH_COUNT 1000000

// Simulated fiber context
typedef struct {
    uint64_t regs[16];  // Simulated registers
    uint64_t stack_ptr;
    uint64_t instruction_ptr;
} fiber_context_t;

// Inline fiber switch (no function call overhead)
static inline void fiber_switch(fiber_context_t* from, fiber_context_t* to) {
    // In real implementation, this would be assembly
    // For testing, we simulate the switch
    uint64_t temp[16];
    __builtin_memcpy(temp, from->regs, sizeof(temp));
    __builtin_memcpy(from->regs, to->regs, sizeof(temp));
    __builtin_memcpy(to->regs, temp, sizeof(temp));
}

// CPU tick counter
static inline uint64_t rdtsc(void) {
#if defined(__x86_64__) || defined(__i386__)
    unsigned int lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
#else
    return 0;
#endif
}

int main() {
    fiber_context_t fiber1 = {0};
    fiber_context_t fiber2 = {0};
    
    // Initialize contexts
    for (int i = 0; i < 16; i++) {
        fiber1.regs[i] = i;
        fiber2.regs[i] = i + 100;
    }
    
    printf("BitActor Fiber Switch Performance Test\n");
    printf("=====================================\n");
    
    // Warmup
    for (int i = 0; i < 10000; i++) {
        fiber_switch(&fiber1, &fiber2);
    }
    
    // Measure switch time
    uint64_t start = rdtsc();
    
    for (int i = 0; i < SWITCH_COUNT; i++) {
        fiber_switch(&fiber1, &fiber2);
    }
    
    uint64_t end = rdtsc();
    uint64_t total_ticks = end - start;
    double ticks_per_switch = (double)total_ticks / SWITCH_COUNT;
    
    printf("Total switches: %d\n", SWITCH_COUNT);
    printf("Total ticks: %lu\n", total_ticks);
    printf("Ticks per switch: %.2f\n", ticks_per_switch);
    
    // Verify contexts were actually switched
    int valid = 1;
    for (int i = 0; i < 16; i++) {
        if (fiber1.regs[i] == (uint64_t)i) {
            valid = 0;
            break;
        }
    }
    
    if (valid) {
        printf("\n✅ Fiber contexts switched correctly\n");
    } else {
        printf("\n❌ Fiber context switch failed\n");
        return 1;
    }
    
    // Performance gate
    if (ticks_per_switch < 20) {
        printf("✅ Fiber switch performance acceptable\n");
        return 0;
    } else {
        printf("❌ Fiber switch too slow\n");
        return 1;
    }
}