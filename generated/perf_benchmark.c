#include <stdio.h>
#include <time.h>
#include <stdint.h>

#define ITERATIONS 1000000
#define TARGET_CYCLES 8

int main() {
    struct timespec start, end;
    uint64_t total_cycles = 0;
    
    for (int i = 0; i < ITERATIONS; i++) {
        clock_gettime(CLOCK_MONOTONIC, &start);
        
        // Simulated work
        volatile int x = 0;
        for (int j = 0; j < TARGET_CYCLES; j++) {
            x += j;
        }
        
        clock_gettime(CLOCK_MONOTONIC, &end);
        total_cycles += (end.tv_nsec - start.tv_nsec);
    }
    
    double avg_cycles = (double)total_cycles / ITERATIONS;
    printf("Average cycles: %.2f\n", avg_cycles);
    
    return avg_cycles <= 1000 ? 0 : 1;  // Sub-microsecond
}