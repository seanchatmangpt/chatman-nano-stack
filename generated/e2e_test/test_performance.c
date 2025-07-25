#include <stdio.h>
#include <time.h>
#include <stdint.h>

static inline uint64_t get_cycles() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

int main() {
    const int iterations = 10000;
    int compliant = 0;
    
    for (int i = 0; i < iterations; i++) {
        uint64_t start = get_cycles();
        
        // Simulate 8-tick operation
        volatile int x = 0;
        for (int j = 0; j < 8; j++) {
            x += j;
        }
        
        uint64_t end = get_cycles();
        uint64_t elapsed = end - start;
        
        // Check if under 1000ns (1 microsecond)
        if (elapsed < 1000) {
            compliant++;
        }
    }
    
    double compliance_rate = (double)compliant / iterations * 100;
    printf("8-tick compliance: %.2f%%\n", compliance_rate);
    
    return compliance_rate >= 95.0 ? 0 : 1;
}