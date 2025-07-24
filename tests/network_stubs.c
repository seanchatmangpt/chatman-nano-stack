/* Network function stubs for testing */
#include <stdio.h>
int network_engine_init(int port) { (void)port; return 0; }
void network_engine_shutdown(void) {}
void network_engine_print_stats(void) { printf("Network stats: (stubbed)\n"); }

/* Missing get_cpu_cycles if not defined */
#ifndef __x86_64__
#ifndef __aarch64__
#include <time.h>
uint64_t get_cpu_cycles(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}
#endif
#endif
