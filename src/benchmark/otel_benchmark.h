#ifndef OTEL_BENCHMARK_H
#define OTEL_BENCHMARK_H

#include <stdint.h>
#include <stdbool.h>
#include <time.h>

typedef struct {
    const char* name;
    uint64_t start_tsc;
    uint64_t end_tsc;
    uint64_t count;
    double duration_ns;
} otel_metric_t;

typedef struct {
    otel_metric_t metrics[100];
    int metric_count;
} otel_context_t;

void otel_init(otel_context_t* ctx);
void otel_start_timing(otel_context_t* ctx, const char* name);
void otel_end_timing(otel_context_t* ctx, const char* name, uint64_t count);
void otel_report_mermaid(otel_context_t* ctx);

static inline uint64_t rdtsc() {
#ifdef __aarch64__
    uint64_t val;
    asm volatile("mrs %0, cntvct_el0" : "=r" (val));
    return val;
#else
    unsigned int lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
#endif
}

#endif