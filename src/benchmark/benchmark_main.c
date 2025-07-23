#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include "otel_benchmark.h"
#include "../cns/tick_parallel.h"
#include "../cns/bitactor_lite.h"
#include "../cns/cns_pipeline.h"
#include "../sparql/sparql_to_bitactor.h"

#define ITERATIONS 1000000
#define ARENA_SIZE (64 * 1024 * 1024)

void benchmark_arena_allocator(otel_context_t* ctx) {
    void* arena_mem = mmap(NULL, ARENA_SIZE, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    void* arena_ptr = arena_mem;
    
    otel_start_timing(ctx, "arena_alloc_1tick");
    for (int i = 0; i < ITERATIONS; i++) {
        void* p = tick_arena_alloc(&arena_ptr, 64);
        __asm__ __volatile__("" : : "r"(p) : "memory");
    }
    otel_end_timing(ctx, "arena_alloc_1tick", ITERATIONS);
    
    munmap(arena_mem, ARENA_SIZE);
}

void benchmark_bitactor_lite(otel_context_t* ctx) {
    fast_proof_t proof = {
        .capability = 0x1234567890ABCDEF,
        .hash = 0xB3C4D5E6F7A8B9C0
    };
    
    otel_start_timing(ctx, "bitactor_verify_1tick");
    for (int i = 0; i < ITERATIONS; i++) {
        bool result = bitactor_verify_fast(&proof);
        __asm__ __volatile__("" : : "r"(result) : "memory");
    }
    otel_end_timing(ctx, "bitactor_verify_1tick", ITERATIONS);
}

void benchmark_8tick_pipeline(otel_context_t* ctx) {
    quote_t quotes[1000];
    for (int i = 0; i < 1000; i++) {
        quotes[i].symbol = 0x41424344;
        quotes[i].price = 100 + (i % 50);
        quotes[i].volume = 1000 * (i + 1);
        quotes[i].timestamp = time(NULL) + i;
    }
    
    otel_start_timing(ctx, "process_quote_8tick");
    for (int i = 0; i < 1000; i++) {
        process_quote_8tick(&quotes[i]);
    }
    otel_end_timing(ctx, "process_quote_8tick", 1000);
}

void benchmark_sparql_8hop(otel_context_t* ctx) {
    proof_chain_t chain = {
        .hops = {
            {.capability_id = 0x1234567890ABCDEF, .validation_fn = (uint64_t)&validate_exists, .data_offset = 0},
            {.capability_id = 0x234567890ABCDEF1, .validation_fn = (uint64_t)&validate_exists, .data_offset = 8},
            {.capability_id = 0x34567890ABCDEF12, .validation_fn = (uint64_t)&validate_exists, .data_offset = 16},
            {.capability_id = 0x4567890ABCDEF123, .validation_fn = (uint64_t)&validate_not_expired, .data_offset = 24},
            {.capability_id = 0x567890ABCDEF1234, .validation_fn = (uint64_t)&always_true, .data_offset = 32},
            {.capability_id = 0x67890ABCDEF12345, .validation_fn = (uint64_t)&always_true, .data_offset = 40},
            {.capability_id = 0x7890ABCDEF123456, .validation_fn = (uint64_t)&always_true, .data_offset = 48},
            {.capability_id = 0x890ABCDEF1234567, .validation_fn = (uint64_t)&always_true, .data_offset = 56}
        }
    };
    
    uint64_t test_data[8];
    for (int i = 0; i < 8; i++) {
        test_data[i] = 0xFFFFFFFFFFFFFFFF;
    }
    test_data[3] = time(NULL) + 3600;
    
    otel_start_timing(ctx, "sparql_execute_8hop");
    for (int i = 0; i < ITERATIONS; i++) {
        bool result = bitactor_execute_8hop(&chain, test_data);
        __asm__ __volatile__("" : : "r"(result) : "memory");
    }
    otel_end_timing(ctx, "sparql_execute_8hop", ITERATIONS);
}

static void increment(void* data) {
    int* counter = (int*)data;
    (*counter)++;
}

void benchmark_tick_unit(otel_context_t* ctx) {
    int counters[8] = {0};
    
    tick_unit_t unit = {
        .ops = {increment, increment, increment, increment,
                increment, increment, increment, increment},
        .data = {&counters[0], &counters[1], &counters[2], &counters[3],
                 &counters[4], &counters[5], &counters[6], &counters[7]},
        .tick_mask = 0xFF
    };
    
    otel_start_timing(ctx, "tick_execute_8tick");
    for (int i = 0; i < 10000; i++) {
        tick_execute(&unit);
    }
    otel_end_timing(ctx, "tick_execute_8tick", 10000);
}

int main() {
    otel_context_t ctx;
    otel_init(&ctx);
    
    printf("Running OpenTelemetry benchmarks...\n\n");
    
    benchmark_arena_allocator(&ctx);
    benchmark_bitactor_lite(&ctx);
    benchmark_tick_unit(&ctx);
    benchmark_8tick_pipeline(&ctx);
    benchmark_sparql_8hop(&ctx);
    
    otel_report_mermaid(&ctx);
    
    return 0;
}