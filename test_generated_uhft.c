#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

// Include our generated UHFT core module
#include "generated_c/uhft_core/uhft_core.h"

// Simple timing for 8-tick validation
static inline uint64_t rdtsc() {
#ifdef __x86_64__
    return __builtin_ia32_rdtsc();
#elif defined(__aarch64__)
    uint64_t val;
    asm volatile("mrs %0, cntvct_el0" : "=r" (val));
    return val;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#endif
}

// Stub implementations for missing functions
void long_destroy(long_t* ptr) {
    if (ptr) free(ptr);
}

void arena_destroy(BitActor_t* arena) {
    if (arena) bit_actor_destroy(arena);
}

bool arena_validate(BitActor_t* arena) {
    return arena && bit_actor_is_healthy(arena);
}

bool bit_actor_validate(BitActor_t* actor) {
    return bit_actor_is_healthy(actor);
}

bool ring_bus_validate(RingBus_t* bus) {
    return bus && ((BitActor_t*)bus)->state == BIT_ACTOR_RUNNING;
}

bool fiber_validate(Fiber_t* fiber) {
    return fiber && ((BitActor_t*)fiber)->state == BIT_ACTOR_RUNNING;
}

int ring_bus_destroy(RingBus_t* bus) {
    return bit_actor_destroy((BitActor_t*)bus);
}

int fiber_destroy(Fiber_t* fiber) {
    return bit_actor_destroy((BitActor_t*)fiber);
}

// Stub implementations for base classes
BitActor_t* bit_actor_create(const char* name, const BitActorVTable_t* vtable) {
    BitActor_t* actor = calloc(1, sizeof(BitActor_t));
    if (actor) {
        actor->magic_header = BIT_ACTOR_MAGIC;
        actor->actor_name = name;
        actor->vtable = vtable;
        actor->state = BIT_ACTOR_CREATED;
    }
    return actor;
}

int bit_actor_destroy(BitActor_t* actor) {
    if (actor) {
        free(actor);
        return 0;
    }
    return -1;
}

bool bit_actor_is_healthy(BitActor_t* actor) {
    return actor && actor->magic_header == BIT_ACTOR_MAGIC;
}

int main() {
    printf("CNS v8.0 Generated UHFT Code Test\n");
    printf("==================================\n\n");
    
    // Test basic type creation
    printf("Testing Trading Order creation...\n");
    Trading_Order_t* order = trading_order_create();
    if (!order) {
        printf("ERROR: Failed to create Trading Order\n");
        return 1;
    }
    
    // Set some properties
    order->order_price = 125.50;
    order->order_quantity = 1000;
    
    printf("✓ Trading Order created successfully\n");
    printf("  - Price: %.2f\n", order->order_price);
    printf("  - Quantity: %d\n", order->order_quantity);
    
    // Test validation
    printf("\nTesting validation...\n");
    uint64_t start_ticks = rdtsc();
    bool valid = trading_order_validate(order);
    uint64_t end_ticks = rdtsc();
    uint64_t validation_ticks = end_ticks - start_ticks;
    
    printf("✓ Validation result: %s\n", valid ? "VALID" : "INVALID");
    printf("✓ Validation ticks: %llu\n", validation_ticks);
    
    if (validation_ticks <= 8) {
        printf("✅ 8-TICK COMPLIANCE: PASSED\n");
    } else {
        printf("❌ 8-TICK COMPLIANCE: FAILED (%llu > 8)\n", validation_ticks);
    }
    
    // Benchmark validation performance
    printf("\nBenchmarking validation performance...\n");
    const int iterations = 1000000;
    
    start_ticks = rdtsc();
    for (int i = 0; i < iterations; i++) {
        bool result = trading_order_validate(order);
        __asm__ volatile("" : : "r"(result) : "memory"); // Prevent optimization
    }
    end_ticks = rdtsc();
    
    uint64_t total_ticks = end_ticks - start_ticks;
    double avg_ticks = (double)total_ticks / iterations;
    
    printf("✓ Total iterations: %d\n", iterations);
    printf("✓ Total ticks: %llu\n", total_ticks);
    printf("✓ Average ticks per validation: %.2f\n", avg_ticks);
    
    if (avg_ticks <= 8.0) {
        printf("✅ PERFORMANCE TARGET: ACHIEVED (%.2f ≤ 8.0 ticks)\n", avg_ticks);
    } else {
        printf("❌ PERFORMANCE TARGET: MISSED (%.2f > 8.0 ticks)\n", avg_ticks);
    }
    
    // OpenTelemetry-style mermaid output
    printf("\n```mermaid\n");
    printf("graph LR\n");
    printf("    A[Trading Order] --> B[Validation]\n");
    printf("    B --> C[Result: %s]\n", valid ? "Valid" : "Invalid");
    printf("    C --> D[Ticks: %.2f]\n", avg_ticks);
    printf("    D --> E[Target: ≤8 ticks]\n");
    if (avg_ticks <= 8.0) {
        printf("    E --> F[✅ PASSED]\n");
    } else {
        printf("    E --> F[❌ FAILED]\n");
    }
    printf("```\n");
    
    // Cleanup
    trading_order_destroy(order);
    printf("\n✓ Cleanup completed\n");
    
    return 0;
}