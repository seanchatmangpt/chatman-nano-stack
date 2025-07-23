/*
 * Lightweight BDD Framework for BitActor
 * Zero-overhead behavior specifications for real-time systems
 */
#ifndef BITACTOR_BDD_FRAMEWORK_H
#define BITACTOR_BDD_FRAMEWORK_H

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

/* BDD Context Management */
typedef struct {
    const char* feature;
    const char* scenario;
    int assertions;
    int failures;
    bool scenario_failed;
    char failure_msg[256];
} bdd_context_t;

static bdd_context_t g_bdd_ctx = {0};

/* Core BDD Macros */
#define FEATURE(name) \
    static void feature_##name(void); \
    int main(void) { \
        printf("\n🧪 Feature: %s\n", #name); \
        printf("=====================================\n"); \
        g_bdd_ctx.feature = #name; \
        feature_##name(); \
        printf("\n📊 Results: %d assertions, %d failures\n", \
               g_bdd_ctx.assertions, g_bdd_ctx.failures); \
        return g_bdd_ctx.failures > 0 ? 1 : 0; \
    } \
    static void feature_##name(void)

#define SCENARIO(desc) \
    printf("\n📋 Scenario: %s\n", desc); \
    g_bdd_ctx.scenario = desc; \
    g_bdd_ctx.scenario_failed = false; \
    do

#define END_SCENARIO \
    while(0); \
    if (g_bdd_ctx.scenario_failed) { \
        printf("   ❌ FAILED: %s\n", g_bdd_ctx.failure_msg); \
    } else { \
        printf("   ✅ PASSED\n"); \
    }

/* Given/When/Then with inline execution */
#define GIVEN(desc, ...) \
    printf("   Given %s\n", desc); \
    __VA_ARGS__;

#define WHEN(desc, ...) \
    printf("   When %s\n", desc); \
    __VA_ARGS__;

#define THEN(desc, ...) \
    printf("   Then %s\n", desc); \
    __VA_ARGS__;

#define AND(desc, ...) \
    printf("   And %s\n", desc); \
    __VA_ARGS__;

/* Expectations with zero overhead */
#define EXPECT(expr) do { \
    g_bdd_ctx.assertions++; \
    if (!(expr)) { \
        g_bdd_ctx.failures++; \
        g_bdd_ctx.scenario_failed = true; \
        snprintf(g_bdd_ctx.failure_msg, 256, \
                "Expected: %s (line %d)", #expr, __LINE__); \
    } \
} while(0)

#define EXPECT_EQ(actual, expected) do { \
    g_bdd_ctx.assertions++; \
    if ((actual) != (expected)) { \
        g_bdd_ctx.failures++; \
        g_bdd_ctx.scenario_failed = true; \
        snprintf(g_bdd_ctx.failure_msg, 256, \
                "Expected %ld == %ld (line %d)", \
                (long)(actual), (long)(expected), __LINE__); \
    } \
} while(0)

#define EXPECT_LT(actual, limit) do { \
    g_bdd_ctx.assertions++; \
    if ((actual) >= (limit)) { \
        g_bdd_ctx.failures++; \
        g_bdd_ctx.scenario_failed = true; \
        snprintf(g_bdd_ctx.failure_msg, 256, \
                "Expected %ld < %ld (line %d)", \
                (long)(actual), (long)(limit), __LINE__); \
    } \
} while(0)

#define EXPECT_GT(actual, limit) do { \
    g_bdd_ctx.assertions++; \
    if ((actual) <= (limit)) { \
        g_bdd_ctx.failures++; \
        g_bdd_ctx.scenario_failed = true; \
        snprintf(g_bdd_ctx.failure_msg, 256, \
                "Expected %ld > %ld (line %d)", \
                (long)(actual), (long)(limit), __LINE__); \
    } \
} while(0)

/* Performance measurement helpers */
static inline uint64_t measure_ticks(void (*fn)(void)) {
    uint64_t start = __builtin_readcyclecounter();
    fn();
    return __builtin_readcyclecounter() - start;
}

/* Memory tracking helpers */
#define TRACK_MEMORY_START() \
    size_t _mem_before = get_current_memory_usage()

#define TRACK_MEMORY_END() \
    size_t _mem_after = get_current_memory_usage(); \
    size_t _mem_delta = _mem_after - _mem_before

/* Mock/Stub helpers */
#define MOCK_FUNCTION(ret_type, name, ...) \
    static int name##_call_count = 0; \
    static ret_type name##_return_value; \
    ret_type name(__VA_ARGS__) { \
        name##_call_count++; \
        return name##_return_value; \
    }

#define STUB_FUNCTION(ret_type, name, ...) \
    ret_type name(__VA_ARGS__)

#define VERIFY_CALLED(func, times) \
    EXPECT_EQ(func##_call_count, times)

#define WHEN_CALLED(func, ret_val) \
    func##_return_value = ret_val

/* Test data builders */
#define BUILD_SIGNAL(...) \
    (signal_t){ __VA_ARGS__ }

#define BUILD_RESULT(...) \
    (result_t){ __VA_ARGS__ }

/* Inline cycle counter that works across architectures */
static inline uint64_t rdtsc_portable(void) {
#if defined(__x86_64__) || defined(__i386__)
    unsigned int lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
#elif defined(__aarch64__)
    uint64_t val;
    __asm__ __volatile__ ("mrs %0, cntvct_el0" : "=r" (val));
    return val;
#else
    return 0; // Fallback
#endif
}

#endif /* BITACTOR_BDD_FRAMEWORK_H */