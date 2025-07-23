#ifndef BITACTOR_TEST_HARNESS_H
#define BITACTOR_TEST_HARNESS_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>
#include <assert.h>

// CPU tick counter for cycle-accurate measurements
static inline uint64_t rdtsc(void) {
#if defined(__x86_64__) || defined(__i386__)
    unsigned int lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
#elif defined(__aarch64__)
    uint64_t val;
    __asm__ __volatile__ ("mrs %0, cntvct_el0" : "=r" (val));
    return val;
#else
    // Fallback for other architectures
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#endif
}

// Test result tracking
typedef struct {
    const char* test_name;
    bool passed;
    uint64_t start_ticks;
    uint64_t end_ticks;
    char message[256];
} test_result_t;

// Test runner macros
#define TEST_INIT() \
    int test_count = 0; \
    int pass_count = 0; \
    int fail_count = 0; \
    test_result_t results[100]; \
    printf("🚀 BitActor Test Suite Starting...\n\n")

#define RUN_TEST(test_func) do { \
    printf("Running: %s... ", #test_func); \
    fflush(stdout); \
    test_result_t* result = &results[test_count]; \
    result->test_name = #test_func; \
    result->start_ticks = rdtsc(); \
    result->passed = test_func(&result->message[0]); \
    result->end_ticks = rdtsc(); \
    test_count++; \
    if (result->passed) { \
        printf("✅ PASS\n"); \
        pass_count++; \
    } else { \
        printf("❌ FAIL\n    %s\n", result->message); \
        fail_count++; \
    } \
} while(0)

#define TEST_SUMMARY() do { \
    printf("\n📊 Test Summary:\n"); \
    printf("Total: %d | Pass: %d | Fail: %d\n", test_count, pass_count, fail_count); \
    printf("Success Rate: %.1f%%\n", (pass_count * 100.0) / test_count); \
    \
    if (fail_count == 0) { \
        printf("\n✅ All tests passed! 🎉\n"); \
    } else { \
        printf("\n❌ %d tests failed\n", fail_count); \
        exit(1); \
    } \
} while(0)

// Test assertion helpers
#define TEST_ASSERT(cond, msg) do { \
    if (!(cond)) { \
        snprintf(error_msg, 256, "Assertion failed: %s", msg); \
        return false; \
    } \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) do { \
    if ((a) != (b)) { \
        snprintf(error_msg, 256, "%s: Expected %ld, got %ld", msg, (long)(b), (long)(a)); \
        return false; \
    } \
} while(0)

#define TEST_ASSERT_LT(a, b, msg) do { \
    if ((a) >= (b)) { \
        snprintf(error_msg, 256, "%s: %ld not less than %ld", msg, (long)(a), (long)(b)); \
        return false; \
    } \
} while(0)

#define TEST_ASSERT_LE(a, b, msg) do { \
    if ((a) > (b)) { \
        snprintf(error_msg, 256, "%s: %ld not less than or equal to %ld", msg, (long)(a), (long)(b)); \
        return false; \
    } \
} while(0)

// Memory tracking helpers
typedef struct {
    size_t allocated;
    size_t freed;
    int alloc_count;
    int free_count;
} memory_stats_t;

extern memory_stats_t g_memory_stats;

// Override malloc/free for memory tracking
#ifdef TRACK_MEMORY
void* tracked_malloc(size_t size);
void tracked_free(void* ptr);
#define malloc(size) tracked_malloc(size)
#define free(ptr) tracked_free(ptr)
#endif

#endif // BITACTOR_TEST_HARNESS_H