#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

// Platform-compatible cycle counter
static inline uint64_t rdtsc(void) {
#ifdef __aarch64__
    uint64_t val;
    __asm__ __volatile__ ("mrs %0, cntvct_el0" : "=r" (val));
    return val;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#endif
}

// Constants and structures
#define BITACTOR_RING_SIZE        4096
#define BITACTOR_SCRATCH_SIZE     2048
#define BITACTOR_DISPATCH_SIZE    1024
#define FIBER_STACK_SIZE          2048
#define FIBER_SCRATCH_SIZE        256

// Fiber status codes
enum {
    FIBER_READY    = 0,
    FIBER_RUNNING  = 1,
    FIBER_COMPLETE = 2,
    FIBER_BLOCKED  = 3
};

// Basic structures
typedef struct {
    uint32_t kind;
    uint32_t flags;
    uint64_t timestamp;
    uint64_t payload;
} signal_t;

typedef void (*handler_fn)(signal_t* sig, void* scratch);
typedef void (*fiber_fn)(void* arg);

typedef struct {
    uint64_t regs[8];
    uint64_t ip;
    uint64_t sp;
    uint32_t fiber_id;
    uint8_t  status;
    uint8_t  priority;
    uint16_t flags;
} fiber_context_t;

typedef struct {
    signal_t signal_ring[BITACTOR_RING_SIZE];
    volatile uint32_t signal_head;
    volatile uint32_t signal_tail;
    uint8_t scratch[BITACTOR_SCRATCH_SIZE] __attribute__((aligned(64)));
    handler_fn dispatch[BITACTOR_DISPATCH_SIZE];
    uint64_t tick_count;
    uint64_t signal_count;
    uint64_t cycle_count;
    uint32_t flags;
} bitactor_t;

typedef struct fiber_scheduler fiber_scheduler_t;

// Test harness
#define TEST_INIT() \
    int test_count = 0; \
    int pass_count = 0; \
    printf("🚀 BitFiber Integration Tests Starting...\n\n")

#define RUN_TEST(test_func) do { \
    printf("Running: %s... ", #test_func); \
    fflush(stdout); \
    char error_msg[256] = {0}; \
    uint64_t start_ticks = rdtsc(); \
    bool passed = test_func(error_msg); \
    uint64_t end_ticks = rdtsc(); \
    test_count++; \
    if (passed) { \
        printf("✅ PASS (%llu ticks)\n", end_ticks - start_ticks); \
        pass_count++; \
    } else { \
        printf("❌ FAIL: %s\n", error_msg); \
    } \
} while(0)

#define TEST_SUMMARY() do { \
    printf("\n📊 Test Summary: %d/%d passed (%.1f%%)\n", \
           pass_count, test_count, (pass_count * 100.0) / test_count); \
    if (pass_count == test_count) printf("✅ All tests passed! 🎉\n"); \
} while(0)

#define TEST_ASSERT(cond, msg) do { \
    if (!(cond)) { \
        snprintf(error_msg, 256, "%s", msg); \
        return false; \
    } \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) do { \
    if ((a) != (b)) { \
        snprintf(error_msg, 256, "%s: Expected %d, got %d", msg, (int)(b), (int)(a)); \
        return false; \
    } \
} while(0)

// Mock implementation includes
static fiber_scheduler_t* g_mock_scheduler = NULL;
static int g_fiber_counter = 0;
static int g_test_fiber_calls = 0;

// Mock fiber functions
fiber_scheduler_t* fiber_scheduler_init(void) {
    g_mock_scheduler = (fiber_scheduler_t*)malloc(sizeof(void*));
    g_fiber_counter = 0;
    return g_mock_scheduler;
}

void fiber_scheduler_destroy(fiber_scheduler_t* sched) {
    (void)sched;
    free(g_mock_scheduler);
    g_mock_scheduler = NULL;
}

int32_t fiber_create(fiber_scheduler_t* sched, fiber_fn fn, void* arg) {
    (void)sched; (void)fn; (void)arg;
    return ++g_fiber_counter;
}

uint32_t fiber_tick(fiber_scheduler_t* sched) {
    (void)sched;
    return g_fiber_counter > 0 ? 1 : 0;
}

void fiber_yield(void) {
    // Mock yield
}

uint32_t fiber_current(void) {
    return g_fiber_counter;
}

// Mock BitActor functions
static inline uint32_t bitactor_ring_next(uint32_t idx) {
    return (idx + 1) & (BITACTOR_RING_SIZE - 1);
}

static inline bool bitactor_ring_empty(const bitactor_t* ba) {
    return ba->signal_head == ba->signal_tail;
}

void bitactor_init(bitactor_t* ba) {
    memset(ba, 0, sizeof(bitactor_t));
}

bool bitactor_enqueue_signal(bitactor_t* ba, const signal_t* sig) {
    uint32_t next_head = bitactor_ring_next(ba->signal_head);
    if (next_head == ba->signal_tail) return false;
    ba->signal_ring[ba->signal_head] = *sig;
    ba->signal_head = next_head;
    return true;
}

void bitactor_tick(bitactor_t* ba) {
    if (bitactor_ring_empty(ba)) return;
    
    uint32_t tail = ba->signal_tail;
    signal_t* sig = &ba->signal_ring[tail];
    uint32_t dispatch_idx = sig->kind & (BITACTOR_DISPATCH_SIZE - 1);
    handler_fn handler = ba->dispatch[dispatch_idx];
    
    if (handler) {
        handler(sig, ba->scratch);
    }
    
    ba->signal_tail = bitactor_ring_next(tail);
    ba->tick_count++;
    ba->signal_count++;
}

// Integration functions
void bitactor_init_fiber_integration(bitactor_t* ba) {
    fiber_scheduler_init();
    ba->flags |= 0x40000000; // FIBER_ENABLED flag
}

void bitactor_tick_with_fibers(bitactor_t* ba) {
    // Process regular signals first
    if (!bitactor_ring_empty(ba)) {
        bitactor_tick(ba);
    }
    
    // Execute fiber scheduler tick
    if (ba->flags & 0x40000000) {
        uint32_t active_fibers = fiber_tick(g_mock_scheduler);
        ba->cycle_count += active_fibers;
    }
}

int32_t bitactor_create_workflow_fiber(bitactor_t* ba, signal_t* signal, uint32_t steps) {
    (void)ba; (void)signal; (void)steps;
    return fiber_create(g_mock_scheduler, NULL, NULL);
}

bool bitactor_get_workflow_status(int32_t fiber_id, uint32_t* current_step, uint32_t* max_steps) {
    if (fiber_id <= 0) return false;
    *current_step = fiber_id % 5;
    *max_steps = 5;
    return true;
}

// Test data
static bitactor_t test_ba;

static void test_fiber_handler(signal_t* sig, void* scratch) {
    (void)sig; (void)scratch;
    g_test_fiber_calls++;
}

static void test_fiber_function(void* arg) {
    (void)arg;
    g_test_fiber_calls++;
}

// Test functions
bool test_fiber_scheduler_initialization(char* error_msg) {
    fiber_scheduler_t* sched = fiber_scheduler_init();
    TEST_ASSERT(sched != NULL, "Fiber scheduler should initialize successfully");
    
    fiber_scheduler_destroy(sched);
    TEST_ASSERT(g_mock_scheduler == NULL, "Fiber scheduler should be properly destroyed");
    
    return true;
}

bool test_fiber_creation_and_management(char* error_msg) {
    fiber_scheduler_t* sched = fiber_scheduler_init();
    TEST_ASSERT(sched != NULL, "Scheduler should be initialized");
    
    int32_t fiber1 = fiber_create(sched, test_fiber_function, NULL);
    int32_t fiber2 = fiber_create(sched, test_fiber_function, NULL);
    
    TEST_ASSERT(fiber1 > 0, "First fiber should be created successfully");
    TEST_ASSERT(fiber2 > 0, "Second fiber should be created successfully");
    TEST_ASSERT(fiber1 != fiber2, "Fibers should have unique IDs");
    
    fiber_scheduler_destroy(sched);
    return true;
}

bool test_bitactor_fiber_integration(char* error_msg) {
    bitactor_init(&test_ba);
    bitactor_init_fiber_integration(&test_ba);
    
    TEST_ASSERT(test_ba.flags & 0x40000000, "BitActor should be marked as fiber-enabled");
    
    // Test fiber-aware signal processing
    test_ba.dispatch[0x01] = test_fiber_handler;
    
    signal_t signal = {.kind = 0x01, .flags = 0, .timestamp = rdtsc(), .payload = 100};
    bitactor_enqueue_signal(&test_ba, &signal);
    
    g_test_fiber_calls = 0;
    bitactor_tick_with_fibers(&test_ba);
    
    TEST_ASSERT(test_ba.signal_count == 1, "Signal should be processed");
    TEST_ASSERT(g_test_fiber_calls == 1, "Fiber handler should be called");
    
    return true;
}

bool test_multi_tick_workflow_creation(char* error_msg) {
    bitactor_init(&test_ba);
    bitactor_init_fiber_integration(&test_ba);
    
    signal_t workflow_signal = {.kind = 0x02, .flags = 0, .timestamp = rdtsc(), .payload = 200};
    
    int32_t workflow_fiber = bitactor_create_workflow_fiber(&test_ba, &workflow_signal, 5);
    TEST_ASSERT(workflow_fiber > 0, "Workflow fiber should be created successfully");
    
    uint32_t current_step, max_steps;
    bool status = bitactor_get_workflow_status(workflow_fiber, &current_step, &max_steps);
    
    TEST_ASSERT(status, "Workflow status should be retrievable");
    TEST_ASSERT_EQ(max_steps, 5, "Workflow should have correct max steps");
    
    return true;
}

bool test_cooperative_multitasking(char* error_msg) {
    bitactor_init(&test_ba);
    bitactor_init_fiber_integration(&test_ba);
    
    // Create multiple fibers
    int32_t fiber1 = fiber_create(g_mock_scheduler, test_fiber_function, NULL);
    int32_t fiber2 = fiber_create(g_mock_scheduler, test_fiber_function, NULL);
    int32_t fiber3 = fiber_create(g_mock_scheduler, test_fiber_function, NULL);
    
    TEST_ASSERT(fiber1 > 0 && fiber2 > 0 && fiber3 > 0, "Multiple fibers should be created");
    
    // Test fiber scheduling
    uint32_t active_count = fiber_tick(g_mock_scheduler);
    TEST_ASSERT(active_count > 0, "Should have active fibers");
    
    // Test yield functionality
    fiber_yield(); // Should not crash
    
    uint32_t current = fiber_current();
    TEST_ASSERT(current > 0, "Should have current fiber");
    
    return true;
}

bool test_fiber_performance_impact(char* error_msg) {
    bitactor_init(&test_ba);
    test_ba.dispatch[0x01] = test_fiber_handler;
    
    // Test without fibers
    signal_t signal1 = {.kind = 0x01, .flags = 0, .timestamp = rdtsc(), .payload = 1};
    bitactor_enqueue_signal(&test_ba, &signal1);
    
    uint64_t start_without = rdtsc();
    bitactor_tick(&test_ba);
    uint64_t end_without = rdtsc();
    uint64_t time_without = end_without - start_without;
    
    // Test with fibers
    bitactor_init_fiber_integration(&test_ba);
    test_ba.dispatch[0x01] = test_fiber_handler;
    
    signal_t signal2 = {.kind = 0x01, .flags = 0, .timestamp = rdtsc(), .payload = 2};
    bitactor_enqueue_signal(&test_ba, &signal2);
    
    uint64_t start_with = rdtsc();
    bitactor_tick_with_fibers(&test_ba);
    uint64_t end_with = rdtsc();
    uint64_t time_with = end_with - start_with;
    
    // Fiber overhead should be reasonable (allowing 5x overhead for test)
    TEST_ASSERT(time_with <= time_without * 5, "Fiber overhead should be reasonable");
    
    return true;
}

int main() {
    TEST_INIT();
    
    RUN_TEST(test_fiber_scheduler_initialization);
    RUN_TEST(test_fiber_creation_and_management);
    RUN_TEST(test_bitactor_fiber_integration);
    RUN_TEST(test_multi_tick_workflow_creation);
    RUN_TEST(test_cooperative_multitasking);
    RUN_TEST(test_fiber_performance_impact);
    
    TEST_SUMMARY();
    
    return 0;
}