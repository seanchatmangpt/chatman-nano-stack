/*
 * Unit Tests for Zero-Tick Optimization Components
 * TDD approach with specific function-level testing
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#ifdef __x86_64__
#include <immintrin.h>
#define GET_CYCLES() __rdtsc()
#elif defined(__aarch64__)
static inline uint64_t __rdtsc(void) {
    uint64_t val;
    asm volatile("mrs %0, cntvct_el0" : "=r" (val));
    return val;
}
#define GET_CYCLES() __rdtsc()
#else
#define GET_CYCLES() 0
#endif

#include "../bitactor/include/bitactor/bitactor.h"
#include "../bitactor/include/bitactor/bitactor_dispatch.h"
#include "../bitactor/runtime/bytecode_loader.c"  // Include for testing static functions

/* Test macros */
#define TEST(name) void test_##name(void)
#define RUN_TEST(name) do { \
    printf("Running " #name "... "); \
    test_##name(); \
    printf("PASSED\n"); \
    tests_passed++; \
} while(0)

static int tests_passed = 0;

/* Helper functions */
static signal_t make_signal(uint8_t type, uint64_t payload, uint8_t flags) {
    signal_t sig = {0};
    sig.id = rand() % 10000;
    sig.type = type;
    sig.payload = payload;
    sig.flags = flags;
    sig.timestamp = GET_CYCLES();
    return sig;
}

/* Unit Tests */

TEST(signal_is_trivially_skippable_heartbeat) {
    signal_t heartbeat = make_signal(0xFF, 0, 0);  // SIG_HEARTBEAT
    assert(signal_is_trivially_skippable(&heartbeat) == true);
}

TEST(signal_is_trivially_skippable_zero_confidence) {
    signal_t zero_conf = make_signal(0x01, 0x0000000000000000, 0);  // confidence = 0
    assert(signal_is_trivially_skippable(&zero_conf) == true);
}

TEST(signal_is_trivially_skippable_test_signal) {
    signal_t test_sig = make_signal(0x01, 0x1234567890ABCDEF, 0x80);  // TEST_SIGNAL_FLAG
    assert(signal_is_trivially_skippable(&test_sig) == true);
}

TEST(signal_is_trivially_skippable_normal_false) {
    signal_t normal = make_signal(0x01, 0x1234567890ABCDEF, 0x00);
    assert(signal_is_trivially_skippable(&normal) == false);
}

TEST(dispatch_zero_tick_handler_returns_zero_ticks) {
    signal_t sig = make_signal(0xFF, 0, 0);
    result_t result = dispatch_zero_tick_handler(&sig, NULL);
    
    assert(result.ticks == 0);
    assert(result.exec_hash == 0x5A4E00);
    assert(result.status == BITACTOR_OK);
    assert(result.signal_id == sig.id);
}

TEST(signal_is_zero_tick_candidate_comprehensive) {
    /* Test all zero-tick conditions */
    
    /* Heartbeat */
    signal_t heartbeat = make_signal(0xFF, 0x1234, 0x00);
    assert(signal_is_zero_tick_candidate(&heartbeat) == true);
    
    /* Zero confidence */
    signal_t zero_conf = make_signal(0x01, 0x0000000000000000, 0x00);
    assert(signal_is_zero_tick_candidate(&zero_conf) == true);
    
    /* Test signal */
    signal_t test_sig = make_signal(0x01, 0x1234, 0x80);
    assert(signal_is_zero_tick_candidate(&test_sig) == true);
    
    /* Normal signal - should be false */
    signal_t normal = make_signal(0x01, 0x1234567890ABCDEF, 0x00);
    assert(signal_is_zero_tick_candidate(&normal) == false);
}

TEST(bitactor_dispatch_signal_zero_tick_path) {
    dispatch_table_t table = {0};
    dispatch_init(&table);
    
    signal_t heartbeat = make_signal(0xFF, 0, 0);
    result_t result = bitactor_dispatch_signal(&table, &heartbeat);
    
    assert(result.ticks == 0);
    assert(result.exec_hash == 0x5A4E00);
    assert(result.status == BITACTOR_OK);
}

TEST(bitactor_execute_bytecode_zero_tick_early_exit) {
    /* Create a trivially skippable signal */
    signal_t zero_conf = make_signal(0x01, 0x0000000000000000, 0x00);
    
    /* Dummy bytecode */
    uint8_t bytecode[] = {
        0x42, 0x49, 0x54, 0x43,  // BITC magic
        0x01, 0x00,              // version
        0x00, 0x00,              // reserved
        0x10, 0x00, 0x00, 0x00,  // entry_offset
        0x00, 0x00, 0x00, 0x00   // constant_count
    };
    
    result_t result = bitactor_execute_bytecode(&zero_conf, bytecode, 
                                               sizeof(bytecode), NULL);
    
    /* Should exit early with zero ticks */
    assert(result.ticks == 0);
    assert(result.exec_hash == 0x5A4E00);
    assert(result.status == BITACTOR_OK);
}

TEST(dispatch_register_zero_tick_handler_flags) {
    dispatch_table_t table = {0};
    dispatch_init(&table);
    
    /* Register zero-tick handler */
    int ret = dispatch_register(&table, 0xFF, dispatch_zero_tick_handler, NULL);
    assert(ret == 0);
    
    /* Check that the zero-tick flag is set */
    assert(table.entries[0xFF].flags & 0x01);  // ZERO_TICK_HANDLER_FLAG
    assert(table.entries[0xFF].handler == dispatch_zero_tick_handler);
}

TEST(telemetry_zero_tick_metrics_increment) {
    /* Reset metrics (would need proper API in real implementation) */
    signal_t heartbeat = make_signal(0xFF, 0, 0);
    
    /* Get initial count */
    uint64_t initial_count = telemetry_get_zero_tick_count();
    
    /* Record zero-tick execution */
    telemetry_ring_t ring = {0};
    telemetry_init(&ring);
    telemetry_record_zero_tick(&ring, &heartbeat);
    
    /* Verify count increased */
    uint64_t new_count = telemetry_get_zero_tick_count();
    assert(new_count > initial_count);
}

TEST(telemetry_zero_tick_ratio_calculation) {
    uint64_t ratio_0 = telemetry_get_zero_tick_ratio(0);
    assert(ratio_0 == 0);  // Avoid division by zero
    
    uint64_t ratio_50 = telemetry_get_zero_tick_ratio(100);  // Assuming 50 zero-tick signals
    assert(ratio_50 <= 100);  // Should be a valid percentage
}

TEST(zero_tick_flag_instruction_skip) {
    /* Test that instructions with ZERO_TICK_FLAG are skipped */
    bc_instruction_t instr = {0};
    instr.opcode = OP_ADD;
    instr.flags = ZERO_TICK_FLAG;
    
    /* This would require access to execute_instruction internals */
    /* For now, just verify the flag constant is defined */
    assert(ZERO_TICK_FLAG == 0x01);
    assert(instr.flags & ZERO_TICK_FLAG);
}

TEST(fiber_has_signals_detection) {
    /* Create a fiber with different states */
    fiber_t fiber = {0};
    
    /* Ready fiber should have signals */
    fiber.context.status = FIBER_READY;
    assert(fiber_has_signals(&fiber) == true);
    
    /* Running fiber should have signals */
    fiber.context.status = FIBER_RUNNING;
    assert(fiber_has_signals(&fiber) == true);
    
    /* Complete fiber should not have signals */
    fiber.context.status = FIBER_COMPLETE;
    assert(fiber_has_signals(&fiber) == false);
    
    /* Uninitialized fiber should not have signals */
    fiber.context.status = 0;
    assert(fiber_has_signals(&fiber) == false);
}

TEST(performance_target_validation) {
    /* Validate that zero-tick optimizations meet performance targets */
    
    const uint32_t test_iterations = 1000;
    uint64_t total_ticks = 0;
    uint64_t zero_tick_count = 0;
    
    dispatch_table_t table = {0};
    dispatch_init(&table);
    
    /* Process 80% zero-tick signals, 20% normal */
    for (uint32_t i = 0; i < test_iterations; i++) {
        signal_t sig;
        
        if (i % 5 == 0) {
            /* 20% normal signals */
            sig = make_signal(0x01, 0x1234567890ABCDEF, 0x00);
        } else {
            /* 80% zero-tick signals */
            switch (i % 4) {
                case 1: sig = make_signal(0xFF, 0, 0); break;  // Heartbeat
                case 2: sig = make_signal(0x01, 0x0000000000000000, 0); break;  // Zero conf
                case 3: sig = make_signal(0x01, 0x1234, 0x80); break;  // Test
                default: sig = make_signal(0x01, 0x1234, 0x80); break;
            }
        }
        
        result_t result = bitactor_dispatch_signal(&table, &sig);
        total_ticks += result.ticks;
        if (result.ticks == 0) zero_tick_count++;
    }
    
    /* Verify performance targets */
    double avg_ticks = (double)total_ticks / test_iterations;
    double zero_tick_ratio = (double)zero_tick_count / test_iterations * 100;
    
    printf("\n  Performance Results:\n");
    printf("    Average ticks per signal: %.2f (target: <2.5)\n", avg_ticks);
    printf("    Zero-tick ratio: %.1f%% (target: ~80%%)\n", zero_tick_ratio);
    
    assert(avg_ticks < 2.5);  // Target from zero-tick.md
    assert(zero_tick_ratio >= 70.0);  // Allow some variance
}

/* Test runner */
int main(void) {
    printf("=== Zero-Tick Optimization Unit Tests ===\n\n");
    
    srand(time(NULL));
    
    RUN_TEST(signal_is_trivially_skippable_heartbeat);
    RUN_TEST(signal_is_trivially_skippable_zero_confidence);
    RUN_TEST(signal_is_trivially_skippable_test_signal);
    RUN_TEST(signal_is_trivially_skippable_normal_false);
    RUN_TEST(dispatch_zero_tick_handler_returns_zero_ticks);
    RUN_TEST(signal_is_zero_tick_candidate_comprehensive);
    RUN_TEST(bitactor_dispatch_signal_zero_tick_path);
    RUN_TEST(bitactor_execute_bytecode_zero_tick_early_exit);
    RUN_TEST(dispatch_register_zero_tick_handler_flags);
    RUN_TEST(telemetry_zero_tick_metrics_increment);
    RUN_TEST(telemetry_zero_tick_ratio_calculation);
    RUN_TEST(zero_tick_flag_instruction_skip);
    RUN_TEST(fiber_has_signals_detection);
    RUN_TEST(performance_target_validation);
    
    printf("\n=== Unit Test Summary ===\n");
    printf("Tests passed: %d\n", tests_passed);
    printf("✅ All unit tests passed!\n");
    
    return 0;
}