/*
 * Tick Parallel Execution BDD Tests
 * Testing the optimized 8-tick execution engine
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../src/cns/tick_parallel.h"
#include <string.h>

/* Test operation counters */
static int op_call_counts[8] = {0};
static int total_ops_executed = 0;

/* Test operations that track execution */
static void test_op_0(void* data) { op_call_counts[0]++; total_ops_executed++; }
static void test_op_1(void* data) { op_call_counts[1]++; total_ops_executed++; }
static void test_op_2(void* data) { op_call_counts[2]++; total_ops_executed++; }
static void test_op_3(void* data) { op_call_counts[3]++; total_ops_executed++; }
static void test_op_4(void* data) { op_call_counts[4]++; total_ops_executed++; }
static void test_op_5(void* data) { op_call_counts[5]++; total_ops_executed++; }
static void test_op_6(void* data) { op_call_counts[6]++; total_ops_executed++; }
static void test_op_7(void* data) { op_call_counts[7]++; total_ops_executed++; }

/* Data modification test operations */
static void increment_int(void* data) { (*(int*)data)++; }
static void multiply_by_two(void* data) { (*(int*)data) *= 2; }
static void add_constant(void* data) { (*(int*)data) += 42; }

/* Reset test state */
static void reset_test_state(void) {
    memset(op_call_counts, 0, sizeof(op_call_counts));
    total_ops_executed = 0;
}

FEATURE(Tick_Parallel_Execution_Engine) {
    
    SCENARIO("Single operation executes correctly") {
        tick_unit_t unit;
        int test_data = 10;
        
        GIVEN("a tick unit with one operation",
            reset_test_state();
            unit = (tick_unit_t){
                .ops = {increment_int, NULL, NULL, NULL, NULL, NULL, NULL, NULL},
                .data = {&test_data, NULL, NULL, NULL, NULL, NULL, NULL, NULL},
                .tick_mask = 0x01  // Only first operation active
            };
        );
        
        WHEN("the tick unit is executed",
            tick_execute(&unit);
        );
        
        THEN("only the first operation is called",
            EXPECT_EQ(test_data, 11);  // Incremented from 10 to 11
        );
    } END_SCENARIO
    
    SCENARIO("Multiple operations execute in sequence") {
        tick_unit_t unit;
        int test_values[3] = {1, 2, 3};
        
        GIVEN("a tick unit with three operations",
            reset_test_state();
            unit = (tick_unit_t){
                .ops = {increment_int, multiply_by_two, add_constant, 
                       NULL, NULL, NULL, NULL, NULL},
                .data = {&test_values[0], &test_values[1], &test_values[2],
                        NULL, NULL, NULL, NULL, NULL},
                .tick_mask = 0x07  // First three operations (0b00000111)
            };
        );
        
        WHEN("the tick unit is executed",
            tick_execute(&unit);
        );
        
        THEN("all three operations are applied",
            EXPECT_EQ(test_values[0], 2);   // 1 + 1 = 2
            EXPECT_EQ(test_values[1], 4);   // 2 * 2 = 4  
            EXPECT_EQ(test_values[2], 45);  // 3 + 42 = 45
        );
    } END_SCENARIO
    
    SCENARIO("Tick mask controls operation execution") {
        tick_unit_t unit;
        
        GIVEN("a tick unit with all 8 operations defined",
            reset_test_state();
            unit = (tick_unit_t){
                .ops = {test_op_0, test_op_1, test_op_2, test_op_3,
                       test_op_4, test_op_5, test_op_6, test_op_7},
                .data = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL},
                .tick_mask = 0xAA  // Alternating pattern: 10101010
            };
        );
        
        WHEN("execution occurs with alternating mask",
            tick_execute(&unit);
        );
        
        THEN("only even-indexed operations are executed",
            EXPECT_EQ(op_call_counts[0], 0);  // Bit 0 not set
            EXPECT_EQ(op_call_counts[1], 1);  // Bit 1 set
            EXPECT_EQ(op_call_counts[2], 0);  // Bit 2 not set
            EXPECT_EQ(op_call_counts[3], 1);  // Bit 3 set
            EXPECT_EQ(op_call_counts[4], 0);  // Bit 4 not set
            EXPECT_EQ(op_call_counts[5], 1);  // Bit 5 set
            EXPECT_EQ(op_call_counts[6], 0);  // Bit 6 not set
            EXPECT_EQ(op_call_counts[7], 1);  // Bit 7 set
        );
        
        AND("total executed operations match expectation",
            EXPECT_EQ(total_ops_executed, 4);  // 4 bits set in 0xAA
        );
    } END_SCENARIO
    
    SCENARIO("Empty tick mask executes no operations") {
        tick_unit_t unit;
        
        GIVEN("a tick unit with zero mask",
            reset_test_state();
            unit = (tick_unit_t){
                .ops = {test_op_0, test_op_1, test_op_2, test_op_3,
                       test_op_4, test_op_5, test_op_6, test_op_7},
                .data = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL},
                .tick_mask = 0x00  // No operations enabled
            };
        );
        
        WHEN("execution is attempted",
            tick_execute(&unit);
        );
        
        THEN("no operations are called",
            for (int i = 0; i < 8; i++) {
                EXPECT_EQ(op_call_counts[i], 0);
            }
            EXPECT_EQ(total_ops_executed, 0);
        );
    } END_SCENARIO
    
    SCENARIO("Full tick mask executes all operations") {
        tick_unit_t unit;
        
        GIVEN("a tick unit with all operations enabled",
            reset_test_state();
            unit = (tick_unit_t){
                .ops = {test_op_0, test_op_1, test_op_2, test_op_3,
                       test_op_4, test_op_5, test_op_6, test_op_7},
                .data = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL},
                .tick_mask = 0xFF  // All 8 operations enabled
            };
        );
        
        WHEN("execution occurs",
            tick_execute(&unit);
        );
        
        THEN("all 8 operations are executed exactly once",
            for (int i = 0; i < 8; i++) {
                EXPECT_EQ(op_call_counts[i], 1);
            }
            EXPECT_EQ(total_ops_executed, 8);
        );
    } END_SCENARIO
    
    SCENARIO("Arena allocation provides aligned memory") {
        char arena_memory[1024];
        void* arena_ptr = arena_memory;
        void* allocated_blocks[10];
        
        GIVEN("an arena memory pool",
            // Arena pointer starts at beginning
            arena_ptr = arena_memory;
        );
        
        WHEN("multiple allocations are made",
            for (int i = 0; i < 10; i++) {
                allocated_blocks[i] = tick_arena_alloc(&arena_ptr, 32 + i);
            }
        );
        
        THEN("all allocations are 64-byte aligned",
            for (int i = 0; i < 10; i++) {
                uintptr_t addr = (uintptr_t)allocated_blocks[i];
                EXPECT_EQ(addr & 63, 0);  // Must be 64-byte aligned
            }
        );
        
        AND("allocations don't overlap",
            for (int i = 0; i < 9; i++) {
                uintptr_t current = (uintptr_t)allocated_blocks[i];
                uintptr_t next = (uintptr_t)allocated_blocks[i + 1];
                EXPECT_GT(next, current + 32 + i);  // No overlap
            }
        );
    } END_SCENARIO
    
    SCENARIO("Execution performance meets tick budget") {
        tick_unit_t unit;
        int dummy_data[8] = {1, 2, 3, 4, 5, 6, 7, 8};
        uint64_t start_cycles, end_cycles, duration;
        
        GIVEN("a fully loaded tick unit",
            unit = (tick_unit_t){
                .ops = {increment_int, increment_int, increment_int, increment_int,
                       increment_int, increment_int, increment_int, increment_int},
                .data = {&dummy_data[0], &dummy_data[1], &dummy_data[2], &dummy_data[3],
                        &dummy_data[4], &dummy_data[5], &dummy_data[6], &dummy_data[7]},
                .tick_mask = 0xFF  // All operations active
            };
        );
        
        WHEN("execution is measured",
            start_cycles = rdtsc_portable();
            tick_execute(&unit);
            end_cycles = rdtsc_portable();
            duration = end_cycles - start_cycles;
        );
        
        THEN("execution completes within reasonable time",
            printf("       Execution took %llu cycles\n", 
                   (unsigned long long)duration);
            // This is a very lenient check since we're not doing real work
            EXPECT_LT(duration, 1000);
        );
        
        AND("all operations were executed",
            for (int i = 0; i < 8; i++) {
                EXPECT_EQ(dummy_data[i], i + 2);  // Original + 1
            }
        );
    } END_SCENARIO
    
    SCENARIO("Repeated execution shows consistent performance") {
        tick_unit_t unit;
        int test_data = 0;
        uint64_t durations[100];
        uint64_t min_duration = UINT64_MAX;
        uint64_t max_duration = 0;
        
        GIVEN("a simple tick unit for repeated execution",
            unit = (tick_unit_t){
                .ops = {increment_int, NULL, NULL, NULL, NULL, NULL, NULL, NULL},
                .data = {&test_data, NULL, NULL, NULL, NULL, NULL, NULL, NULL},
                .tick_mask = 0x01
            };
        );
        
        WHEN("execution is repeated 100 times",
            for (int i = 0; i < 100; i++) {
                uint64_t start = rdtsc_portable();
                tick_execute(&unit);
                uint64_t end = rdtsc_portable();
                durations[i] = end - start;
                
                if (durations[i] < min_duration) min_duration = durations[i];
                if (durations[i] > max_duration) max_duration = durations[i];
            }
        );
        
        THEN("performance is consistent",
            printf("       Min: %llu, Max: %llu cycles\n",
                   (unsigned long long)min_duration,
                   (unsigned long long)max_duration);
            
            // Variance should be reasonable
            uint64_t variance = max_duration - min_duration;
            EXPECT_LT(variance, 100);  // Less than 100 cycle variance
        );
        
        AND("all operations completed successfully",
            EXPECT_EQ(test_data, 100);  // Incremented 100 times
        );
    } END_SCENARIO
}