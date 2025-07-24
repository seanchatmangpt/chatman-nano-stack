/*
 * Real BitFiber Scheduler Implementation BDD Tests
 * Testing the actual fiber scheduling and context switching
 * NO MOCKS - Real fiber scheduler testing
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../bitactor/src/bitfiber.h"
#include "../bitactor/include/bitactor_public.h"
#include <string.h>
#include <stdlib.h>

// Test fiber contexts
typedef struct {
    int fiber_id;
    int execution_count;
    int yield_count;
    uint64_t total_ticks;
    bool completed;
} test_fiber_context_t;

// Global test state
static int total_fiber_switches = 0;
static uint64_t context_switch_times[100];
static int switch_count = 0;

// Test fiber functions
static void test_fiber_simple(void* context) {
    test_fiber_context_t* ctx = (test_fiber_context_t*)context;
    
    for (int i = 0; i < 5; i++) {
        uint64_t start = rdtsc_portable();
        ctx->execution_count++;
        
        // Simulate work
        volatile int sum = 0;
        for (int j = 0; j < 100; j++) {
            sum += j;
        }
        
        uint64_t before_yield = rdtsc_portable();
        fiber_yield();
        uint64_t after_yield = rdtsc_portable();
        
        ctx->yield_count++;
        ctx->total_ticks += (before_yield - start);
        
        if (switch_count < 100) {
            context_switch_times[switch_count++] = after_yield - before_yield;
        }
    }
    
    ctx->completed = true;
}

static void test_fiber_priority(void* context) {
    test_fiber_context_t* ctx = (test_fiber_context_t*)context;
    
    // High priority fiber does more work per slice
    for (int i = 0; i < 10; i++) {
        uint64_t start = rdtsc_portable();
        ctx->execution_count++;
        
        // More intensive work for priority test
        volatile int sum = 0;
        for (int j = 0; j < 200; j++) {
            sum += j * j;
        }
        
        uint64_t end = rdtsc_portable();
        ctx->total_ticks += (end - start);
        
        if (i % 3 == 0) {
            fiber_yield();
            ctx->yield_count++;
        }
    }
    
    ctx->completed = true;
}

FEATURE(BitFiber_Real_Implementation) {
    
    SCENARIO("Fiber scheduler initialization and configuration") {
        fiber_scheduler_t* scheduler = NULL;
        
        GIVEN("uninitialized fiber scheduler",
            scheduler = NULL;
        );
        
        WHEN("scheduler is initialized",
            uint64_t start = rdtsc_portable();
            scheduler = fiber_scheduler_init();
            uint64_t end = rdtsc_portable();
            uint64_t init_time = end - start;
        );
        
        THEN("scheduler initializes with proper state",
            EXPECT_NE(scheduler, NULL);
            EXPECT(scheduler->initialized);
            EXPECT_EQ(scheduler->active_fiber_count, 0);
            EXPECT_EQ(scheduler->current_fiber, NULL);
            EXPECT_NE(scheduler->fiber_pool, NULL);
            EXPECT_GT(scheduler->max_fibers, 0);
            EXPECT_LE(scheduler->max_fibers, BITACTOR_MAX_FIBERS);
            
            printf("       Scheduler init time: %llu ticks\n",
                   (unsigned long long)init_time);
            printf("       Max fibers: %u\n", scheduler->max_fibers);
        );
        
        AND("fiber pool is properly allocated",
            // Verify fiber pool structure
            for (uint32_t i = 0; i < scheduler->max_fibers; i++) {
                fiber_t* fiber = &scheduler->fiber_pool[i];
                EXPECT_EQ(fiber->state, FIBER_STATE_FREE);
                EXPECT_EQ(fiber->id, i);
                EXPECT_NE(fiber->stack, NULL);
                EXPECT_GT(fiber->stack_size, 0);
            }
            
            fiber_scheduler_free(scheduler);
        );
    } END_SCENARIO
    
    SCENARIO("Fiber creation and lifecycle management") {
        fiber_scheduler_t* scheduler = fiber_scheduler_init();
        test_fiber_context_t contexts[3];
        
        GIVEN("scheduler with test contexts",
            EXPECT_NE(scheduler, NULL);
            
            // Initialize test contexts
            for (int i = 0; i < 3; i++) {
                contexts[i] = (test_fiber_context_t){
                    .fiber_id = i,
                    .execution_count = 0,
                    .yield_count = 0,
                    .total_ticks = 0,
                    .completed = false
                };
            }
        );
        
        WHEN("fibers are created with different functions",
            fiber_t* fibers[3];
            uint64_t creation_times[3];
            
            for (int i = 0; i < 3; i++) {
                uint64_t start = rdtsc_portable();
                
                if (i == 0) {
                    fibers[i] = fiber_create(scheduler, 0); // Normal priority
                    fiber_set_function(fibers[i], test_fiber_simple, &contexts[i]);
                } else {
                    fibers[i] = fiber_create(scheduler, i); // Varying priorities
                    fiber_set_function(fibers[i], test_fiber_priority, &contexts[i]);
                }
                
                uint64_t end = rdtsc_portable();
                creation_times[i] = end - start;
            }
        );
        
        THEN("fiber creation completes within tick budget",
            for (int i = 0; i < 3; i++) {
                EXPECT_NE(fibers[i], NULL);
                EXPECT_LE(creation_times[i], 8);
                EXPECT_EQ(fibers[i]->state, FIBER_STATE_READY);
                EXPECT_NE(fibers[i]->context, NULL);
                EXPECT_EQ(fibers[i]->priority, (i == 0) ? 0 : i);
                
                printf("       Fiber %d creation: %llu ticks\n",
                       i, (unsigned long long)creation_times[i]);
            }
            
            EXPECT_EQ(scheduler->active_fiber_count, 3);
        );
        
        AND("fiber properties are correctly set",
            for (int i = 0; i < 3; i++) {
                EXPECT_EQ(fibers[i]->context, &contexts[i]);
                EXPECT_NE(fibers[i]->stack, NULL);
                EXPECT_GT(fibers[i]->stack_size, 0);
                EXPECT_EQ(fibers[i]->scheduler, scheduler);
            }
            
            fiber_scheduler_free(scheduler);
        );
    } END_SCENARIO
    
    SCENARIO("Cooperative fiber scheduling and context switching") {
        fiber_scheduler_t* scheduler = fiber_scheduler_init();
        test_fiber_context_t contexts[2];
        
        GIVEN("scheduler with two cooperative fibers",
            // Initialize contexts
            for (int i = 0; i < 2; i++) {
                contexts[i] = (test_fiber_context_t){
                    .fiber_id = i,
                    .execution_count = 0,
                    .yield_count = 0,
                    .total_ticks = 0,
                    .completed = false
                };
            }
            
            // Create fibers
            fiber_t* fiber1 = fiber_create(scheduler, 0);
            fiber_t* fiber2 = fiber_create(scheduler, 0);
            
            fiber_set_function(fiber1, test_fiber_simple, &contexts[0]);
            fiber_set_function(fiber2, test_fiber_simple, &contexts[1]);
            
            EXPECT_NE(fiber1, NULL);
            EXPECT_NE(fiber2, NULL);
        );
        
        WHEN("fibers execute cooperatively",
            // Reset global counters
            total_fiber_switches = 0;
            switch_count = 0;
            
            uint64_t start = rdtsc_portable();
            
            // Run scheduler until fibers complete
            int max_ticks = 100;
            int tick_count = 0;
            
            while (!contexts[0].completed || !contexts[1].completed) {
                fiber_tick(scheduler);
                tick_count++;
                
                if (tick_count >= max_ticks) break; // Safety limit
            }
            
            uint64_t end = rdtsc_portable();
            uint64_t total_time = end - start;
        );
        
        THEN("cooperative scheduling works efficiently",
            EXPECT(contexts[0].completed);
            EXPECT(contexts[1].completed);
            
            // Verify both fibers executed
            EXPECT_GT(contexts[0].execution_count, 0);
            EXPECT_GT(contexts[1].execution_count, 0);
            EXPECT_GT(contexts[0].yield_count, 0);
            EXPECT_GT(contexts[1].yield_count, 0);
            
            printf("       Cooperative execution:\n");
            printf("         Fiber 0: %d executions, %d yields\n",
                   contexts[0].execution_count, contexts[0].yield_count);
            printf("         Fiber 1: %d executions, %d yields\n",
                   contexts[1].execution_count, contexts[1].yield_count);
            printf("         Total scheduler time: %llu ticks\n",
                   (unsigned long long)total_time);
        );
        
        AND("context switches are fast",
            if (switch_count > 0) {
                uint64_t total_switch_time = 0;
                uint64_t max_switch_time = 0;
                
                for (int i = 0; i < switch_count; i++) {
                    total_switch_time += context_switch_times[i];
                    if (context_switch_times[i] > max_switch_time) {
                        max_switch_time = context_switch_times[i];
                    }
                }
                
                uint64_t avg_switch_time = total_switch_time / switch_count;
                
                printf("         Context switches: %d\n", switch_count);
                printf("         Avg switch time: %llu ticks\n",
                       (unsigned long long)avg_switch_time);
                printf("         Max switch time: %llu ticks\n",
                       (unsigned long long)max_switch_time);
                
                EXPECT_LE(avg_switch_time, 3); // Context switch should be very fast
                EXPECT_LE(max_switch_time, 8);
            }
            
            fiber_scheduler_free(scheduler);
        );
    } END_SCENARIO
    
    SCENARIO("Priority-based fiber scheduling") {
        fiber_scheduler_t* scheduler = fiber_scheduler_init();
        test_fiber_context_t contexts[3];
        
        GIVEN("fibers with different priorities",
            // Initialize contexts
            for (int i = 0; i < 3; i++) {
                contexts[i] = (test_fiber_context_t){
                    .fiber_id = i,
                    .execution_count = 0,
                    .yield_count = 0,
                    .total_ticks = 0,
                    .completed = false
                };
            }
            
            // Create fibers with different priorities
            fiber_t* low_priority = fiber_create(scheduler, 1);    // Low priority
            fiber_t* med_priority = fiber_create(scheduler, 5);    // Medium priority
            fiber_t* high_priority = fiber_create(scheduler, 10);  // High priority
            
            fiber_set_function(low_priority, test_fiber_priority, &contexts[0]);
            fiber_set_function(med_priority, test_fiber_priority, &contexts[1]);
            fiber_set_function(high_priority, test_fiber_priority, &contexts[2]);
        );
        
        WHEN("scheduler runs with priority scheduling",
            uint64_t start = rdtsc_portable();
            
            // Run scheduler for fixed number of ticks
            for (int i = 0; i < 50; i++) {
                fiber_tick(scheduler);
            }
            
            uint64_t end = rdtsc_portable();
            uint64_t scheduling_time = end - start;
        );
        
        THEN("higher priority fibers get more execution time",
            EXPECT_GT(contexts[2].execution_count, contexts[1].execution_count); // High > Med
            EXPECT_GT(contexts[1].execution_count, contexts[0].execution_count); // Med > Low
            
            printf("       Priority scheduling results:\n");
            printf("         Low priority (1):  %d executions\n", contexts[0].execution_count);
            printf("         Med priority (5):  %d executions\n", contexts[1].execution_count);
            printf("         High priority (10): %d executions\n", contexts[2].execution_count);
            printf("         Total scheduling time: %llu ticks\n",
                   (unsigned long long)scheduling_time);
            
            // Verify priority ratios are reasonable
            if (contexts[0].execution_count > 0) {
                double high_to_low_ratio = (double)contexts[2].execution_count / contexts[0].execution_count;
                EXPECT_GT(high_to_low_ratio, 2.0); // High priority should get at least 2x more time
            }
            
            fiber_scheduler_free(scheduler);
        );
    } END_SCENARIO
    
    SCENARIO("Fiber yield and resume mechanics") {
        fiber_scheduler_t* scheduler = fiber_scheduler_init();
        
        GIVEN("fiber with yield points",
            test_fiber_context_t context = {
                .fiber_id = 0,
                .execution_count = 0,
                .yield_count = 0,
                .total_ticks = 0,
                .completed = false
            };
            
            fiber_t* fiber = fiber_create(scheduler, 5);
            fiber_set_function(fiber, test_fiber_simple, &context);
        );
        
        WHEN("fiber yields and resumes multiple times",
            uint64_t yield_times[10];
            int yield_count = 0;
            
            while (!context.completed && yield_count < 10) {
                uint64_t start = rdtsc_portable();
                fiber_tick(scheduler);
                uint64_t end = rdtsc_portable();
                
                yield_times[yield_count] = end - start;
                yield_count++;
            }
        );
        
        THEN("yield/resume cycle is consistent and fast",
            EXPECT(context.completed);
            EXPECT_GT(context.yield_count, 0);
            EXPECT_EQ(context.yield_count, 5); // test_fiber_simple yields 5 times
            
            // Verify yield timing consistency
            uint64_t total_yield_time = 0;
            uint64_t max_yield_time = 0;
            
            for (int i = 0; i < context.yield_count; i++) {
                total_yield_time += yield_times[i];
                if (yield_times[i] > max_yield_time) {
                    max_yield_time = yield_times[i];
                }
            }
            
            uint64_t avg_yield_time = total_yield_time / context.yield_count;
            
            printf("       Yield/Resume mechanics:\n");
            printf("         Yields completed: %d\n", context.yield_count);
            printf("         Avg yield cycle: %llu ticks\n",
                   (unsigned long long)avg_yield_time);
            printf("         Max yield cycle: %llu ticks\n",
                   (unsigned long long)max_yield_time);
            
            EXPECT_LE(avg_yield_time, 8);
            EXPECT_LE(max_yield_time, 16);
            
            fiber_scheduler_free(scheduler);
        );
    } END_SCENARIO
    
    SCENARIO("Fiber completion and cleanup") {
        fiber_scheduler_t* scheduler = fiber_scheduler_init();
        
        GIVEN("multiple fibers in various states",
            test_fiber_context_t contexts[4];
            fiber_t* fibers[4];
            
            for (int i = 0; i < 4; i++) {
                contexts[i] = (test_fiber_context_t){
                    .fiber_id = i,
                    .execution_count = 0,
                    .yield_count = 0,
                    .total_ticks = 0,
                    .completed = false
                };
                
                fibers[i] = fiber_create(scheduler, i + 1);
                fiber_set_function(fibers[i], test_fiber_simple, &contexts[i]);
            }
            
            EXPECT_EQ(scheduler->active_fiber_count, 4);
        );
        
        WHEN("fibers complete and are cleaned up",
            // Run scheduler until all fibers complete
            int max_iterations = 200;
            int iterations = 0;
            
            while (scheduler->active_fiber_count > 0 && iterations < max_iterations) {
                fiber_tick(scheduler);
                iterations++;
            }
        );
        
        THEN("completed fibers are properly cleaned up",
            // All fibers should be completed
            for (int i = 0; i < 4; i++) {
                EXPECT(contexts[i].completed);
                EXPECT_EQ(fibers[i]->state, FIBER_STATE_COMPLETED);
            }
            
            // Scheduler should have cleaned up
            EXPECT_EQ(scheduler->active_fiber_count, 0);
            EXPECT_EQ(scheduler->current_fiber, NULL);
            
            printf("       Fiber completion:\n");
            printf("         All fibers completed: %s\n", 
                   (scheduler->active_fiber_count == 0) ? "YES" : "NO");
            printf("         Iterations required: %d\n", iterations);
            
            fiber_scheduler_free(scheduler);
        );
    } END_SCENARIO
    
    SCENARIO("Scheduler integration with BitActor ticks") {
        fiber_scheduler_t* scheduler = fiber_scheduler_init();
        bitactor_engine* engine = bitactor_init();
        
        GIVEN("fiber scheduler integrated with BitActor engine",
            EXPECT_NE(scheduler, NULL);
            EXPECT_NE(engine, NULL);
            
            // Integrate scheduler with engine
            engine->fiber_sched = scheduler;
        );
        
        WHEN("BitActor tick processes fibers",
            // Create test fiber
            test_fiber_context_t context = {
                .fiber_id = 0,
                .execution_count = 0,
                .yield_count = 0,
                .total_ticks = 0,
                .completed = false
            };
            
            fiber_t* fiber = fiber_create(scheduler, 5);
            fiber_set_function(fiber, test_fiber_simple, &context);
            
            // Run integrated ticks
            uint64_t total_time = 0;
            int tick_count = 0;
            
            while (!context.completed && tick_count < 20) {
                uint64_t start = rdtsc_portable();
                
                // BitActor tick includes fiber processing
                bitactor_tick(engine);
                
                uint64_t end = rdtsc_portable();
                total_time += (end - start);
                tick_count++;
            }
        );
        
        THEN("integrated ticks maintain performance budget",
            EXPECT(context.completed);
            
            uint64_t avg_tick_time = total_time / tick_count;
            
            printf("       Integrated BitActor/Fiber ticks:\n");
            printf("         Ticks executed: %d\n", tick_count);
            printf("         Average tick time: %llu ticks\n",
                   (unsigned long long)avg_tick_time);
            printf("         Total time: %llu ticks\n",
                   (unsigned long long)total_time);
            
            EXPECT_LE(avg_tick_time, 8);
            
            // Verify telemetry captured fiber execution
            EXPECT_GT(engine->telemetry.frame_count, 0);
            
            bool found_fiber_execution = false;
            for (uint32_t i = 0; i < engine->telemetry.frame_count; i++) {
                telemetry_frame_t* frame = &engine->telemetry.frames[i];
                if (frame->operation == TRACE_OP_FIBER) {
                    found_fiber_execution = true;
                    break;
                }
            }
            
            EXPECT(found_fiber_execution);
        );
    } END_SCENARIO
    
    SCENARIO("Fiber scheduler performance under load") {
        fiber_scheduler_t* scheduler = fiber_scheduler_init();
        const int FIBER_COUNT = 20;
        
        GIVEN("scheduler with maximum fiber load",
            test_fiber_context_t contexts[FIBER_COUNT];
            
            for (int i = 0; i < FIBER_COUNT; i++) {
                contexts[i] = (test_fiber_context_t){
                    .fiber_id = i,
                    .execution_count = 0,
                    .yield_count = 0,
                    .total_ticks = 0,
                    .completed = false
                };
                
                fiber_t* fiber = fiber_create(scheduler, i % 10); // Varying priorities
                
                if (fiber != NULL) {
                    fiber_set_function(fiber, test_fiber_simple, &contexts[i]);
                }
            }
            
            printf("       Created %u fibers\n", scheduler->active_fiber_count);
        );
        
        WHEN("scheduler handles high fiber load",
            uint64_t start = rdtsc_portable();
            
            // Run until half the fibers complete
            int completed_count = 0;
            int max_iterations = 1000;
            int iterations = 0;
            
            while (completed_count < FIBER_COUNT / 2 && iterations < max_iterations) {
                fiber_tick(scheduler);
                iterations++;
                
                // Count completed fibers
                completed_count = 0;
                for (int i = 0; i < FIBER_COUNT; i++) {
                    if (contexts[i].completed) completed_count++;
                }
            }
            
            uint64_t end = rdtsc_portable();
            uint64_t load_test_time = end - start;
        );
        
        THEN("scheduler maintains performance under load",
            uint64_t avg_tick_time = load_test_time / iterations;
            
            printf("       Load test results:\n");
            printf("         Fibers completed: %d/%d\n", completed_count, FIBER_COUNT);
            printf("         Iterations: %d\n", iterations);
            printf("         Average tick time: %llu ticks\n",
                   (unsigned long long)avg_tick_time);
            printf("         Total time: %llu ticks\n",
                   (unsigned long long)load_test_time);
            
            EXPECT_GT(completed_count, 0);
            EXPECT_LE(avg_tick_time, 8);
            
            fiber_scheduler_free(scheduler);
        );
    } END_SCENARIO
}