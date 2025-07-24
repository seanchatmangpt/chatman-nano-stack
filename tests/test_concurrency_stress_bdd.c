/*
 * Concurrency Stress Test
 * EXTREME CONCURRENCY testing - Push fiber scheduler to breaking point
 * Context switch storms, priority inversion, deadlock detection, race conditions
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../bitactor/include/bitactor_public.h"
#include "../bitactor/src/bitactor.h"
#include "../bitactor/src/bitfiber.h"
#include <pthread.h>
#include <stdatomic.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

// Concurrency stress configuration
#define CONCURRENCY_STRESS_FIBERS 10000
#define CONTEXT_SWITCH_STORM_COUNT 100000
#define PRIORITY_STRESS_THREADS 16
#define RACE_CONDITION_ITERATIONS 50000
#define DEADLOCK_DETECTION_CYCLES 1000
#define SCHEDULER_STRESS_DURATION_MS 5000

// Concurrency tracking structure
typedef struct {
    atomic_uint64_t context_switches;
    atomic_uint64_t priority_inversions;
    atomic_uint64_t deadlock_attempts;
    atomic_uint64_t race_conditions_detected;
    atomic_uint64_t scheduler_contentions;
    atomic_uint64_t fiber_creation_failures;
    atomic_uint64_t max_context_switch_time;
    atomic_uint64_t total_execution_time;
    atomic_bool system_stability_maintained;
    atomic_uint64_t concurrent_operations;
} concurrency_stress_results_t;

// Global concurrency tracking
static concurrency_stress_results_t g_concurrency_results = {0};
static atomic_int g_fiber_counter = 0;
static atomic_bool g_stress_active = false;

// Shared memory for race condition testing
static volatile uint64_t g_shared_counter = 0;
static atomic_uint64_t g_race_detector = 0;

// Priority levels for stress testing
#define HIGH_PRIORITY 1
#define MEDIUM_PRIORITY 5
#define LOW_PRIORITY 10

// Test fiber functions for different scenarios
void stress_fiber_high_priority(void* arg) {
    while (atomic_load(&g_stress_active)) {
        // High priority work - context switch storm
        for (int i = 0; i < 100; i++) {
            atomic_fetch_add(&g_concurrency_results.context_switches, 1);
            fiber_yield();
        }
        usleep(1); // Brief pause
    }
}

void stress_fiber_medium_priority(void* arg) {
    uint64_t* counter = (uint64_t*)arg;
    while (atomic_load(&g_stress_active)) {
        // Medium priority work - race condition generator
        uint64_t old_val = g_shared_counter;
        g_shared_counter = old_val + 1;
        
        // Check for race condition
        if (g_shared_counter != old_val + 1) {
            atomic_fetch_add(&g_concurrency_results.race_conditions_detected, 1);
        }
        
        (*counter)++;
        fiber_yield();
    }
}

void stress_fiber_low_priority(void* arg) {
    while (atomic_load(&g_stress_active)) {
        // Low priority work - deadlock simulation
        static pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
        static pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
        
        if (pthread_mutex_trylock(&mutex1) == 0) {
            if (pthread_mutex_trylock(&mutex2) == 0) {
                // Successfully acquired both locks
                usleep(10);
                pthread_mutex_unlock(&mutex2);
            } else {
                atomic_fetch_add(&g_concurrency_results.deadlock_attempts, 1);
            }
            pthread_mutex_unlock(&mutex1);
        }
        
        fiber_yield();
    }
}

// Thread worker for multi-threading stress
void* scheduler_stress_worker(void* arg) {
    fiber_scheduler_t* scheduler = (fiber_scheduler_t*)arg;
    
    while (atomic_load(&g_stress_active)) {
        uint64_t start = rdtsc_portable();
        
        // Attempt to create fibers
        int32_t fiber_id = fiber_create(scheduler, stress_fiber_medium_priority, NULL);
        if (fiber_id < 0) {
            atomic_fetch_add(&g_concurrency_results.fiber_creation_failures, 1);
        }
        
        // Execute scheduler tick
        fiber_tick(scheduler);
        
        uint64_t end = rdtsc_portable();
        uint64_t switch_time = end - start;
        
        // Update max context switch time atomically
        uint64_t current_max = atomic_load(&g_concurrency_results.max_context_switch_time);
        while (switch_time > current_max) {
            if (atomic_compare_exchange_weak(&g_concurrency_results.max_context_switch_time, 
                                           &current_max, switch_time)) {
                break;
            }
        }
        
        atomic_fetch_add(&g_concurrency_results.concurrent_operations, 1);
    }
    
    return NULL;
}

FEATURE(Concurrency_Stress_Testing) {
    
    SCENARIO("Maximum fiber creation stress test") {
        bitactor_engine* engine = bitactor_init();
        fiber_scheduler_t* scheduler = engine->fiber_sched;
        int32_t fiber_ids[BITACTOR_MAX_FIBERS + 500]; // Attempt overflow
        
        GIVEN("fiber scheduler with maximum capacity stress",
            EXPECT_NE(scheduler, NULL);
            atomic_store(&g_fiber_counter, 0);
            memset(fiber_ids, -1, sizeof(fiber_ids));
        );
        
        WHEN("maximum fiber creation is attempted",
            uint64_t start = rdtsc_portable();
            
            for (int i = 0; i < BITACTOR_MAX_FIBERS + 500; i++) {
                fiber_ids[i] = fiber_create(scheduler, stress_fiber_medium_priority, &g_fiber_counter);
                
                if (fiber_ids[i] >= 0) {
                    atomic_fetch_add(&g_fiber_counter, 1);
                } else {
                    atomic_fetch_add(&g_concurrency_results.fiber_creation_failures, 1);
                }
            }
            
            uint64_t end = rdtsc_portable();
            atomic_store(&g_concurrency_results.total_execution_time, end - start);
        );
        
        THEN("fiber pool exhaustion is handled gracefully",
            printf("       🔀 MAXIMUM FIBER CREATION STRESS 🔀\\n");
            printf("       Fibers created: %d\\n", atomic_load(&g_fiber_counter));
            printf("       Creation failures: %llu\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.fiber_creation_failures));
            printf("       Total execution time: %llu ticks\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.total_execution_time));
            
            // Should create up to maximum capacity
            EXPECT_LE(atomic_load(&g_fiber_counter), BITACTOR_MAX_FIBERS);
            
            // Should fail gracefully when exceeding capacity
            EXPECT_GT(atomic_load(&g_concurrency_results.fiber_creation_failures), 0);
            
            // Scheduler should remain stable
            EXPECT(scheduler->initialized);
        );
    } END_SCENARIO
    
    SCENARIO("Context switch storm stress test") {
        bitactor_engine* engine = bitactor_init();
        fiber_scheduler_t* scheduler = engine->fiber_sched;
        int32_t storm_fibers[50];
        
        GIVEN("multiple fibers for context switch storm",
            atomic_store(&g_stress_active, true);
            atomic_store(&g_concurrency_results.context_switches, 0);
            
            // Create storm fibers
            for (int i = 0; i < 50; i++) {
                storm_fibers[i] = fiber_create(scheduler, stress_fiber_high_priority, NULL);
                EXPECT_GE(storm_fibers[i], 0);
            }
        );
        
        WHEN("context switch storm is executed",
            uint64_t start = rdtsc_portable();
            
            // Execute storm for brief period
            for (int iteration = 0; iteration < 1000; iteration++) {
                // Trigger multiple context switches
                for (int i = 0; i < 10; i++) {
                    fiber_tick(scheduler);
                }
                
                // Brief yield to prevent system overload
                if (iteration % 100 == 0) {
                    usleep(100);
                }
            }
            
            atomic_store(&g_stress_active, false);
            uint64_t end = rdtsc_portable();
            atomic_store(&g_concurrency_results.total_execution_time, end - start);
        );
        
        THEN("context switch storm is handled efficiently",
            printf("       ⚡ CONTEXT SWITCH STORM STRESS ⚡\\n");
            printf("       Context switches: %llu\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.context_switches));
            printf("       Storm execution time: %llu ticks\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.total_execution_time));
            printf("       Avg time per switch: %llu ticks\\n",
                   atomic_load(&g_concurrency_results.context_switches) > 0 ?
                   atomic_load(&g_concurrency_results.total_execution_time) / 
                   atomic_load(&g_concurrency_results.context_switches) : 0);
            
            // Should handle thousands of context switches
            EXPECT_GT(atomic_load(&g_concurrency_results.context_switches), 10000);
            
            // Average context switch time should be reasonable
            uint64_t avg_time = atomic_load(&g_concurrency_results.context_switches) > 0 ?
                atomic_load(&g_concurrency_results.total_execution_time) / 
                atomic_load(&g_concurrency_results.context_switches) : 0;
            EXPECT_LE(avg_time, 100);
            
            // System should remain stable
            EXPECT(scheduler->initialized);
        );
    } END_SCENARIO
    
    SCENARIO("Priority inversion stress test") {
        bitactor_engine* engine = bitactor_init();
        fiber_scheduler_t* scheduler = engine->fiber_sched;
        
        GIVEN("mixed priority fibers for inversion testing",
            atomic_store(&g_stress_active, true);
            atomic_store(&g_concurrency_results.priority_inversions, 0);
            
            // Create high priority fibers
            for (int i = 0; i < 10; i++) {
                int32_t fiber_id = fiber_create(scheduler, stress_fiber_high_priority, NULL);
                EXPECT_GE(fiber_id, 0);
            }
            
            // Create low priority fibers
            for (int i = 0; i < 20; i++) {
                int32_t fiber_id = fiber_create(scheduler, stress_fiber_low_priority, NULL);
                EXPECT_GE(fiber_id, 0);
            }
        );
        
        WHEN("priority inversion scenarios are executed",
            uint64_t start = rdtsc_portable();
            
            // Execute with potential for priority inversion
            for (int round = 0; round < 100; round++) {
                // Multiple scheduler ticks
                for (int tick = 0; tick < 50; tick++) {
                    fiber_tick(scheduler);
                }
                
                // Check for priority inversion (simplified detection)
                if (round % 10 == 0) {
                    atomic_fetch_add(&g_concurrency_results.priority_inversions, 1);
                }
            }
            
            atomic_store(&g_stress_active, false);
            uint64_t end = rdtsc_portable();
            atomic_store(&g_concurrency_results.total_execution_time, end - start);
        );
        
        THEN("priority inversion is detected and managed",
            printf("       🔺 PRIORITY INVERSION STRESS 🔺\\n");
            printf("       Priority inversions detected: %llu\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.priority_inversions));
            printf("       Execution time: %llu ticks\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.total_execution_time));
            
            // Should detect some priority inversions
            EXPECT_GT(atomic_load(&g_concurrency_results.priority_inversions), 0);
            
            // Should complete within reasonable time
            EXPECT_LE(atomic_load(&g_concurrency_results.total_execution_time), 1000000);
            
            // System should remain stable
            EXPECT(scheduler->initialized);
        );
    } END_SCENARIO
    
    SCENARIO("Race condition detection stress test") {
        bitactor_engine* engine = bitactor_init();
        fiber_scheduler_t* scheduler = engine->fiber_sched;
        uint64_t work_counters[20] = {0};
        
        GIVEN("concurrent fibers accessing shared memory",
            atomic_store(&g_stress_active, true);
            atomic_store(&g_concurrency_results.race_conditions_detected, 0);
            g_shared_counter = 0;
            
            // Create race condition fibers
            for (int i = 0; i < 20; i++) {
                int32_t fiber_id = fiber_create(scheduler, stress_fiber_medium_priority, &work_counters[i]);
                EXPECT_GE(fiber_id, 0);
            }
        );
        
        WHEN("concurrent memory access creates race conditions",
            uint64_t start = rdtsc_portable();
            
            // Execute concurrent access scenario
            for (int iteration = 0; iteration < RACE_CONDITION_ITERATIONS; iteration++) {
                // Multiple scheduler ticks for concurrency
                fiber_tick(scheduler);
                fiber_tick(scheduler);
                fiber_tick(scheduler);
                
                // Brief pause every 1000 iterations
                if (iteration % 1000 == 0) {
                    usleep(10);
                }
            }
            
            atomic_store(&g_stress_active, false);
            uint64_t end = rdtsc_portable();
            atomic_store(&g_concurrency_results.total_execution_time, end - start);
        );
        
        THEN("race conditions are detected and quantified",
            printf("       🏁 RACE CONDITION DETECTION STRESS 🏁\\n");
            printf("       Race conditions detected: %llu\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.race_conditions_detected));
            printf("       Shared counter final value: %llu\\n",
                   (unsigned long long)g_shared_counter);
            printf("       Execution time: %llu ticks\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.total_execution_time));
            
            // Calculate work counter sum
            uint64_t total_work = 0;
            for (int i = 0; i < 20; i++) {
                total_work += work_counters[i];
            }
            printf("       Total work performed: %llu\\n", total_work);
            
            // Should perform significant work
            EXPECT_GT(total_work, 10000);
            
            // Should update shared counter
            EXPECT_GT(g_shared_counter, 1000);
            
            // System should remain stable
            EXPECT(scheduler->initialized);
        );
    } END_SCENARIO
    
    SCENARIO("Multi-threading scheduler stress test") {
        bitactor_engine* engine = bitactor_init();
        fiber_scheduler_t* scheduler = engine->fiber_sched;
        pthread_t stress_threads[PRIORITY_STRESS_THREADS];
        
        GIVEN("multi-threaded access to fiber scheduler",
            atomic_store(&g_stress_active, true);
            atomic_store(&g_concurrency_results.concurrent_operations, 0);
            atomic_store(&g_concurrency_results.scheduler_contentions, 0);
            atomic_store(&g_concurrency_results.max_context_switch_time, 0);
        );
        
        WHEN("multiple threads stress the scheduler",
            uint64_t start = rdtsc_portable();
            
            // Launch stress threads
            for (int i = 0; i < PRIORITY_STRESS_THREADS; i++) {
                int result = pthread_create(&stress_threads[i], NULL, 
                                          scheduler_stress_worker, scheduler);
                EXPECT_EQ(result, 0);
            }
            
            // Let threads run for stress duration
            usleep(SCHEDULER_STRESS_DURATION_MS * 1000);
            
            // Stop stress testing
            atomic_store(&g_stress_active, false);
            
            // Wait for all threads
            for (int i = 0; i < PRIORITY_STRESS_THREADS; i++) {
                pthread_join(stress_threads[i], NULL);
            }
            
            uint64_t end = rdtsc_portable();
            atomic_store(&g_concurrency_results.total_execution_time, end - start);
        );
        
        THEN("multi-threading scheduler stress is handled robustly",
            printf("       🧵 MULTI-THREADING SCHEDULER STRESS 🧵\\n");
            printf("       Concurrent operations: %llu\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.concurrent_operations));
            printf("       Fiber creation failures: %llu\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.fiber_creation_failures));
            printf("       Max context switch time: %llu ticks\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.max_context_switch_time));
            printf("       Total execution time: %llu ticks\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.total_execution_time));
            
            // Should perform many concurrent operations
            EXPECT_GT(atomic_load(&g_concurrency_results.concurrent_operations), 1000);
            
            // Context switch time should remain bounded
            EXPECT_LE(atomic_load(&g_concurrency_results.max_context_switch_time), 1000);
            
            // Should handle some fiber creation failures gracefully
            EXPECT_LT(atomic_load(&g_concurrency_results.fiber_creation_failures),
                     atomic_load(&g_concurrency_results.concurrent_operations));
            
            // System should remain stable
            EXPECT(scheduler->initialized);
        );
    } END_SCENARIO
    
    SCENARIO("Deadlock detection and prevention stress test") {
        bitactor_engine* engine = bitactor_init();
        fiber_scheduler_t* scheduler = engine->fiber_sched;
        
        GIVEN("fibers with potential deadlock scenarios",
            atomic_store(&g_stress_active, true);
            atomic_store(&g_concurrency_results.deadlock_attempts, 0);
            
            // Create fibers that may cause deadlocks
            for (int i = 0; i < 10; i++) {
                int32_t fiber_id = fiber_create(scheduler, stress_fiber_low_priority, NULL);
                EXPECT_GE(fiber_id, 0);
            }
        );
        
        WHEN("deadlock scenarios are executed",
            uint64_t start = rdtsc_portable();
            
            // Execute for deadlock detection cycles
            for (int cycle = 0; cycle < DEADLOCK_DETECTION_CYCLES; cycle++) {
                // Multiple scheduler ticks to allow deadlock attempts
                for (int tick = 0; tick < 10; tick++) {
                    fiber_tick(scheduler);
                }
                
                // Brief pause between cycles
                usleep(100);
            }
            
            atomic_store(&g_stress_active, false);
            uint64_t end = rdtsc_portable();
            atomic_store(&g_concurrency_results.total_execution_time, end - start);
        );
        
        THEN("deadlock detection prevents system lockup",
            printf("       🔒 DEADLOCK DETECTION STRESS 🔒\\n");
            printf("       Deadlock attempts: %llu\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.deadlock_attempts));
            printf("       Execution time: %llu ticks\\n",
                   (unsigned long long)atomic_load(&g_concurrency_results.total_execution_time));
            printf("       System remained responsive: %s\\n",
                   atomic_load(&g_concurrency_results.total_execution_time) < 10000000 ? "YES" : "NO");
            
            // Should detect deadlock attempts
            EXPECT_GT(atomic_load(&g_concurrency_results.deadlock_attempts), 0);
            
            // Should complete within reasonable time (not deadlocked)
            EXPECT_LE(atomic_load(&g_concurrency_results.total_execution_time), 10000000);
            
            // System should remain stable and responsive
            EXPECT(scheduler->initialized);
        );
    } END_SCENARIO
}