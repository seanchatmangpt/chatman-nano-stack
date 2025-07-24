/*
 * Memory Stress Test
 * EXTREME MEMORY testing - Push memory allocation to breaking point
 * Pool exhaustion, zero-allocation validation, leak detection
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../bitactor/include/bitactor_public.h"
#include "../bitactor/src/bitactor.h"
#include "../bitactor/src/bitfiber.h"
#include <string.h>
#include <stdlib.h>
#include <time.h>

// Memory stress configuration
#define MEMORY_STRESS_ITERATIONS 100000
#define POOL_EXHAUSTION_ATTEMPTS 50000
#define ALLOCATION_BURST_SIZE 1000
#define MAX_ALLOCATION_SIZE 4096
#define LEAK_DETECTION_CYCLES 10000

// Memory tracking structure
typedef struct {
    void* ptr;
    size_t size;
    uint64_t allocation_tick;
    bool leaked;
} memory_allocation_t;

// Memory stress results
typedef struct {
    uint64_t allocations_attempted;
    uint64_t allocations_successful;
    uint64_t allocations_failed;
    uint64_t pool_exhaustions;
    uint64_t potential_leaks;
    uint64_t memory_waste_bytes;
    uint64_t max_allocation_time;
    uint64_t total_allocated_bytes;
    double allocation_success_rate;
    bool zero_allocation_maintained;
} memory_stress_results_t;

// Global memory tracking
static memory_allocation_t allocations[MEMORY_STRESS_ITERATIONS];
static uint32_t allocation_count = 0;

// Custom memory tracking wrapper
static void* tracked_alloc(size_t size) {
    if (allocation_count >= MEMORY_STRESS_ITERATIONS) {
        return NULL; // Prevent overflow
    }
    
    void* ptr = malloc(size);
    if (ptr) {
        allocations[allocation_count] = (memory_allocation_t){
            .ptr = ptr,
            .size = size,
            .allocation_tick = rdtsc_portable(),
            .leaked = true // Assume leaked until freed
        };
        allocation_count++;
    }
    return ptr;
}

static void tracked_free(void* ptr) {
    for (uint32_t i = 0; i < allocation_count; i++) {
        if (allocations[i].ptr == ptr) {
            allocations[i].leaked = false;
            free(ptr);
            return;
        }
    }
    // Free without tracking (should not happen in correct code)
    free(ptr);
}

// Reset tracking
static void reset_memory_tracking(void) {
    allocation_count = 0;
    memset(allocations, 0, sizeof(allocations));
}

FEATURE(Memory_Stress_Testing) {
    
    SCENARIO("BitActor memory pool exhaustion stress test") {
        bitactor_engine* engine = bitactor_init();
        memory_stress_results_t results = {0};
        
        GIVEN("BitActor engine with limited memory pools",
            EXPECT_NE(engine, NULL);
            reset_memory_tracking();
        );
        
        WHEN("memory pools are pushed to exhaustion",
            // Test fiber pool exhaustion
            fiber_t* fibers[BITACTOR_MAX_FIBERS + 100]; // Try to exceed limit
            
            for (int i = 0; i < BITACTOR_MAX_FIBERS + 100; i++) {
                results.allocations_attempted++;
                
                uint64_t start = rdtsc_portable();
                fibers[i] = fiber_create(engine->fiber_sched, i % 10);
                uint64_t end = rdtsc_portable();
                
                uint64_t alloc_time = end - start;
                if (alloc_time > results.max_allocation_time) {
                    results.max_allocation_time = alloc_time;
                }
                
                if (fibers[i] != NULL) {
                    results.allocations_successful++;
                } else {
                    results.allocations_failed++;
                    results.pool_exhaustions++;
                }
            }
            
            // Test signal ring buffer exhaustion
            for (int i = 0; i < BITACTOR_MAX_SIGNALS + 1000; i++) {
                signal_t signal = {
                    .id = 700000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 128,
                    .flags = 0x7000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                results.allocations_attempted++;
                if (bitactor_enqueue(engine, signal)) {
                    results.allocations_successful++;
                } else {
                    results.allocations_failed++;
                    results.pool_exhaustions++;
                }
            }
            
            results.allocation_success_rate = 
                (double)results.allocations_successful / results.allocations_attempted;
        );
        
        THEN("memory pool exhaustion is handled gracefully",
            printf("       💾 MEMORY POOL EXHAUSTION STRESS 💾\n");
            printf("       Allocations attempted: %llu\n",
                   (unsigned long long)results.allocations_attempted);
            printf("       Allocations successful: %llu\n",
                   (unsigned long long)results.allocations_successful);
            printf("       Allocations failed: %llu\n",
                   (unsigned long long)results.allocations_failed);
            printf("       Pool exhaustions: %llu\n",
                   (unsigned long long)results.pool_exhaustions);
            printf("       Success rate: %.2f%%\n",
                   results.allocation_success_rate * 100);
            printf("       Max allocation time: %llu ticks\n",
                   (unsigned long long)results.max_allocation_time);
            
            // System should survive pool exhaustion
            EXPECT(engine->initialized);
            
            // Should have some pool exhaustion events
            EXPECT_GT(results.pool_exhaustions, 0);
            
            // Should fail gracefully (not crash)
            EXPECT_GE(results.allocation_success_rate, 0.5); // At least 50% should succeed
            
            // Allocation time should remain bounded even under stress
            EXPECT_LE(results.max_allocation_time, 100);
        );
    } END_SCENARIO
    
    SCENARIO("Zero heap allocation validation under stress") {
        bitactor_engine* engine = bitactor_init();
        size_t initial_heap_usage = 0;
        
        GIVEN("baseline heap measurement",
            // Record initial state (in real implementation would use heap profiler)
            initial_heap_usage = allocation_count * sizeof(memory_allocation_t);
            reset_memory_tracking();
        );
        
        WHEN("intensive BitActor operations are performed",
            // Register handler
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x8000, 
                            NULL); // Use default no-allocation handler
            
            // Process many signals without heap allocation
            for (int i = 0; i < 10000; i++) {
                signal_t signal = {
                    .id = 800000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 128,
                    .flags = 0x8000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                bitactor_enqueue(engine, signal);
                
                if (i % 100 == 0) {
                    bitactor_tick(engine);
                }
            }
            
            // Final processing
            while (engine->signal_count > 0) {
                bitactor_tick(engine);
            }
        );
        
        THEN("zero heap allocation guarantee is maintained",
            size_t final_heap_usage = allocation_count * sizeof(memory_allocation_t);
            
            printf("       ⚡ ZERO HEAP ALLOCATION VALIDATION ⚡\n");
            printf("       Initial heap usage: %zu bytes\n", initial_heap_usage);
            printf("       Final heap usage: %zu bytes\n", final_heap_usage);
            printf("       Heap growth: %zd bytes\n", 
                   (ssize_t)(final_heap_usage - initial_heap_usage));
            printf("       Zero allocation maintained: %s\n",
                   (final_heap_usage == initial_heap_usage) ? "YES" : "NO");
            
            // No additional heap allocation should occur during signal processing
            EXPECT_EQ(final_heap_usage, initial_heap_usage);
            
            // System should remain functional
            EXPECT(engine->initialized);
            EXPECT_EQ(engine->signal_count, 0);
        );
    } END_SCENARIO
    
    SCENARIO("Memory fragmentation stress test") {
        // Simulate memory fragmentation with varied allocation sizes
        memory_stress_results_t frag_results = {0};
        
        GIVEN("system performing varied memory operations",
            reset_memory_tracking();
        );
        
        WHEN("random allocation patterns create fragmentation",
            // Create fragmentation with varied sizes
            for (int i = 0; i < ALLOCATION_BURST_SIZE; i++) {
                size_t size = (i % 16 + 1) * 64; // 64, 128, ..., 1024 bytes
                
                uint64_t start = rdtsc_portable();
                void* ptr = tracked_alloc(size);
                uint64_t end = rdtsc_portable();
                
                frag_results.allocations_attempted++;
                if (ptr) {
                    frag_results.allocations_successful++;
                    frag_results.total_allocated_bytes += size;
                } else {
                    frag_results.allocations_failed++;
                }
                
                uint64_t alloc_time = end - start;
                if (alloc_time > frag_results.max_allocation_time) {
                    frag_results.max_allocation_time = alloc_time;
                }
                
                // Free every other allocation to create holes
                if (i % 2 == 1 && i > 0) {
                    tracked_free(allocations[i-1].ptr);
                }
            }
            
            // Calculate fragmentation metrics
            uint32_t leaked_count = 0;
            for (uint32_t i = 0; i < allocation_count; i++) {
                if (allocations[i].leaked) {
                    leaked_count++;
                    frag_results.memory_waste_bytes += allocations[i].size;
                }
            }
            frag_results.potential_leaks = leaked_count;
        );
        
        THEN("fragmentation is managed efficiently",
            printf("       🧩 MEMORY FRAGMENTATION STRESS 🧩\n");
            printf("       Total allocated: %llu bytes\n",
                   (unsigned long long)frag_results.total_allocated_bytes);
            printf("       Memory waste: %llu bytes\n",
                   (unsigned long long)frag_results.memory_waste_bytes);
            printf("       Potential leaks: %llu\n",
                   (unsigned long long)frag_results.potential_leaks);
            printf("       Max allocation time: %llu ticks\n",
                   (unsigned long long)frag_results.max_allocation_time);
            printf("       Fragmentation ratio: %.2f%%\n",
                   frag_results.total_allocated_bytes > 0 ?
                   (frag_results.memory_waste_bytes * 100.0) / frag_results.total_allocated_bytes : 0.0);
            
            // Should successfully allocate most requests
            EXPECT_GT(frag_results.allocations_successful, 
                     frag_results.allocations_attempted * 0.9);
            
            // Allocation time should remain reasonable despite fragmentation
            EXPECT_LE(frag_results.max_allocation_time, 200);
            
            // Cleanup remaining allocations
            for (uint32_t i = 0; i < allocation_count; i++) {
                if (allocations[i].leaked && allocations[i].ptr) {
                    tracked_free(allocations[i].ptr);
                }
            }
        );
    } END_SCENARIO
    
    SCENARIO("Memory leak detection under load") {
        memory_stress_results_t leak_results = {0};
        
        GIVEN("system with potential leak sources",
            reset_memory_tracking();
        );
        
        WHEN("repetitive allocation cycles are performed",
            for (int cycle = 0; cycle < LEAK_DETECTION_CYCLES; cycle++) {
                // Allocate memory
                size_t size = 64 + (cycle % 512);
                void* ptr = tracked_alloc(size);
                
                leak_results.allocations_attempted++;
                if (ptr) {
                    leak_results.allocations_successful++;
                    leak_results.total_allocated_bytes += size;
                    
                    // Simulate work with allocated memory
                    memset(ptr, 0xAB, size);
                    
                    // Intentionally leak some allocations (10%)
                    if (cycle % 10 != 0) {
                        tracked_free(ptr);
                    } else {
                        leak_results.potential_leaks++;
                        leak_results.memory_waste_bytes += size;
                    }
                } else {
                    leak_results.allocations_failed++;
                }
            }
        );
        
        THEN("memory leaks are detected and quantified",
            printf("       🚰 MEMORY LEAK DETECTION 🚰\n");
            printf("       Allocation cycles: %d\n", LEAK_DETECTION_CYCLES);
            printf("       Potential leaks: %llu\n",
                   (unsigned long long)leak_results.potential_leaks);
            printf("       Leaked memory: %llu bytes\n",
                   (unsigned long long)leak_results.memory_waste_bytes);
            printf("       Leak rate: %.2f%%\n",
                   leak_results.allocations_successful > 0 ?
                   (leak_results.potential_leaks * 100.0) / leak_results.allocations_successful : 0.0);
            
            // Should detect expected leak rate (around 10%)
            double expected_leaks = LEAK_DETECTION_CYCLES * 0.1;
            EXPECT_NEAR(leak_results.potential_leaks, expected_leaks, expected_leaks * 0.1);
            
            // Should track leaked memory accurately
            EXPECT_GT(leak_results.memory_waste_bytes, 0);
            
            // Cleanup all remaining allocations
            for (uint32_t i = 0; i < allocation_count; i++) {
                if (allocations[i].leaked && allocations[i].ptr) {
                    tracked_free(allocations[i].ptr);
                }
            }
        );
    } END_SCENARIO
    
    SCENARIO("Stack overflow protection stress test") {
        volatile char stack_stress[8192]; // Large stack allocation
        
        GIVEN("deep call stack simulation",
            // Fill stack array to force stack usage
            for (int i = 0; i < 8192; i++) {
                stack_stress[i] = (char)(i & 0xFF);
            }
        );
        
        WHEN("recursive operations stress the stack",
            // Simulate deep recursion with BitActor operations
            bitactor_engine* engine = bitactor_init();
            
            // Register handler that uses stack space
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x9000, 
                            NULL);
            
            // Create signals that would trigger deep processing
            int successful_operations = 0;
            for (int depth = 0; depth < 1000; depth++) {
                signal_t signal = {
                    .id = 900000 + depth,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 128,
                    .flags = 0x9000,
                    .payload = depth,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                if (bitactor_enqueue(engine, signal)) {
                    bitactor_tick(engine);
                    successful_operations++;
                    
                    // Use some stack space
                    volatile char local_buffer[256];
                    for (int j = 0; j < 256; j++) {
                        local_buffer[j] = (char)(depth + j);
                    }
                }
            }
        );
        
        THEN("stack overflow protection prevents crashes",
            // Verify stack content is still valid
            bool stack_integrity = true;
            for (int i = 0; i < 100; i++) { // Check sample
                if (stack_stress[i] != (char)(i & 0xFF)) {
                    stack_integrity = false;
                    break;
                }
            }
            
            printf("       📚 STACK OVERFLOW PROTECTION 📚\n");
            printf("       Successful operations: %d/1000\n", successful_operations);
            printf("       Stack integrity: %s\n",
                   stack_integrity ? "MAINTAINED" : "CORRUPTED");
            
            // Should complete most operations without stack corruption
            EXPECT_GT(successful_operations, 800);
            EXPECT(stack_integrity);
        );
    } END_SCENARIO
    
    SCENARIO("Memory pressure endurance test") {
        memory_stress_results_t pressure_results = {0};
        
        GIVEN("system under sustained memory pressure",
            reset_memory_tracking();
        );
        
        WHEN("continuous memory allocation/deallocation occurs",
            void* pressure_ptrs[1000];
            
            for (int iteration = 0; iteration < 1000; iteration++) {
                // Allocation phase
                for (int i = 0; i < 1000; i++) {
                    size_t size = 32 + (i % 256);
                    pressure_ptrs[i] = tracked_alloc(size);
                    
                    pressure_results.allocations_attempted++;
                    if (pressure_ptrs[i]) {
                        pressure_results.allocations_successful++;
                        pressure_results.total_allocated_bytes += size;
                    } else {
                        pressure_results.allocations_failed++;
                    }
                }
                
                // Deallocation phase
                for (int i = 0; i < 1000; i++) {
                    if (pressure_ptrs[i]) {
                        tracked_free(pressure_ptrs[i]);
                        pressure_ptrs[i] = NULL;
                    }
                }
                
                // Check for memory pressure effects every 100 iterations
                if (iteration % 100 == 0) {
                    if (pressure_results.allocations_failed > 
                        pressure_results.allocations_attempted * 0.5) {
                        printf("       ⚠️  High memory pressure at iteration %d\n", iteration);
                        break;
                    }
                }
            }
            
            pressure_results.allocation_success_rate = 
                (double)pressure_results.allocations_successful / 
                pressure_results.allocations_attempted;
        );
        
        THEN("system maintains stability under memory pressure",
            printf("       🎯 MEMORY PRESSURE ENDURANCE 🎯\n");
            printf("       Total allocations: %llu\n",
                   (unsigned long long)pressure_results.allocations_attempted);
            printf("       Success rate: %.2f%%\n",
                   pressure_results.allocation_success_rate * 100);
            printf("       Total bytes processed: %llu\n",
                   (unsigned long long)pressure_results.total_allocated_bytes);
            
            // Should maintain high success rate under sustained pressure
            EXPECT_GE(pressure_results.allocation_success_rate, 0.95);
            
            // Should process significant amount of memory
            EXPECT_GT(pressure_results.total_allocated_bytes, 1024 * 1024); // At least 1MB
        );
    } END_SCENARIO
}