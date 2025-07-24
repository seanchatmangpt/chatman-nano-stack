/*
 * BitActor Load Stress Test
 * EXTREME LOAD testing - Push BitActor to absolute breaking point
 * 100K+ signals, queue overflow, batch processing limits
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../bitactor/include/bitactor_public.h"
#include "../bitactor/src/bitactor.h"
#include <string.h>
#include <stdlib.h>
#include <time.h>

// Stress test configuration
#define EXTREME_SIGNAL_COUNT 100000
#define QUEUE_OVERFLOW_TEST 50000
#define BATCH_STRESS_SIZE 10000
#define CONCURRENT_SENDERS 100
#define STRESS_ITERATIONS 1000000

// Stress test results
typedef struct {
    uint64_t signals_processed;
    uint64_t signals_dropped;
    uint64_t queue_overflows;
    uint64_t tick_violations;
    uint64_t max_latency_ticks;
    uint64_t total_stress_time;
    double throughput_signals_per_tick;
    bool system_survived;
} load_stress_results_t;

// Stress handler that processes signals as fast as possible
static result_t stress_speed_handler(signal_t signal, void* ctx, void* scratch) {
    // Ultra-minimal processing - just count and return
    static volatile uint64_t counter = 0;
    counter++;
    
    result_t result = {
        .signal_id = signal.id,
        .exec_hash = 0xDEADBEEF,
        .status = RESULT_STATUS_SUCCESS,
        .ticks = 1, // Claim 1 tick processing
        .result = signal.payload,
        .fiber_id = 0
    };
    
    return result;
}

// Handler that intentionally takes longer to stress timing
static result_t stress_slow_handler(signal_t signal, void* ctx, void* scratch) {
    // Simulate expensive computation
    volatile uint64_t work = 0;
    for (int i = 0; i < 1000; i++) {
        work += i * signal.payload;
    }
    
    result_t result = {
        .signal_id = signal.id,
        .exec_hash = 0xFEEDBEEF,
        .status = RESULT_STATUS_SUCCESS,
        .ticks = 15, // Claim more than 8 ticks to stress system
        .result = work,
        .fiber_id = 0
    };
    
    return result;
}

FEATURE(BitActor_Load_Stress_Testing) {
    
    SCENARIO("Extreme signal load stress test - 100K signals") {
        bitactor_engine* engine = bitactor_init();
        load_stress_results_t results = {0};
        
        GIVEN("BitActor engine configured for extreme load",
            EXPECT_NE(engine, NULL);
            
            // Register fast stress handler
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x1000, 
                            stress_speed_handler);
        );
        
        WHEN("100,000 signals are pumped through the system",
            uint64_t start_time = rdtsc_portable();
            
            // Generate massive signal load
            for (int i = 0; i < EXTREME_SIGNAL_COUNT; i++) {
                signal_t stress_signal = {
                    .id = 100000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = i % 256,
                    .flags = 0x1000,
                    .payload = (uint64_t)i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                bool enqueued = bitactor_enqueue(engine, stress_signal);
                if (enqueued) {
                    results.signals_processed++;
                } else {
                    results.signals_dropped++;
                    results.queue_overflows++;
                }
                
                // Process in batches to prevent total queue overflow
                if (i % 1000 == 0) {
                    uint64_t batch_start = rdtsc_portable();
                    uint32_t processed = bitactor_drain(engine, 1000);
                    uint64_t batch_end = rdtsc_portable();
                    
                    if (batch_end - batch_start > 8000) { // 8 ticks * 1000 signals
                        results.tick_violations++;
                    }
                    
                    uint64_t batch_latency = batch_end - batch_start;
                    if (batch_latency > results.max_latency_ticks) {
                        results.max_latency_ticks = batch_latency;
                    }
                }
            }
            
            // Final drain
            while (engine->signal_count > 0) {
                bitactor_drain(engine, 1000);
            }
            
            uint64_t end_time = rdtsc_portable();
            results.total_stress_time = end_time - start_time;
            results.throughput_signals_per_tick = 
                (double)results.signals_processed / results.total_stress_time;
            results.system_survived = (engine->initialized && 
                                     results.signals_processed > EXTREME_SIGNAL_COUNT * 0.9);
        );
        
        THEN("system handles extreme load with graceful degradation",
            printf("       ⚡ EXTREME LOAD STRESS RESULTS ⚡\n");
            printf("       Signals processed: %llu/%d (%.1f%%)\n",
                   (unsigned long long)results.signals_processed,
                   EXTREME_SIGNAL_COUNT,
                   (results.signals_processed * 100.0) / EXTREME_SIGNAL_COUNT);
            printf("       Signals dropped: %llu\n", 
                   (unsigned long long)results.signals_dropped);
            printf("       Queue overflows: %llu\n",
                   (unsigned long long)results.queue_overflows);
            printf("       Tick violations: %llu\n",
                   (unsigned long long)results.tick_violations);
            printf("       Max latency: %llu ticks\n",
                   (unsigned long long)results.max_latency_ticks);
            printf("       Throughput: %.2f signals/tick\n",
                   results.throughput_signals_per_tick);
            printf("       System survived: %s\n",
                   results.system_survived ? "YES" : "NO");
            
            // System should survive extreme load
            EXPECT(results.system_survived);
            
            // Should process at least 90% of signals
            EXPECT_GE(results.signals_processed, EXTREME_SIGNAL_COUNT * 0.9);
            
            // Should maintain reasonable throughput even under stress
            EXPECT_GT(results.throughput_signals_per_tick, 0.1);
        );
    } END_SCENARIO
    
    SCENARIO("Queue overflow stress test - Push ring buffer limits") {
        bitactor_engine* engine = bitactor_init();
        uint32_t overflow_count = 0;
        
        GIVEN("engine with ring buffer near capacity",
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x2000, 
                            stress_speed_handler);
        );
        
        WHEN("signals are enqueued faster than processing",
            // Fill queue to capacity without processing
            for (int i = 0; i < QUEUE_OVERFLOW_TEST; i++) {
                signal_t signal = {
                    .id = 200000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 5,
                    .flags = 0x2000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                if (!bitactor_enqueue(engine, signal)) {
                    overflow_count++;
                }
            }
        );
        
        THEN("queue overflow is handled gracefully",
            printf("       Queue Overflow Stress:\n");
            printf("         Overflow events: %u\n", overflow_count);
            printf("         Queue utilization: %u/%u\n", 
                   engine->signal_count, BITACTOR_MAX_SIGNALS);
            printf("         System state: %s\n",
                   engine->initialized ? "STABLE" : "CRASHED");
            
            // System should remain stable even with overflows
            EXPECT(engine->initialized);
            
            // Should have some overflow events (testing overflow handling)
            EXPECT_GT(overflow_count, 0);
            
            // Queue should be at capacity
            EXPECT_EQ(engine->signal_count, BITACTOR_MAX_SIGNALS);
            
            // System should recover after draining
            bitactor_drain(engine, BITACTOR_MAX_SIGNALS);
            EXPECT_LT(engine->signal_count, BITACTOR_MAX_SIGNALS);
        );
    } END_SCENARIO
    
    SCENARIO("Batch processing stress test - Maximum batch sizes") {
        bitactor_engine* engine = bitactor_init();
        
        GIVEN("engine loaded with maximum batch size",
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x3000, 
                            stress_speed_handler);
            
            // Load maximum signals
            for (int i = 0; i < BATCH_STRESS_SIZE; i++) {
                signal_t signal = {
                    .id = 300000 + i,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = i % 256,
                    .flags = 0x3000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                if (!bitactor_enqueue(engine, signal)) {
                    break; // Queue full
                }
            }
        );
        
        WHEN("maximum batch drain is performed",
            uint64_t start = rdtsc_portable();
            uint32_t processed = bitactor_drain(engine, BATCH_STRESS_SIZE);
            uint64_t end = rdtsc_portable();
            uint64_t batch_time = end - start;
        );
        
        THEN("batch processing scales efficiently",
            uint64_t avg_per_signal = processed > 0 ? batch_time / processed : 0;
            
            printf("       Batch Processing Stress:\n");
            printf("         Signals processed: %u\n", processed);
            printf("         Total batch time: %llu ticks\n",
                   (unsigned long long)batch_time);
            printf("         Avg per signal: %llu ticks\n",
                   (unsigned long long)avg_per_signal);
            printf("         Batch efficiency: %.2f signals/tick\n",
                   processed > 0 ? (double)processed / batch_time : 0.0);
            
            // Should process significant batch
            EXPECT_GT(processed, BATCH_STRESS_SIZE / 2);
            
            // Average per signal should be well under 8 ticks
            EXPECT_LE(avg_per_signal, 4);
            
            // Batch efficiency should be high
            EXPECT_GT((double)processed / batch_time, 1.0);
        );
    } END_SCENARIO
    
    SCENARIO("Concurrent signal injection stress test") {
        bitactor_engine* engine = bitactor_init();
        
        GIVEN("engine with concurrent signal sources",
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x4000, 
                            stress_speed_handler);
        );
        
        WHEN("multiple concurrent senders inject signals",
            uint64_t total_injected = 0;
            uint64_t start = rdtsc_portable();
            
            // Simulate concurrent injection
            for (int sender = 0; sender < CONCURRENT_SENDERS; sender++) {
                for (int burst = 0; burst < 100; burst++) {
                    signal_t signal = {
                        .id = 400000 + (sender * 1000) + burst,
                        .kind = SIGNAL_KIND_DATA,
                        .priority = sender % 16,
                        .flags = 0x4000,
                        .payload = (sender << 16) | burst,
                        .timestamp = rdtsc_portable(),
                        .context = sender
                    };
                    
                    if (bitactor_enqueue(engine, signal)) {
                        total_injected++;
                    }
                    
                    // Periodic processing to simulate real-time
                    if (burst % 10 == 0) {
                        bitactor_tick(engine);
                    }
                }
            }
            
            // Final processing
            while (engine->signal_count > 0) {
                bitactor_tick(engine);
            }
            
            uint64_t end = rdtsc_portable();
            uint64_t concurrent_time = end - start;
        );
        
        THEN("concurrent injection is handled efficiently",
            printf("       Concurrent Injection Stress:\n");
            printf("         Concurrent senders: %d\n", CONCURRENT_SENDERS);
            printf("         Total injected: %llu\n", 
                   (unsigned long long)total_injected);
            printf("         Processing time: %llu ticks\n",
                   (unsigned long long)concurrent_time);
            printf("         Concurrency efficiency: %.2f signals/tick\n",
                   (double)total_injected / concurrent_time);
            
            // Should handle most injected signals
            EXPECT_GT(total_injected, CONCURRENT_SENDERS * 80); // 80% success rate
            
            // Should maintain good throughput under concurrency
            EXPECT_GT((double)total_injected / concurrent_time, 0.5);
            
            // System should remain stable
            EXPECT(engine->initialized);
        );
    } END_SCENARIO
    
    SCENARIO("Priority inversion stress test under load") {
        bitactor_engine* engine = bitactor_init();
        
        GIVEN("mixed priority signals under extreme load",
            // Register handlers with different processing times
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x5000, 
                            stress_speed_handler);
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_CONTROL, 
                            0x5001, 
                            stress_slow_handler);
        );
        
        WHEN("high and low priority signals compete under load",
            uint32_t high_priority_processed = 0;
            uint32_t low_priority_processed = 0;
            
            // Inject mix of high and low priority signals
            for (int i = 0; i < 5000; i++) {
                // High priority signal (should be fast)
                signal_t high_pri = {
                    .id = 500000 + i * 2,
                    .kind = SIGNAL_KIND_DATA,
                    .priority = 255, // Maximum priority
                    .flags = 0x5000,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                // Low priority signal (intentionally slow)
                signal_t low_pri = {
                    .id = 500000 + i * 2 + 1,
                    .kind = SIGNAL_KIND_CONTROL,
                    .priority = 1, // Minimum priority
                    .flags = 0x5001,
                    .payload = i,
                    .timestamp = rdtsc_portable(),
                    .context = 0
                };
                
                bitactor_enqueue(engine, low_pri);  // Enqueue slow one first
                bitactor_enqueue(engine, high_pri); // Then fast one
                
                if (i % 100 == 0) {
                    bitactor_tick(engine);
                }
            }
            
            // Process remaining signals
            while (engine->signal_count > 0) {
                bitactor_tick(engine);
            }
            
            // Count processed signals by type
            for (uint32_t i = 0; i < engine->telemetry.frame_count; i++) {
                telemetry_frame_t* frame = &engine->telemetry.frames[i];
                if (frame->signal.kind == SIGNAL_KIND_DATA) {
                    high_priority_processed++;
                } else if (frame->signal.kind == SIGNAL_KIND_CONTROL) {
                    low_priority_processed++;
                }
            }
        );
        
        THEN("priority scheduling maintains fairness under stress",
            printf("       Priority Inversion Stress:\n");
            printf("         High priority processed: %u\n", high_priority_processed);
            printf("         Low priority processed: %u\n", low_priority_processed);
            printf("         Priority ratio: %.2f:1\n",
                   low_priority_processed > 0 ? 
                   (double)high_priority_processed / low_priority_processed : 0.0);
            
            // Both types should be processed
            EXPECT_GT(high_priority_processed, 0);
            EXPECT_GT(low_priority_processed, 0);
            
            // High priority should not completely starve low priority
            EXPECT_LT((double)high_priority_processed / low_priority_processed, 10.0);
            
            // But high priority should get preference
            EXPECT_GT(high_priority_processed, low_priority_processed);
        );
    } END_SCENARIO
    
    SCENARIO("Sustained load endurance test") {
        bitactor_engine* engine = bitactor_init();
        
        GIVEN("engine configured for sustained operation",
            dispatch_register(&engine->dispatch, 
                            SIGNAL_KIND_DATA, 
                            0x6000, 
                            stress_speed_handler);
        );
        
        WHEN("sustained load is applied over extended period",
            uint64_t total_signals = 0;
            uint64_t start_time = rdtsc_portable();
            
            // Run for many iterations
            for (int iteration = 0; iteration < STRESS_ITERATIONS / 1000; iteration++) {
                // Burst of signals
                for (int burst = 0; burst < 100; burst++) {
                    signal_t signal = {
                        .id = 600000 + iteration * 100 + burst,
                        .kind = SIGNAL_KIND_DATA,
                        .priority = 128,
                        .flags = 0x6000,
                        .payload = iteration * 100 + burst,
                        .timestamp = rdtsc_portable(),
                        .context = 0
                    };
                    
                    if (bitactor_enqueue(engine, signal)) {
                        total_signals++;
                    }
                }
                
                // Process burst
                bitactor_drain(engine, 100);
                
                // Check system health periodically
                if (iteration % 100 == 0) {
                    if (!engine->initialized) {
                        printf("       ⚠️  System failure at iteration %d\n", iteration);
                        break;
                    }
                }
            }
            
            uint64_t end_time = rdtsc_portable();
            uint64_t sustained_time = end_time - start_time;
        );
        
        THEN("system maintains stability under sustained load",
            printf("       Sustained Load Endurance:\n");
            printf("         Total signals: %llu\n", 
                   (unsigned long long)total_signals);
            printf("         Sustained time: %llu ticks\n",
                   (unsigned long long)sustained_time);
            printf("         Average throughput: %.2f signals/tick\n",
                   (double)total_signals / sustained_time);
            printf("         System integrity: %s\n",
                   engine->initialized ? "MAINTAINED" : "COMPROMISED");
            
            // System should survive sustained load
            EXPECT(engine->initialized);
            
            // Should process significant number of signals
            EXPECT_GT(total_signals, STRESS_ITERATIONS / 20);
            
            // Should maintain consistent throughput
            EXPECT_GT((double)total_signals / sustained_time, 0.01);
            
            // No signal queue should be left over
            EXPECT_EQ(engine->signal_count, 0);
        );
    } END_SCENARIO
}