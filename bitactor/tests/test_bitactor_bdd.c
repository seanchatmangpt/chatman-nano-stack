/*
 * BitActor BDD Specifications
 * Behavior-driven tests for deterministic signal processing
 */
#include "bdd_framework.h"
#include "../include/bitactor/bitactor.h"
#include <sys/resource.h>
#include <unistd.h>
#include <stdlib.h>

/* Test helpers */
static size_t get_current_memory_usage(void) {
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);
    return usage.ru_maxrss;
}

FEATURE(BitActor_Deterministic_Signal_Processing) {
    
    SCENARIO("Processing signals within 8 CPU tick budget") {
        bitactor_engine_t* engine;
        signal_t signal;
        result_t result;
        uint64_t ticks;
        
        GIVEN("a BitActor engine is initialized",
            engine = bitactor_init();
            EXPECT(engine != NULL);
            EXPECT(bitactor_is_ready(engine));
        );
        
        WHEN("a high-priority signal is processed",
            signal = BUILD_SIGNAL(
                .id = 0x1001,
                .kind = 0x01,
                .priority = 255,
                .payload = 0xDEADBEEF
            );
            
            uint64_t start = rdtsc_portable();
            result = bitactor_tick(engine, &signal);
            ticks = rdtsc_portable() - start;
        );
        
        THEN("the processing completes within 8 ticks",
            EXPECT_LT(ticks, 9);
            EXPECT_EQ(result.status, BITACTOR_OK);
        );
        
        AND("the result contains valid execution metadata",
            EXPECT_EQ(result.signal_id, 0x1001);
            EXPECT_GT(result.exec_hash, 0);
            EXPECT_LT(result.ticks, 9);
        );
    } END_SCENARIO
    
    SCENARIO("Zero heap allocation after initialization") {
        bitactor_engine_t* engine;
        size_t heap_delta;
        
        GIVEN("memory usage is tracked before initialization",
            TRACK_MEMORY_START();
        );
        
        WHEN("BitActor is initialized and processes 10,000 signals",
            engine = bitactor_init();
            
            for (int i = 0; i < 10000; i++) {
                signal_t sig = BUILD_SIGNAL(
                    .id = i,
                    .kind = (uint8_t)(i % 256),
                    .payload = i * 31337
                );
                bitactor_tick(engine, &sig);
            }
            
            TRACK_MEMORY_END();
            heap_delta = _mem_delta;
        );
        
        THEN("no additional heap memory is allocated",
            EXPECT_EQ(heap_delta, 0);
        );
    } END_SCENARIO
    
    SCENARIO("Deterministic execution for identical signals") {
        bitactor_engine_t* engine;
        signal_t signal;
        result_t results[100];
        
        GIVEN("a BitActor engine with a registered handler",
            engine = bitactor_init();
            // Handler is pre-registered in init
        );
        
        WHEN("the same signal is processed 100 times",
            signal = BUILD_SIGNAL(
                .id = 0x42,
                .kind = 0x01,
                .payload = 0xCAFEBABE,
                .timestamp = 1234567890
            );
            
            for (int i = 0; i < 100; i++) {
                results[i] = bitactor_tick(engine, &signal);
            }
        );
        
        THEN("all executions produce identical results",
            uint32_t first_hash = results[0].exec_hash;
            uint8_t first_ticks = results[0].ticks;
            
            for (int i = 1; i < 100; i++) {
                EXPECT_EQ(results[i].exec_hash, first_hash);
                EXPECT_EQ(results[i].status, BITACTOR_OK);
                // Allow ±1 tick variance for cache effects
                EXPECT_LT(abs(results[i].ticks - first_ticks), 2);
            }
        );
    } END_SCENARIO
    
    SCENARIO("Signal queue handles burst traffic") {
        bitactor_engine_t* engine;
        bool enqueue_success;
        uint32_t processed;
        
        GIVEN("a BitActor engine is ready",
            engine = bitactor_init();
        );
        
        WHEN("1000 signals are enqueued rapidly",
            for (int i = 0; i < 1000; i++) {
                signal_t sig = BUILD_SIGNAL(
                    .id = i,
                    .kind = (uint8_t)(i % 4),
                    .payload = i
                );
                enqueue_success = bitactor_enqueue(engine, &sig);
                EXPECT(enqueue_success);
            }
        );
        
        THEN("all signals can be drained efficiently",
            uint32_t pending_before = bitactor_pending_count(engine);
            EXPECT_EQ(pending_before, 1000);
            
            processed = bitactor_drain(engine, 1000);
            EXPECT_EQ(processed, 1000);
            
            uint32_t pending_after = bitactor_pending_count(engine);
            EXPECT_EQ(pending_after, 0);
        );
    } END_SCENARIO
    
    SCENARIO("Performance degrades gracefully under load") {
        bitactor_engine_t* engine;
        uint64_t baseline_ticks, loaded_ticks;
        
        GIVEN("a baseline measurement with no load",
            engine = bitactor_init();
            signal_t sig = BUILD_SIGNAL(.kind = 0x01, .payload = 0x1234);
            
            // Warmup
            for (int i = 0; i < 100; i++) {
                bitactor_tick(engine, &sig);
            }
            
            // Measure baseline
            uint64_t start = rdtsc_portable();
            bitactor_tick(engine, &sig);
            baseline_ticks = rdtsc_portable() - start;
        );
        
        WHEN("the system is under heavy load",
            // Fill the queue
            for (int i = 0; i < BITACTOR_MAX_SIGNALS - 1; i++) {
                signal_t load_sig = BUILD_SIGNAL(
                    .kind = (uint8_t)(i % 256),
                    .payload = i
                );
                bitactor_enqueue(engine, &load_sig);
            }
            
            // Measure under load
            uint64_t load_start = rdtsc_portable();
            bitactor_tick(engine, &sig);
            loaded_ticks = rdtsc_portable() - load_start;
        );
        
        THEN("latency remains within 2x of baseline",
            EXPECT_LT(loaded_ticks, baseline_ticks * 2);
        );
        
        AND("still meets the 8-tick budget",
            EXPECT_LT(loaded_ticks, 9);
        );
    } END_SCENARIO
    
    SCENARIO("Invalid signals are handled safely") {
        bitactor_engine_t* engine;
        result_t result;
        
        GIVEN("a BitActor engine is initialized",
            engine = bitactor_init();
        );
        
        WHEN("a null signal is processed",
            result = bitactor_tick(engine, NULL);
        );
        
        THEN("an error status is returned",
            EXPECT_EQ(result.status, BITACTOR_INVALID_SIGNAL);
        );
        
        AND("the engine remains operational",
            signal_t valid_sig = BUILD_SIGNAL(.kind = 0x01);
            result_t valid_result = bitactor_tick(engine, &valid_sig);
            EXPECT_EQ(valid_result.status, BITACTOR_OK);
        );
    } END_SCENARIO
    
    SCENARIO("Engine statistics accurately track operations") {
        bitactor_engine_t* engine;
        struct {
            uint64_t total_signals;
            uint64_t total_ticks;
            uint32_t max_ticks;
            uint32_t pending_signals;
            float avg_ticks;
        } stats;
        
        GIVEN("a fresh BitActor engine",
            engine = bitactor_init();
        );
        
        WHEN("exactly 1000 signals are processed",
            for (int i = 0; i < 1000; i++) {
                signal_t sig = BUILD_SIGNAL(
                    .kind = (uint8_t)(i % 10),
                    .payload = i
                );
                bitactor_tick(engine, &sig);
            }
            
            bitactor_stats(engine, &stats);
        );
        
        THEN("statistics reflect accurate counts",
            EXPECT_EQ(stats.total_signals, 1000);
            EXPECT_GT(stats.total_ticks, 0);
            EXPECT_LT(stats.max_ticks, 9);
            EXPECT_GT(stats.avg_ticks, 0.0f);
            EXPECT_LT(stats.avg_ticks, 8.0f);
        );
    } END_SCENARIO
}