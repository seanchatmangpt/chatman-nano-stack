/*
 * Example BitActor BDD Specification
 * Demonstrates lightweight BDD testing in C
 */
#include "bdd_framework.h"
#include <string.h>

/* Simple signal processing system for demo */
typedef struct {
    uint32_t id;
    uint8_t type;
    uint64_t payload;
} demo_signal_t;

typedef struct {
    uint32_t processed_count;
    uint64_t last_payload;
    bool initialized;
} demo_engine_t;

/* Demo functions */
static demo_engine_t* demo_init(void) {
    static demo_engine_t engine = {0};
    engine.initialized = true;
    engine.processed_count = 0;
    return &engine;
}

static int demo_process(demo_engine_t* engine, demo_signal_t* signal) {
    if (!engine || !signal) return -1;
    engine->processed_count++;
    engine->last_payload = signal->payload;
    return 0;
}

FEATURE(Simple_Signal_Processing_Demo) {
    
    SCENARIO("Engine processes signals successfully") {
        demo_engine_t* engine;
        demo_signal_t signal;
        int result;
        
        GIVEN("an initialized processing engine",
            engine = demo_init();
            EXPECT(engine != NULL);
            EXPECT(engine->initialized);
        );
        
        WHEN("a valid signal is processed",
            signal = (demo_signal_t){
                .id = 42,
                .type = 1,
                .payload = 0xCAFEBABE
            };
            result = demo_process(engine, &signal);
        );
        
        THEN("the signal is processed without errors",
            EXPECT_EQ(result, 0);
            EXPECT_EQ(engine->processed_count, 1);
            EXPECT_EQ(engine->last_payload, 0xCAFEBABE);
        );
    } END_SCENARIO
    
    SCENARIO("Multiple signals are processed in order") {
        demo_engine_t* engine;
        
        GIVEN("a fresh engine instance",
            engine = demo_init();
        );
        
        WHEN("three signals are processed sequentially",
            demo_signal_t signals[] = {
                {.id = 1, .type = 1, .payload = 100},
                {.id = 2, .type = 2, .payload = 200},
                {.id = 3, .type = 1, .payload = 300}
            };
            
            for (int i = 0; i < 3; i++) {
                demo_process(engine, &signals[i]);
            }
        );
        
        THEN("all signals are counted correctly",
            EXPECT_EQ(engine->processed_count, 3);
        );
        
        AND("the last payload is preserved",
            EXPECT_EQ(engine->last_payload, 300);
        );
    } END_SCENARIO
    
    SCENARIO("Invalid inputs are handled gracefully") {
        demo_engine_t* engine;
        int result;
        
        GIVEN("a valid engine",
            engine = demo_init();
        );
        
        WHEN("a null signal is provided",
            result = demo_process(engine, NULL);
        );
        
        THEN("an error code is returned",
            EXPECT_EQ(result, -1);
        );
        
        AND("the engine state remains unchanged",
            EXPECT_EQ(engine->processed_count, 0);
        );
    } END_SCENARIO
}