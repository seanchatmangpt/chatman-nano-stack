/*
 * Real BitActor Integration Layer BDD Tests
 * Testing the actual CNS integration implementation
 * NO MOCKS - Real integration testing
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../bitactor/include/bitactor_public.h"
#include "../bitactor/integration/cns_integration.h"
#include "../src/cns/cns_pipeline.h"
#include <string.h>
#include <stdlib.h>

// Real test data structures
typedef struct {
    uint32_t quotes_processed;
    uint32_t orders_processed;
    uint32_t errors_encountered;
    uint64_t total_latency_ticks;
} integration_stats_t;

// Test helpers
static integration_stats_t test_stats = {0};

// Callback for pipeline integration
static void test_pipeline_callback(const char* symbol, double price, uint64_t timestamp) {
    test_stats.quotes_processed++;
    test_stats.total_latency_ticks += rdtsc_portable() - timestamp;
}

FEATURE(BitActor_Integration_Real_Implementation) {
    
    SCENARIO("CNS BitActor integration initialization and configuration") {
        cns_bitactor_integration* integration = NULL;
        
        GIVEN("uninitialized CNS integration layer",
            integration = NULL;
        );
        
        WHEN("integration layer is initialized",
            uint64_t start = rdtsc_portable();
            integration = cns_bitactor_init();
            uint64_t end = rdtsc_portable();
            uint64_t init_time = end - start;
        );
        
        THEN("integration initializes with all subsystems connected",
            EXPECT_NE(integration, NULL);
            EXPECT(integration->initialized);
            EXPECT_NE(integration->engine, NULL);
            EXPECT_NE(integration->pipeline, NULL);
            EXPECT_EQ(integration->stats.total_quotes, 0);
            EXPECT_EQ(integration->stats.total_orders, 0);
            
            printf("       Integration init time: %llu ticks\n", 
                   (unsigned long long)init_time);
            
            // Verify subsystem connections
            EXPECT(integration->engine->initialized);
            EXPECT_EQ(integration->pipeline->stage_count, 0);
        );
        
        AND("default handlers are registered",
            // Verify market data handlers
            EXPECT_GT(integration->engine->dispatch.handler_count, 0);
            
            // Check for essential handlers
            bool has_quote_handler = false;
            bool has_order_handler = false;
            
            for (uint32_t i = 0; i < integration->engine->dispatch.handler_count; i++) {
                if (integration->engine->dispatch.handlers[i].kind == SIGNAL_KIND_QUOTE) {
                    has_quote_handler = true;
                }
                if (integration->engine->dispatch.handlers[i].kind == SIGNAL_KIND_ORDER) {
                    has_order_handler = true;
                }
            }
            
            EXPECT(has_quote_handler);
            EXPECT(has_order_handler);
        );
    } END_SCENARIO
    
    SCENARIO("Pipeline integration with CNS quote processing") {
        cns_bitactor_integration* integration = cns_bitactor_init();
        CNSPipeline test_pipeline;
        
        GIVEN("integration with configured pipeline",
            EXPECT_NE(integration, NULL);
            
            // Configure test pipeline
            cns_pipeline_init(&test_pipeline);
            cns_pipeline_add_stage(&test_pipeline, "quote_validator", NULL);
            cns_pipeline_add_stage(&test_pipeline, "price_calculator", NULL);
            
            // Register pipeline with integration
            EXPECT(cns_bitactor_register_pipeline(integration, &test_pipeline));
            integration->pipeline = &test_pipeline;
        );
        
        WHEN("quote signal is processed through pipeline",
            cns_quote_t test_quote = {
                .symbol = "AAPL",
                .bid = 150.25,
                .ask = 150.30,
                .timestamp = rdtsc_portable()
            };
            
            uint64_t start = rdtsc_portable();
            bool result = cns_bitactor_process_quote(integration, &test_quote);
            uint64_t end = rdtsc_portable();
            uint64_t process_time = end - start;
        );
        
        THEN("quote processing completes within 8-tick budget",
            EXPECT(result);
            EXPECT_LE(process_time, 8);
            
            // Verify stats updated
            EXPECT_EQ(integration->stats.total_quotes, 1);
            EXPECT_GT(integration->stats.last_quote_ticks, 0);
            EXPECT_LE(integration->stats.last_quote_ticks, 8);
            
            printf("       Quote processing: %llu ticks\n",
                   (unsigned long long)process_time);
        );
        
        AND("pipeline stages executed in order",
            // Verify telemetry captured pipeline execution
            EXPECT_GT(integration->engine->telemetry.frame_count, 0);
            
            // Check for pipeline stage markers in telemetry
            bool found_validator = false;
            bool found_calculator = false;
            
            for (uint32_t i = 0; i < integration->engine->telemetry.frame_count; i++) {
                telemetry_frame_t* frame = &integration->engine->telemetry.frames[i];
                if (frame->operation == TRACE_OP_HANDLER) {
                    if (frame->exec_hash == hash_string("quote_validator")) {
                        found_validator = true;
                    }
                    if (frame->exec_hash == hash_string("price_calculator")) {
                        found_calculator = true;
                    }
                }
            }
            
            EXPECT(found_validator);
            EXPECT(found_calculator);
        );
    } END_SCENARIO
    
    SCENARIO("Order processing with risk validation") {
        cns_bitactor_integration* integration = cns_bitactor_init();
        
        GIVEN("integration ready for order processing",
            EXPECT_NE(integration, NULL);
            
            // Register order validation handler
            cns_bitactor_register_handlers(integration);
        );
        
        WHEN("orders are processed with risk checks",
            cns_order_t test_orders[] = {
                {.symbol = "AAPL", .quantity = 100, .price = 150.00, .side = 'B'},
                {.symbol = "GOOGL", .quantity = 50, .price = 2800.00, .side = 'S'},
                {.symbol = "MSFT", .quantity = 200, .price = 380.00, .side = 'B'}
            };
            
            uint64_t total_time = 0;
            
            for (int i = 0; i < 3; i++) {
                uint64_t start = rdtsc_portable();
                bool result = cns_bitactor_process_order(integration, &test_orders[i]);
                uint64_t end = rdtsc_portable();
                
                EXPECT(result);
                total_time += (end - start);
            }
        );
        
        THEN("all orders process within tick budget with risk validation",
            uint64_t avg_time = total_time / 3;
            EXPECT_LE(avg_time, 8);
            
            EXPECT_EQ(integration->stats.total_orders, 3);
            EXPECT_EQ(integration->stats.risk_violations, 0);
            
            printf("       Average order processing: %llu ticks\n",
                   (unsigned long long)avg_time);
        );
        
        AND("order telemetry includes risk metrics",
            // Verify risk calculations in telemetry
            bool found_risk_check = false;
            
            for (uint32_t i = 0; i < integration->engine->telemetry.frame_count; i++) {
                telemetry_frame_t* frame = &integration->engine->telemetry.frames[i];
                if (frame->flags & TELEMETRY_FLAG_RISK_CHECK) {
                    found_risk_check = true;
                    EXPECT_EQ(frame->result.status, RESULT_STATUS_SUCCESS);
                }
            }
            
            EXPECT(found_risk_check);
        );
    } END_SCENARIO
    
    SCENARIO("Statistics collection and monitoring") {
        cns_bitactor_integration* integration = cns_bitactor_init();
        
        GIVEN("integration with activity to monitor",
            // Process mixed signals
            for (int i = 0; i < 100; i++) {
                if (i % 2 == 0) {
                    cns_quote_t quote = {
                        .symbol = "TEST",
                        .bid = 100.0 + i * 0.01,
                        .ask = 100.05 + i * 0.01,
                        .timestamp = rdtsc_portable()
                    };
                    cns_bitactor_process_quote(integration, &quote);
                } else {
                    cns_order_t order = {
                        .symbol = "TEST",
                        .quantity = 100,
                        .price = 100.0 + i * 0.01,
                        .side = (i % 4 == 1) ? 'B' : 'S'
                    };
                    cns_bitactor_process_order(integration, &order);
                }
            }
        );
        
        WHEN("statistics are retrieved",
            integration_stats_t* stats = bitactor_get_stats(integration);
            uint64_t total_processed = stats->total_quotes + stats->total_orders;
        );
        
        THEN("statistics accurately reflect processing",
            EXPECT_NE(stats, NULL);
            EXPECT_EQ(stats->total_quotes, 50);
            EXPECT_EQ(stats->total_orders, 50);
            EXPECT_EQ(total_processed, 100);
            
            // Performance metrics
            EXPECT_GT(stats->avg_quote_ticks, 0);
            EXPECT_LE(stats->avg_quote_ticks, 8);
            EXPECT_GT(stats->avg_order_ticks, 0);
            EXPECT_LE(stats->avg_order_ticks, 8);
            
            printf("       Integration Statistics:\n");
            printf("       Quotes: %u (avg %llu ticks)\n", 
                   stats->total_quotes,
                   (unsigned long long)stats->avg_quote_ticks);
            printf("       Orders: %u (avg %llu ticks)\n",
                   stats->total_orders,
                   (unsigned long long)stats->avg_order_ticks);
        );
        
        AND("statistics can be reset",
            bitactor_reset_stats(integration);
            stats = bitactor_get_stats(integration);
            
            EXPECT_EQ(stats->total_quotes, 0);
            EXPECT_EQ(stats->total_orders, 0);
            EXPECT_EQ(stats->total_errors, 0);
        );
    } END_SCENARIO
    
    SCENARIO("Error handling and recovery mechanisms") {
        cns_bitactor_integration* integration = cns_bitactor_init();
        
        GIVEN("integration ready for error testing",
            EXPECT_NE(integration, NULL);
        );
        
        WHEN("invalid signals trigger error handling",
            // Test null quote
            bool null_result = cns_bitactor_process_quote(integration, NULL);
            
            // Test invalid order (negative quantity)
            cns_order_t invalid_order = {
                .symbol = "INVALID",
                .quantity = -100,
                .price = 100.00,
                .side = 'B'
            };
            bool invalid_result = cns_bitactor_process_order(integration, &invalid_order);
            
            // Test oversized symbol
            cns_quote_t oversized_quote = {
                .symbol = "VERYLONGSYMBOLNAMETHATEXCEEDSLIMITS",
                .bid = 100.00,
                .ask = 100.05,
                .timestamp = rdtsc_portable()
            };
            bool oversized_result = cns_bitactor_process_quote(integration, &oversized_quote);
        );
        
        THEN("errors are handled gracefully within tick budget",
            EXPECT_FALSE(null_result);
            EXPECT_FALSE(invalid_result);
            EXPECT_FALSE(oversized_result);
            
            // Verify error stats
            integration_stats_t* stats = bitactor_get_stats(integration);
            EXPECT_GE(stats->total_errors, 3);
            
            // Verify system remains operational
            cns_quote_t valid_quote = {
                .symbol = "AAPL",
                .bid = 150.00,
                .ask = 150.05,
                .timestamp = rdtsc_portable()
            };
            bool recovery_result = cns_bitactor_process_quote(integration, &valid_quote);
            EXPECT(recovery_result);
        );
        
        AND("error telemetry provides diagnostics",
            // Check for error frames in telemetry
            int error_frames = 0;
            
            for (uint32_t i = 0; i < integration->engine->telemetry.frame_count; i++) {
                telemetry_frame_t* frame = &integration->engine->telemetry.frames[i];
                if (frame->result.status == RESULT_STATUS_ERROR) {
                    error_frames++;
                    
                    // Verify error details captured
                    EXPECT_NE(frame->result.exec_hash, 0);
                    EXPECT_GT(frame->timestamp, 0);
                }
            }
            
            EXPECT_GE(error_frames, 3);
            printf("       Error frames captured: %d\n", error_frames);
        );
    } END_SCENARIO
    
    SCENARIO("Pipeline tick integration with CNS stages") {
        cns_bitactor_integration* integration = cns_bitactor_init();
        CNSPipeline pipeline;
        
        GIVEN("integration with multi-stage pipeline",
            cns_pipeline_init(&pipeline);
            
            // Add real pipeline stages
            cns_pipeline_add_stage(&pipeline, "parse", NULL);
            cns_pipeline_add_stage(&pipeline, "validate", NULL);
            cns_pipeline_add_stage(&pipeline, "enrich", NULL);
            cns_pipeline_add_stage(&pipeline, "route", NULL);
            
            cns_bitactor_register_pipeline(integration, &pipeline);
        );
        
        WHEN("integrated tick processes pipeline and BitActor",
            // Add signals for processing
            for (int i = 0; i < 10; i++) {
                cns_quote_t quote = {
                    .symbol = "TEST",
                    .bid = 100.00 + i * 0.01,
                    .ask = 100.05 + i * 0.01,
                    .timestamp = rdtsc_portable()
                };
                cns_bitactor_process_quote(integration, &quote);
            }
            
            // Execute integrated tick
            uint64_t start = rdtsc_portable();
            uint32_t processed = cns_bitactor_tick_integration(integration);
            uint64_t end = rdtsc_portable();
            uint64_t tick_time = end - start;
        );
        
        THEN("integrated tick maintains 8-tick budget across components",
            EXPECT_GT(processed, 0);
            EXPECT_LE(processed, 10);
            
            // Verify timing
            uint64_t avg_per_signal = tick_time / processed;
            EXPECT_LE(avg_per_signal, 8);
            
            printf("       Integrated tick: %u signals in %llu ticks (avg: %llu/signal)\n",
                   processed, (unsigned long long)tick_time,
                   (unsigned long long)avg_per_signal);
        );
        
        AND("pipeline stages execute in correct sequence",
            // Verify stage execution order in telemetry
            const char* expected_stages[] = {"parse", "validate", "enrich", "route"};
            int stage_index = 0;
            
            for (uint32_t i = 0; i < integration->engine->telemetry.frame_count; i++) {
                telemetry_frame_t* frame = &integration->engine->telemetry.frames[i];
                
                if (frame->operation == TRACE_OP_STAGE && stage_index < 4) {
                    uint32_t expected_hash = hash_string(expected_stages[stage_index]);
                    if (frame->exec_hash == expected_hash) {
                        stage_index++;
                    }
                }
            }
            
            EXPECT_EQ(stage_index, 4); // All stages executed
        );
    } END_SCENARIO
    
    SCENARIO("8-tick compliance validation for integration layer") {
        cns_bitactor_integration* integration = cns_bitactor_init();
        const int SAMPLES = 1000;
        
        GIVEN("integration configured for compliance testing",
            cns_bitactor_register_handlers(integration);
        );
        
        WHEN("compliance is validated over many operations",
            tick_compliance_t compliance = {0};
            
            for (int i = 0; i < SAMPLES; i++) {
                if (i % 3 == 0) {
                    cns_quote_t quote = {
                        .symbol = "COMP",
                        .bid = 100.00,
                        .ask = 100.05,
                        .timestamp = rdtsc_portable()
                    };
                    cns_bitactor_process_quote(integration, &quote);
                } else {
                    cns_order_t order = {
                        .symbol = "COMP",
                        .quantity = 100,
                        .price = 100.00,
                        .side = 'B'
                    };
                    cns_bitactor_process_order(integration, &order);
                }
            }
            
            compliance = validate_8tick_compliance(integration);
        );
        
        THEN("compliance meets 99.9% threshold",
            EXPECT_GE(compliance.success_rate, 0.999);
            EXPECT_LE(compliance.avg_ticks, 8);
            EXPECT_LE(compliance.max_ticks, 16); // Allow rare outliers
            
            printf("       8-Tick Compliance Report:\n");
            printf("       Success Rate: %.2f%%\n", compliance.success_rate * 100);
            printf("       Average Ticks: %.2f\n", compliance.avg_ticks);
            printf("       Max Ticks: %llu\n", (unsigned long long)compliance.max_ticks);
            printf("       Violations: %u/%u\n", compliance.violations, compliance.total_ops);
        );
    } END_SCENARIO
}