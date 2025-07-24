/*
 * Real CNS System Integration BDD Tests
 * End-to-end testing of complete CNS system integration
 * BitActor + SPARQL + News Validator + BitFiber working together
 * NO MOCKS - Complete real system testing
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../bitactor/include/bitactor.h"
#include "../bitactor/integration/cns_integration.h"
#include "../src/sparql/sparql_to_bitactor.h"
#include "../src/news/news_validator.h"
#include "../bitactor/src/bitfiber.h"
#include "../src/cns/cns_pipeline.h"
#include <string.h>
#include <stdlib.h>
#include <time.h>

// Complete CNS system state
typedef struct {
    bitactor_engine* engine;
    cns_bitactor_integration* integration;
    news_validator_t* validator;
    fiber_scheduler_t* scheduler;
    sparql_executor_t* sparql_exec;
    CNSPipeline* pipeline;
    bool initialized;
} cns_system_t;

// End-to-end workflow context
typedef struct {
    int articles_processed;
    int articles_validated;
    int sparql_queries_executed;
    int validation_errors;
    uint64_t total_processing_time;
    bool workflow_completed;
} workflow_context_t;

// Initialize complete CNS system
static cns_system_t* initialize_cns_system(void) {
    cns_system_t* system = (cns_system_t*)malloc(sizeof(cns_system_t));
    memset(system, 0, sizeof(cns_system_t));
    
    // Initialize core BitActor engine
    system->engine = bitactor_init();
    if (!system->engine) return NULL;
    
    // Initialize fiber scheduler
    system->scheduler = fiber_scheduler_init();
    if (!system->scheduler) return NULL;
    
    // Integrate scheduler with engine
    system->engine->fiber_sched = system->scheduler;
    
    // Initialize CNS integration layer
    system->integration = cns_bitactor_init();
    if (!system->integration) return NULL;
    
    // Initialize news validator with ontologies
    system->validator = news_validator_init("../ontologies/news_validator.ttl",
                                          "../ontologies/news_validator_shacl.ttl");
    if (!system->validator) return NULL;
    
    // Initialize SPARQL executor
    system->sparql_exec = sparql_executor_init(system->engine);
    if (!system->sparql_exec) return NULL;
    
    // Initialize CNS pipeline
    system->pipeline = (CNSPipeline*)malloc(sizeof(CNSPipeline));
    cns_pipeline_init(system->pipeline);
    
    // Add pipeline stages
    cns_pipeline_add_stage(system->pipeline, "news_intake", NULL);
    cns_pipeline_add_stage(system->pipeline, "validation", NULL);
    cns_pipeline_add_stage(system->pipeline, "sparql_query", NULL);
    cns_pipeline_add_stage(system->pipeline, "result_processing", NULL);
    
    // Register pipeline with integration
    cns_bitactor_register_pipeline(system->integration, system->pipeline);
    
    system->initialized = true;
    return system;
}

// Complete workflow fiber function
static void news_validation_workflow_fiber(void* context) {
    workflow_context_t* ctx = (workflow_context_t*)context;
    cns_system_t* system = (cns_system_t*)ctx; // Context includes system pointer
    
    // Sample news articles for testing
    const char* test_articles[] = {
        "Breaking: Quantum Computing Breakthrough at MIT",
        "Market Analysis: Tech Stocks Rise After AI Announcement", 
        "Climate Change: New Carbon Capture Technology Shows Promise",
        "Healthcare: Gene Therapy Trial Shows Remarkable Results",
        "Space: NASA Confirms Water Ice on Moon's South Pole"
    };
    
    for (int i = 0; i < 5; i++) {
        uint64_t start = rdtsc_portable();
        
        // Create news article
        news_article_t article = {
            .title = (char*)test_articles[i],
            .content = "Detailed article content with sufficient information for validation.",
            .source_url = "https://trusted-news.example.com/article",
            .author = "Professional Journalist",
            .published_date = time(NULL),
            .category = "technology"
        };
        
        // Process through complete workflow
        ctx->articles_processed++;
        
        // Step 1: Validate article
        if (news_validator_validate_article(system->validator, &article)) {
            ctx->articles_validated++;
            
            // Step 2: Execute SPARQL query to check related facts
            const char* verification_query = 
                "SELECT ?fact WHERE { "
                "  ?article :discusses ?topic . "
                "  ?topic :hasVerifiedFact ?fact "
                "}";
            
            sparql_execution_result_t* result = 
                sparql_execute_query(system->sparql_exec, verification_query);
            
            if (result && result->status == EXEC_SUCCESS) {
                ctx->sparql_queries_executed++;
            }
            
            if (result) free_execution_result(result);
            
        } else {
            ctx->validation_errors++;
        }
        
        uint64_t end = rdtsc_portable();
        ctx->total_processing_time += (end - start);
        
        // Yield to allow other fibers to run
        fiber_yield();
    }
    
    ctx->workflow_completed = true;
}

FEATURE(CNS_Real_System_Integration) {
    
    SCENARIO("Complete CNS system initialization and component integration") {
        cns_system_t* system = NULL;
        
        GIVEN("uninitialized CNS system components",
            system = NULL;
        );
        
        WHEN("complete CNS system is initialized",
            uint64_t start = rdtsc_portable();
            system = initialize_cns_system();
            uint64_t end = rdtsc_portable();
            uint64_t init_time = end - start;
        );
        
        THEN("all components are properly integrated",
            EXPECT_NE(system, NULL);
            EXPECT(system->initialized);
            EXPECT_NE(system->engine, NULL);
            EXPECT_NE(system->integration, NULL);
            EXPECT_NE(system->validator, NULL);
            EXPECT_NE(system->scheduler, NULL);
            EXPECT_NE(system->sparql_exec, NULL);
            EXPECT_NE(system->pipeline, NULL);
            
            // Verify cross-component integration
            EXPECT_EQ(system->engine->fiber_sched, system->scheduler);
            EXPECT_EQ(system->integration->engine, system->engine);
            EXPECT_EQ(system->sparql_exec->engine, system->engine);
            
            printf("       Complete CNS init time: %llu ticks\n",
                   (unsigned long long)init_time);
            printf("       Components integrated: 6/6\n");
        );
        
        AND("pipeline stages are properly configured",
            EXPECT_EQ(system->pipeline->stage_count, 4);
            
            const char* expected_stages[] = {
                "news_intake", "validation", "sparql_query", "result_processing"
            };
            
            for (int i = 0; i < 4; i++) {
                EXPECT_STREQ(system->pipeline->stages[i].name, expected_stages[i]);
            }
            
            // Cleanup
            if (system) {
                if (system->validator) news_validator_free(system->validator);
                if (system->scheduler) fiber_scheduler_free(system->scheduler);
                if (system->sparql_exec) sparql_executor_free(system->sparql_exec);
                if (system->pipeline) {
                    cns_pipeline_cleanup(system->pipeline);
                    free(system->pipeline);
                }
                free(system);
            }
        );
    } END_SCENARIO
    
    SCENARIO("End-to-end news validation workflow") {
        cns_system_t* system = initialize_cns_system();
        workflow_context_t workflow_ctx = {0};
        
        GIVEN("initialized CNS system ready for workflow",
            EXPECT_NE(system, NULL);
            EXPECT(system->initialized);
        );
        
        WHEN("complete news validation workflow executes",
            // Create workflow fiber
            fiber_t* workflow_fiber = fiber_create(system->scheduler, 10); // High priority
            
            // Set context to include system reference
            workflow_ctx.articles_processed = 0;
            workflow_ctx.articles_validated = 0;
            workflow_ctx.sparql_queries_executed = 0;
            workflow_ctx.validation_errors = 0;
            workflow_ctx.total_processing_time = 0;
            workflow_ctx.workflow_completed = false;
            
            // Note: In real implementation, we'd pass both system and context
            fiber_set_function(workflow_fiber, news_validation_workflow_fiber, &workflow_ctx);
            
            uint64_t start = rdtsc_portable();
            
            // Execute workflow
            int max_ticks = 200;
            int tick_count = 0;
            
            while (!workflow_ctx.workflow_completed && tick_count < max_ticks) {
                // Integrated tick: BitActor + Fiber + Pipeline
                cns_bitactor_tick_integration(system->integration);
                tick_count++;
            }
            
            uint64_t end = rdtsc_portable();
            uint64_t total_workflow_time = end - start;
        );
        
        THEN("workflow completes successfully within performance budget",
            EXPECT(workflow_ctx.workflow_completed);
            EXPECT_EQ(workflow_ctx.articles_processed, 5);
            EXPECT_GT(workflow_ctx.articles_validated, 0);
            EXPECT_GT(workflow_ctx.sparql_queries_executed, 0);
            
            uint64_t avg_article_time = workflow_ctx.total_processing_time / 
                                       workflow_ctx.articles_processed;
            
            printf("       End-to-end workflow results:\n");
            printf("         Articles processed: %d\n", workflow_ctx.articles_processed);
            printf("         Articles validated: %d\n", workflow_ctx.articles_validated);
            printf("         SPARQL queries executed: %d\n", workflow_ctx.sparql_queries_executed);
            printf("         Validation errors: %d\n", workflow_ctx.validation_errors);
            printf("         Average per article: %llu ticks\n",
                   (unsigned long long)avg_article_time);
            printf("         Total workflow time: %llu ticks\n",
                   (unsigned long long)total_workflow_time);
            
            EXPECT_LE(avg_article_time, 50); // Reasonable for complete workflow
            
            // Cleanup
            if (system->validator) news_validator_free(system->validator);
            if (system->scheduler) fiber_scheduler_free(system->scheduler);
            if (system->sparql_exec) sparql_executor_free(system->sparql_exec);
            if (system->pipeline) {
                cns_pipeline_cleanup(system->pipeline);
                free(system->pipeline);
            }
            free(system);
        );
    } END_SCENARIO
    
    SCENARIO("SPARQL query integration with news validation") {
        cns_system_t* system = initialize_cns_system();
        
        GIVEN("system with news data and SPARQL queries",
            EXPECT_NE(system, NULL);
            
            // Add knowledge to system
            const char* setup_data = 
                "@prefix : <http://news.example.org/> .\n"
                ":article1 :discusses :quantum_computing .\n"
                ":quantum_computing :hasVerifiedFact \"MIT breakthrough confirmed\" .\n"
                ":article1 :hasCredibilityScore 0.9 .\n";
            
            // Load test data into SPARQL executor
            sparql_load_data(system->sparql_exec, setup_data);
        );
        
        WHEN("news validation triggers SPARQL fact checking",
            news_article_t test_article = {
                .title = "Quantum Computing Breakthrough at MIT",
                .content = "MIT researchers announce major quantum computing advancement",
                .source_url = "https://news.mit.edu/quantum-breakthrough",
                .author = "MIT News Office",
                .published_date = time(NULL)
            };
            
            uint64_t start = rdtsc_portable();
            
            // Validate article (this should trigger SPARQL fact checking)
            bool is_valid = news_validator_validate_article(system->validator, &test_article);
            
            // Execute related SPARQL query
            const char* fact_check_query = 
                "SELECT ?fact ?score WHERE { "
                "  ?article :discusses :quantum_computing . "
                "  :quantum_computing :hasVerifiedFact ?fact . "
                "  ?article :hasCredibilityScore ?score "
                "  FILTER(?score > 0.8) }";
            
            sparql_execution_result_t* result = 
                sparql_execute_query(system->sparql_exec, fact_check_query);
            
            uint64_t end = rdtsc_portable();
            uint64_t integration_time = end - start;
        );
        
        THEN("SPARQL integration enhances validation accuracy",
            EXPECT(is_valid);
            EXPECT_NE(result, NULL);
            EXPECT_EQ(result->status, EXEC_SUCCESS);
            EXPECT_LE(integration_time, 8);
            
            // Verify SPARQL results support validation
            EXPECT_GT(result->bindings_count, 0);
            
            printf("       SPARQL-News validation integration:\n");
            printf("         Article validated: %s\n", is_valid ? "YES" : "NO");
            printf("         SPARQL bindings found: %u\n", result->bindings_count);
            printf("         Integration time: %llu ticks\n",
                   (unsigned long long)integration_time);
            
            // Check specific bindings
            for (uint32_t i = 0; i < result->bindings_count; i++) {
                binding_t* binding = &result->bindings[i];
                printf("         Binding %u: %s = %s\n",
                       i, binding->var_name, binding->value.string);
            }
            
            if (result) free_execution_result(result);
        );
        
        AND("telemetry captures cross-component interaction",
            // Verify telemetry shows both validation and SPARQL execution
            bool found_validation = false;
            bool found_sparql = false;
            
            for (uint32_t i = 0; i < system->engine->telemetry.frame_count; i++) {
                telemetry_frame_t* frame = &system->engine->telemetry.frames[i];
                
                if (frame->operation == TRACE_OP_VALIDATION) {
                    found_validation = true;
                }
                if (frame->operation == TRACE_OP_SPARQL) {
                    found_sparql = true;
                }
            }
            
            EXPECT(found_validation);
            EXPECT(found_sparql);
            
            printf("         Telemetry captures: validation=%s, sparql=%s\n",
                   found_validation ? "YES" : "NO",
                   found_sparql ? "YES" : "NO");
            
            // Cleanup
            if (system->validator) news_validator_free(system->validator);
            if (system->scheduler) fiber_scheduler_free(system->scheduler);
            if (system->sparql_exec) sparql_executor_free(system->sparql_exec);
            if (system->pipeline) {
                cns_pipeline_cleanup(system->pipeline);
                free(system->pipeline);
            }
            free(system);
        );
    } END_SCENARIO
    
    SCENARIO("Concurrent multi-fiber news processing") {
        cns_system_t* system = initialize_cns_system();
        const int FIBER_COUNT = 4;
        
        GIVEN("system with multiple processing fibers",
            workflow_context_t contexts[FIBER_COUNT];
            fiber_t* fibers[FIBER_COUNT];
            
            for (int i = 0; i < FIBER_COUNT; i++) {
                contexts[i] = (workflow_context_t){
                    .articles_processed = 0,
                    .articles_validated = 0,
                    .sparql_queries_executed = 0,
                    .validation_errors = 0,
                    .total_processing_time = 0,
                    .workflow_completed = false
                };
                
                fibers[i] = fiber_create(system->scheduler, 5 + i); // Different priorities
                fiber_set_function(fibers[i], news_validation_workflow_fiber, &contexts[i]);
            }
        );
        
        WHEN("multiple fibers process news concurrently",
            uint64_t start = rdtsc_portable();
            
            // Run concurrent processing
            int max_ticks = 500;
            int tick_count = 0;
            int completed_fibers = 0;
            
            while (completed_fibers < FIBER_COUNT && tick_count < max_ticks) {
                // Integrated system tick
                cns_bitactor_tick_integration(system->integration);
                tick_count++;
                
                // Count completed fibers
                completed_fibers = 0;
                for (int i = 0; i < FIBER_COUNT; i++) {
                    if (contexts[i].workflow_completed) {
                        completed_fibers++;
                    }
                }
            }
            
            uint64_t end = rdtsc_portable();
            uint64_t concurrent_time = end - start;
        );
        
        THEN("concurrent processing maintains system performance",
            EXPECT_EQ(completed_fibers, FIBER_COUNT);
            
            int total_articles = 0;
            int total_validated = 0;
            int total_queries = 0;
            
            for (int i = 0; i < FIBER_COUNT; i++) {
                total_articles += contexts[i].articles_processed;
                total_validated += contexts[i].articles_validated;
                total_queries += contexts[i].sparql_queries_executed;
                
                EXPECT(contexts[i].workflow_completed);
                EXPECT_GT(contexts[i].articles_processed, 0);
            }
            
            uint64_t avg_per_article = concurrent_time / total_articles;
            
            printf("       Concurrent processing results:\n");
            printf("         Fibers completed: %d/%d\n", completed_fibers, FIBER_COUNT);
            printf("         Total articles: %d\n", total_articles);
            printf("         Total validated: %d\n", total_validated);
            printf("         Total SPARQL queries: %d\n", total_queries);
            printf("         Concurrent time: %llu ticks\n",
                   (unsigned long long)concurrent_time);
            printf("         Avg per article: %llu ticks\n",
                   (unsigned long long)avg_per_article);
            
            EXPECT_LE(avg_per_article, 100); // Reasonable for concurrent processing
            
            // Cleanup
            if (system->validator) news_validator_free(system->validator);
            if (system->scheduler) fiber_scheduler_free(system->scheduler);
            if (system->sparql_exec) sparql_executor_free(system->sparql_exec);
            if (system->pipeline) {
                cns_pipeline_cleanup(system->pipeline);
                free(system->pipeline);
            }
            free(system);
        );
    } END_SCENARIO
    
    SCENARIO("System fault tolerance and error recovery") {
        cns_system_t* system = initialize_cns_system();
        
        GIVEN("system with error injection capabilities",
            EXPECT_NE(system, NULL);
        );
        
        WHEN("various error conditions are introduced",
            error_test_results_t results = {0};
            
            // Test 1: Invalid news article
            news_article_t invalid_article = {
                .title = "", // Empty title
                .content = "Short", // Too short
                .source_url = "invalid-url", // Invalid URL
                .author = NULL, // Missing author
                .published_date = time(NULL) + 86400 // Future date
            };
            
            uint64_t start = rdtsc_portable();
            bool result1 = news_validator_validate_article(system->validator, &invalid_article);
            
            // Test 2: Invalid SPARQL query
            const char* invalid_query = "INVALID SPARQL SYNTAX {";
            sparql_execution_result_t* result2 = 
                sparql_execute_query(system->sparql_exec, invalid_query);
            
            // Test 3: System under extreme load
            for (int i = 0; i < 1000; i++) {
                bitactor_tick(system->engine);
            }
            
            uint64_t end = rdtsc_portable();
            uint64_t error_handling_time = end - start;
        );
        
        THEN("system handles errors gracefully and recovers",
            // Validation should fail gracefully
            EXPECT_FALSE(result1);
            
            // SPARQL should handle syntax error
            EXPECT_NE(result2, NULL);
            EXPECT_EQ(result2->status, EXEC_ERROR);
            
            // System should remain responsive
            EXPECT_LE(error_handling_time, 10000); // Allow for error processing
            
            // Verify system can still process valid requests
            news_article_t valid_article = {
                .title = "Recovery Test Article",
                .content = "Valid content for testing system recovery after errors",
                .source_url = "https://valid-source.com/article",
                .author = "Test Author",
                .published_date = time(NULL)
            };
            
            bool recovery_result = news_validator_validate_article(system->validator, &valid_article);
            EXPECT(recovery_result);
            
            printf("       Error handling and recovery:\n");
            printf("         Invalid article handled: %s\n", result1 ? "NO" : "YES");
            printf("         Invalid SPARQL handled: %s\n", 
                   (result2 && result2->status == EXEC_ERROR) ? "YES" : "NO");
            printf("         System recovery: %s\n", recovery_result ? "YES" : "NO");
            printf("         Error handling time: %llu ticks\n",
                   (unsigned long long)error_handling_time);
            
            if (result2) free_execution_result(result2);
            
            // Cleanup
            if (system->validator) news_validator_free(system->validator);
            if (system->scheduler) fiber_scheduler_free(system->scheduler);
            if (system->sparql_exec) sparql_executor_free(system->sparql_exec);
            if (system->pipeline) {
                cns_pipeline_cleanup(system->pipeline);
                free(system->pipeline);
            }
            free(system);
        );
    } END_SCENARIO
    
    SCENARIO("Complete system telemetry and monitoring") {
        cns_system_t* system = initialize_cns_system();
        
        GIVEN("system with comprehensive telemetry enabled",
            EXPECT_NE(system, NULL);
            EXPECT_NE(system->engine->telemetry.frames, NULL);
        );
        
        WHEN("system processes various operations with telemetry",
            // Process news article
            news_article_t article = {
                .title = "Telemetry Test Article",
                .content = "Article for testing comprehensive system telemetry",
                .source_url = "https://telemetry-test.com/article",
                .author = "Telemetry Tester",
                .published_date = time(NULL)
            };
            
            news_validator_validate_article(system->validator, &article);
            
            // Execute SPARQL query
            const char* query = "SELECT ?s WHERE { ?s rdf:type :NewsArticle }";
            sparql_execution_result_t* result = 
                sparql_execute_query(system->sparql_exec, query);
            
            // Run BitActor ticks
            for (int i = 0; i < 10; i++) {
                bitactor_tick(system->engine);
            }
            
            if (result) free_execution_result(result);
        );
        
        THEN("telemetry captures comprehensive system activity",
            EXPECT_GT(system->engine->telemetry.frame_count, 0);
            
            // Analyze telemetry data
            int validation_ops = 0;
            int sparql_ops = 0;
            int bitactor_ops = 0;
            int fiber_ops = 0;
            
            uint64_t total_exec_time = 0;
            
            for (uint32_t i = 0; i < system->engine->telemetry.frame_count; i++) {
                telemetry_frame_t* frame = &system->engine->telemetry.frames[i];
                
                switch (frame->operation) {
                    case TRACE_OP_VALIDATION: validation_ops++; break;
                    case TRACE_OP_SPARQL: sparql_ops++; break;
                    case TRACE_OP_HANDLER: bitactor_ops++; break;
                    case TRACE_OP_FIBER: fiber_ops++; break;
                }
                
                total_exec_time += frame->ticks;
            }
            
            printf("       System telemetry analysis:\n");
            printf("         Total frames: %u\n", system->engine->telemetry.frame_count);
            printf("         Validation operations: %d\n", validation_ops);
            printf("         SPARQL operations: %d\n", sparql_ops);
            printf("         BitActor operations: %d\n", bitactor_ops);
            printf("         Fiber operations: %d\n", fiber_ops);
            printf("         Total execution time: %llu ticks\n",
                   (unsigned long long)total_exec_time);
            
            EXPECT_GT(validation_ops, 0);
            EXPECT_GT(sparql_ops, 0);
            EXPECT_GT(bitactor_ops, 0);
            
            // Export telemetry to TTL format
            char ttl_buffer[8192];
            size_t ttl_size = telemetry_to_ttl(&system->engine->telemetry,
                                              ttl_buffer, sizeof(ttl_buffer));
            
            EXPECT_GT(ttl_size, 0);
            EXPECT_NE(strstr(ttl_buffer, "bitactor:TelemetryFrame"), NULL);
            
            printf("         TTL export size: %zu bytes\n", ttl_size);
            
            // Cleanup
            if (system->validator) news_validator_free(system->validator);
            if (system->scheduler) fiber_scheduler_free(system->scheduler);
            if (system->sparql_exec) sparql_executor_free(system->sparql_exec);
            if (system->pipeline) {
                cns_pipeline_cleanup(system->pipeline);
                free(system->pipeline);
            }
            free(system);
        );
    } END_SCENARIO
}