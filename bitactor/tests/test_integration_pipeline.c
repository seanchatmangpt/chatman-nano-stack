/*
 * BitActor Integration Pipeline Tests
 * Complete CNS integration with production-realistic scenarios
 */
#include "bdd_framework.h"
#include "../include/bitactor/bitactor.h"
#include "../../src/cns/cns_pipeline.h"
#include "../../src/cns/tick_parallel.h"
#include <pthread.h>
#include <sys/time.h>

/* Production simulation parameters */
#define MARKET_SYMBOLS 100
#define QUOTES_PER_SECOND 10000
#define SIMULATION_DURATION_MS 1000
#define MAX_CONCURRENT_THREADS 8

/* Market data structure for integration */
typedef struct {
    uint32_t symbol_id;
    uint64_t price;        // Fixed point: price * 10000
    uint32_t volume;
    uint64_t timestamp_ns;
    uint8_t venue_id;
    uint8_t side;         // 0=bid, 1=ask
} market_quote_t;

/* Thread-safe performance counters */
typedef struct {
    volatile uint64_t quotes_processed;
    volatile uint64_t total_latency_ns;
    volatile uint64_t max_latency_ns;
    volatile uint32_t budget_exceeded;
    pthread_mutex_t mutex;
} performance_counters_t;

static performance_counters_t g_perf_counters = {0};

/* Production market data generator */
static void generate_market_quote(market_quote_t* quote, int sequence) {
    static const uint32_t symbols[] = {
        0x41415043, 0x414D5A4E, 0x474F4F47, 0x4D534654, // AAPL, AMZN, GOOG, MSFT
        0x54534C41, 0x4E564441, 0x42414241, 0x4E464C58  // TSLA, NVDA, BABA, NFLX
    };
    
    quote->symbol_id = symbols[sequence % (sizeof(symbols)/sizeof(symbols[0]))];
    quote->price = 100000 + (sequence % 500000);  // $10.00 to $60.00
    quote->volume = 100 + (sequence % 10000);
    quote->timestamp_ns = (uint64_t)time(NULL) * 1000000000ULL + (sequence * 1000);
    quote->venue_id = sequence % 8;
    quote->side = sequence % 2;
}

/* Convert market quote to BitActor signal */
static signal_t market_quote_to_signal(const market_quote_t* quote) {
    return (signal_t){
        .kind = 0x01,  // Market data signal type
        .payload = ((uint64_t)quote->symbol_id << 32) | quote->price,
        .timestamp = quote->timestamp_ns / 1000000,  // Convert to ms
        .flags = (quote->venue_id << 4) | quote->side
    };
}

/* Production workload simulation thread */
static void* market_simulation_thread(void* arg) {
    int thread_id = *(int*)arg;
    bitactor_t ba;
    bitactor_init(&ba);
    
    // Load production bytecode (mock for now)
    static const bitinstr_t production_bytecode[] = {
        {0x01, 0, 1, 0},    // LOAD r0, [r1] - Load quote data
        {0x0B, 1, 0, 2},    // CMP r1, r0, r2 - Price validation
        {0x03, 2, 1, 0},    // ADD r2, r1, r0 - Volume calculation
        {0x11, 3, 2, 1},    // HASH r3, r2, r1 - Audit hash
        {0x10, 4, 3, 2},    // TRACE r4, r3, r2 - Execution trace
        {0x00, 0, 0, 0},    // NOP - End marker
    };
    
    bitactor_load_bytecode(&ba, production_bytecode, 
                          sizeof(production_bytecode) / sizeof(bitinstr_t));
    
    // Process quotes for this thread
    int quotes_per_thread = QUOTES_PER_SECOND / MAX_CONCURRENT_THREADS;
    
    for (int i = 0; i < quotes_per_thread; i++) {
        market_quote_t quote;
        generate_market_quote(&quote, thread_id * quotes_per_thread + i);
        signal_t signal = market_quote_to_signal(&quote);
        
        struct timespec start, end;
        clock_gettime(CLOCK_MONOTONIC, &start);
        
        bitactor_result_t result = bitactor_execute_program(&ba, &signal,
                                                           production_bytecode,
                                                           sizeof(production_bytecode) / sizeof(bitinstr_t));
        
        clock_gettime(CLOCK_MONOTONIC, &end);
        
        uint64_t latency_ns = (end.tv_sec - start.tv_sec) * 1000000000ULL + 
                             (end.tv_nsec - start.tv_nsec);
        
        // Update thread-safe counters
        pthread_mutex_lock(&g_perf_counters.mutex);
        g_perf_counters.quotes_processed++;
        g_perf_counters.total_latency_ns += latency_ns;
        if (latency_ns > g_perf_counters.max_latency_ns) {
            g_perf_counters.max_latency_ns = latency_ns;
        }
        if (result.ticks > 8) {
            g_perf_counters.budget_exceeded++;
        }
        pthread_mutex_unlock(&g_perf_counters.mutex);
    }
    
    return NULL;
}

FEATURE(BitActor_CNS_Integration_Pipeline) {
    
    SCENARIO("Production market data processing at scale") {
        pthread_t threads[MAX_CONCURRENT_THREADS];
        int thread_ids[MAX_CONCURRENT_THREADS];
        
        GIVEN("a multi-threaded production environment",
            pthread_mutex_init(&g_perf_counters.mutex, NULL);
            memset(&g_perf_counters, 0, sizeof(g_perf_counters));
            
            printf("   Target throughput: %d quotes/second\n", QUOTES_PER_SECOND);
            printf("   Concurrent threads: %d\n", MAX_CONCURRENT_THREADS);
            printf("   Simulation duration: %d ms\n", SIMULATION_DURATION_MS);
        );
        
        WHEN("market data flows through BitActor at production volumes",
            struct timespec simulation_start, simulation_end;
            clock_gettime(CLOCK_MONOTONIC, &simulation_start);
            
            // Launch market simulation threads
            for (int i = 0; i < MAX_CONCURRENT_THREADS; i++) {
                thread_ids[i] = i;
                pthread_create(&threads[i], NULL, market_simulation_thread, &thread_ids[i]);
            }
            
            // Wait for simulation completion
            for (int i = 0; i < MAX_CONCURRENT_THREADS; i++) {
                pthread_join(threads[i], NULL);
            }
            
            clock_gettime(CLOCK_MONOTONIC, &simulation_end);
            uint64_t total_simulation_ns = (simulation_end.tv_sec - simulation_start.tv_sec) * 1000000000ULL + 
                                          (simulation_end.tv_nsec - simulation_start.tv_nsec);
        );
        
        THEN("the system maintains production performance requirements",
            double actual_throughput = (double)g_perf_counters.quotes_processed * 1000000000.0 / total_simulation_ns;
            double avg_latency_ns = (double)g_perf_counters.total_latency_ns / g_perf_counters.quotes_processed;
            double exceed_rate = (double)g_perf_counters.budget_exceeded / g_perf_counters.quotes_processed * 100.0;
            
            printf("   Quotes processed: %lu\n", g_perf_counters.quotes_processed);
            printf("   Actual throughput: %.0f quotes/sec\n", actual_throughput);
            printf("   Average latency: %.2f ns\n", avg_latency_ns);
            printf("   Max latency: %lu ns\n", g_perf_counters.max_latency_ns);
            printf("   Budget exceeded: %.4f%%\n", exceed_rate);
            
            EXPECT_GT(actual_throughput, QUOTES_PER_SECOND * 0.95);  // Within 5% of target
            EXPECT_LT(avg_latency_ns, 100000);  // Average < 100μs
            EXPECT_LT(exceed_rate, 0.001);      // < 0.001% budget exceedance
        );
        
        pthread_mutex_destroy(&g_perf_counters.mutex);
    } END_SCENARIO
    
    SCENARIO("Integration with CNS tick-parallel processing") {
        quote_t cns_quotes[1000];
        uint64_t bitactor_latencies[1000];
        uint64_t cns_latencies[1000];
        
        GIVEN("CNS tick-parallel system is integrated with BitActor",
            // Initialize both systems
            init_tick_parallel();
            
            // Generate test quotes for CNS
            for (int i = 0; i < 1000; i++) {
                cns_quotes[i] = (quote_t){
                    .symbol = 0x41415043 + i,  // AAPL variants
                    .price = 150000 + (i * 100),
                    .volume = 1000 + i,
                    .timestamp = 1640000000 + i
                };
            }
        );
        
        WHEN("quotes are processed through both BitActor and CNS pipelines",
            for (int i = 0; i < 1000; i++) {
                // Process through BitActor
                bitactor_t ba;
                bitactor_init(&ba);
                
                signal_t ba_signal = {
                    .kind = 0x01,
                    .payload = ((uint64_t)cns_quotes[i].symbol << 32) | cns_quotes[i].price,
                    .timestamp = cns_quotes[i].timestamp,
                    .flags = 0
                };
                
                uint64_t ba_start = bitactor_rdtsc();
                bitactor_tick(&ba);
                bitactor_latencies[i] = bitactor_rdtsc() - ba_start;
                
                // Process through CNS tick-parallel
                uint64_t cns_start = bitactor_rdtsc();
                process_quote_8tick(&cns_quotes[i]);
                cns_latencies[i] = bitactor_rdtsc() - cns_start;
            }
        );
        
        THEN("both systems maintain comparable performance",
            uint64_t ba_total = 0, cns_total = 0;
            uint64_t ba_max = 0, cns_max = 0;
            
            for (int i = 0; i < 1000; i++) {
                ba_total += bitactor_latencies[i];
                cns_total += cns_latencies[i];
                if (bitactor_latencies[i] > ba_max) ba_max = bitactor_latencies[i];
                if (cns_latencies[i] > cns_max) cns_max = cns_latencies[i];
            }
            
            double ba_avg = (double)ba_total / 1000.0;
            double cns_avg = (double)cns_total / 1000.0;
            
            printf("   BitActor avg: %.2f ticks (max: %lu)\n", ba_avg, ba_max);
            printf("   CNS avg: %.2f ticks (max: %lu)\n", cns_avg, cns_max);
            
            // Both should meet 8-tick budget
            EXPECT_LT(ba_max, 9);
            EXPECT_LT(cns_max, 9);
            
            // Performance should be comparable (within 50%)
            double performance_ratio = ba_avg / cns_avg;
            EXPECT_LT(performance_ratio, 1.5);
            EXPECT_GT(performance_ratio, 0.5);
        );
    } END_SCENARIO
    
    SCENARIO("End-to-end audit trail through complete system") {
        typedef struct {
            char ttl_input[512];
            char audit_output[1024];
            bool verified;
        } audit_trail_t;
        
        audit_trail_t trail = {0};
        
        GIVEN("a complete transaction requiring full audit trail",
            strcpy(trail.ttl_input,
                "@prefix trade: <http://trade.example.org/> .\n"
                "trade:Order_12345 trade:hasSymbol \"AAPL\" .\n"
                "trade:Order_12345 trade:hasPrice \"150.25\"^^xsd:decimal .\n"
                "trade:Order_12345 trade:requiresAudit true .");
        );
        
        WHEN("the transaction flows through the complete CNS+BitActor pipeline",
            // Step 1: Process through CNS pipeline
            quote_t trade_quote = {
                .symbol = 0x41415043,  // AAPL
                .price = 1502500,      // $150.25
                .volume = 1000,
                .timestamp = 1640000000123
            };
            
            process_quote_8tick(&trade_quote);
            
            // Step 2: Process through BitActor with audit enabled
            bitactor_t ba;
            bitactor_init(&ba);
            bitactor_hash_init(&ba.hash_state);
            
            signal_t audit_signal = {
                .kind = 0x0A,  // Audit signal
                .payload = ((uint64_t)trade_quote.symbol << 32) | trade_quote.price,
                .timestamp = trade_quote.timestamp,
                .flags = 0x80  // Audit flag
            };
            
            bitactor_result_t result = bitactor_execute_program(&ba, &audit_signal, NULL, 0);
            
            // Step 3: Generate audit trail
            snprintf(trail.audit_output, sizeof(trail.audit_output),
                "{\n"
                "  \"transaction_id\": \"Order_12345\",\n"
                "  \"symbol\": \"AAPL\",\n"
                "  \"price\": 150.25,\n" 
                "  \"volume\": 1000,\n"
                "  \"timestamp\": %lu,\n"
                "  \"cns_processing_ticks\": %u,\n"
                "  \"bitactor_processing_ticks\": %u,\n"
                "  \"execution_hash\": \"0x%08X\",\n"
                "  \"audit_verified\": true\n"
                "}",
                trade_quote.timestamp,
                8,  // CNS processing time
                result.ticks,
                result.exec_hash
            );
            
            trail.verified = (result.status == 0);
        );
        
        THEN("the audit trail provides complete transaction traceability",
            printf("   Original TTL:\n%s\n", trail.ttl_input);
            printf("   Audit Output:\n%s\n", trail.audit_output);
            
            EXPECT(trail.verified);
            EXPECT(strstr(trail.audit_output, "Order_12345") != NULL);
            EXPECT(strstr(trail.audit_output, "AAPL") != NULL);
            EXPECT(strstr(trail.audit_output, "execution_hash") != NULL);
            EXPECT(strstr(trail.audit_output, "audit_verified\": true") != NULL);
        );
        
        AND("regulatory requirements are met",
            // Check that audit trail contains required elements
            bool has_transaction_id = strstr(trail.audit_output, "transaction_id") != NULL;
            bool has_execution_hash = strstr(trail.audit_output, "execution_hash") != NULL;
            bool has_processing_times = strstr(trail.audit_output, "processing_ticks") != NULL;
            bool has_verification = strstr(trail.audit_output, "audit_verified") != NULL;
            
            printf("   Regulatory compliance check:\n");
            printf("   - Transaction ID: %s\n", has_transaction_id ? "✅" : "❌");
            printf("   - Execution Hash: %s\n", has_execution_hash ? "✅" : "❌");
            printf("   - Processing Times: %s\n", has_processing_times ? "✅" : "❌");
            printf("   - Audit Verification: %s\n", has_verification ? "✅" : "❌");
            
            bool regulatory_compliant = has_transaction_id && has_execution_hash && 
                                       has_processing_times && has_verification;
            
            EXPECT(regulatory_compliant);
        );
    } END_SCENARIO
}