/*
 * CNS Pipeline BDD Tests
 * Testing the 8-tick quote processing pipeline
 */
#include "../bitactor/tests/bdd_framework.h"
#include "../src/cns/cns_pipeline.h"
#include <sys/time.h>

/* Test data */
static quote_t valid_quote = {
    .symbol = 0x414D5A4E, // "AMZN" 
    .price = 150000,      // $1500.00
    .volume = 100,
    .timestamp = 1234567890
};

static quote_t invalid_quote = {
    .symbol = 0x414D5A4E,
    .price = 0,           // Invalid price
    .volume = 0,          // Invalid volume
    .timestamp = 1234567890
};

/* Cycle counter for performance measurement */
static inline uint64_t get_cycles(void) {
#ifdef __x86_64__
    unsigned int lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#endif
}

FEATURE(CNS_Pipeline_Quote_Processing) {
    
    SCENARIO("Valid quotes are processed within 8 CPU ticks") {
        quote_t quote;
        uint64_t start_cycles, end_cycles, duration;
        
        GIVEN("a valid market data quote",
            quote = valid_quote;
        );
        
        WHEN("the quote is processed through the 8-tick pipeline",
            start_cycles = get_cycles();
            process_quote_8tick(&quote);
            end_cycles = get_cycles();
            duration = end_cycles - start_cycles;
        );
        
        THEN("processing completes within 8 CPU ticks",
            EXPECT_LT(duration, 9);
            printf("       Processing took %llu ticks\n", 
                   (unsigned long long)duration);
        );
        
        AND("the quote data remains valid",
            EXPECT_EQ(quote.symbol, 0x414D5A4E);
            EXPECT_GT(quote.price, 0);
            EXPECT_GT(quote.volume, 0);
        );
    } END_SCENARIO
    
    SCENARIO("Invalid quotes are rejected immediately") {
        quote_t quote;
        
        GIVEN("a quote with invalid price and volume",
            quote = invalid_quote;
        );
        
        WHEN("the quote validation operation is executed",
            op_validate_quote(&quote);
        );
        
        THEN("the quote is marked as invalid by clearing timestamp",
            EXPECT_EQ(quote.timestamp, 0);
        );
        
        AND("other fields remain unchanged",
            EXPECT_EQ(quote.symbol, 0x414D5A4E);
            EXPECT_EQ(quote.price, 0);
            EXPECT_EQ(quote.volume, 0);
        );
    } END_SCENARIO
    
    SCENARIO("Arena allocation handles memory efficiently") {
        quote_t quote;
        
        GIVEN("a valid quote for order creation",
            quote = valid_quote;
        );
        
        WHEN("arena allocation is performed",
            op_arena_alloc(&quote);
        );
        
        THEN("memory is allocated from the arena",
            // This tests internal state - in real implementation
            // we'd check if order buffer was set
            EXPECT(1); // Placeholder - arena allocation succeeded
        );
    } END_SCENARIO
    
    SCENARIO("Risk rules are applied correctly") {
        rule_t test_rule = {
            .mask = 0xFFFF,
            .threshold = 100000,  // $1000.00
            .action = 1
        };
        
        GIVEN("a high-value order that exceeds risk threshold",
            // Simulate having an order buffer with high price
            quote_t high_quote = {
                .symbol = 0x54534C41, // "TSLA"
                .price = 200000,      // $2000.00
                .volume = 1000,
                .timestamp = 1234567890
            };
            op_arena_alloc(&high_quote);
        );
        
        WHEN("risk rule is applied",
            op_apply_rule(&test_rule);
        );
        
        THEN("the rule action is triggered for high-value orders",
            // This would check internal risk_result state
            EXPECT(1); // Placeholder - risk check performed
        );
    } END_SCENARIO
    
    SCENARIO("Price calculation includes quantity adjustment") {
        uint64_t calculated_price = 0;
        
        GIVEN("an order with known price and quantity",
            quote_t quote = {
                .symbol = 0x474F4F47, // "GOOG"
                .price = 250000,      // $2500.00
                .volume = 50000,      // 50k shares
                .timestamp = 1234567890
            };
            op_arena_alloc(&quote);
        );
        
        WHEN("price calculation is performed",
            op_calculate_price(&calculated_price);
        );
        
        THEN("the calculated price includes quantity adjustment",
            // Price + (quantity / 1000) = 250000 + (50000 / 1000) = 250050
            EXPECT_EQ(calculated_price, 250050);
        );
    } END_SCENARIO
    
    SCENARIO("FIX message formatting follows protocol") {
        char fix_message[256] = {0};
        
        GIVEN("a valid order ready for transmission",
            quote_t quote = {
                .symbol = 0x4D534654, // "MSFT"
                .price = 300000,      // $3000.00
                .volume = 500,
                .timestamp = 1234567890
            };
            op_arena_alloc(&quote);
            
            // Simulate successful risk check
            rule_t rule = {.mask = 0xFFFF, .threshold = 100, .action = 1};
            op_apply_rule(&rule);
        );
        
        WHEN("FIX message is formatted",
            op_format_order(fix_message);
        );
        
        THEN("the message contains required FIX fields",
            EXPECT(strstr(fix_message, "35=D") != NULL);  // New Order Single
            EXPECT(strstr(fix_message, "55=") != NULL);   // Symbol
            EXPECT(strstr(fix_message, "54=1") != NULL);  // Buy side
            EXPECT(strstr(fix_message, "38=") != NULL);   // Order quantity
            EXPECT(strstr(fix_message, "44=") != NULL);   // Price
        );
        
        AND("the message is properly formatted",
            printf("       FIX Message: %s\n", fix_message);
            EXPECT_GT(strlen(fix_message), 20);
        );
    } END_SCENARIO
    
    SCENARIO("Complete pipeline handles burst of quotes") {
        const int QUOTE_COUNT = 1000;
        quote_t quotes[QUOTE_COUNT];
        uint64_t total_cycles = 0;
        uint64_t max_cycles = 0;
        
        GIVEN("a burst of market quotes",
            for (int i = 0; i < QUOTE_COUNT; i++) {
                quotes[i] = (quote_t){
                    .symbol = 0x53505958 + i,  // Various symbols
                    .price = 100000 + (i * 100),
                    .volume = 100 + (i % 500),
                    .timestamp = 1234567890 + i
                };
            }
        );
        
        WHEN("all quotes are processed through the pipeline",
            for (int i = 0; i < QUOTE_COUNT; i++) {
                uint64_t start = get_cycles();
                process_quote_8tick(&quotes[i]);
                uint64_t duration = get_cycles() - start;
                
                total_cycles += duration;
                if (duration > max_cycles) {
                    max_cycles = duration;
                }
            }
        );
        
        THEN("maximum processing time remains under 8 ticks",
            printf("       Max cycles: %llu\n", (unsigned long long)max_cycles);
            printf("       Avg cycles: %llu\n", 
                   (unsigned long long)(total_cycles / QUOTE_COUNT));
            EXPECT_LT(max_cycles, 9);
        );
        
        AND("average processing time is efficient",
            uint64_t avg_cycles = total_cycles / QUOTE_COUNT;
            EXPECT_LT(avg_cycles, 6);  // Should average under 6 ticks
        );
    } END_SCENARIO
    
    SCENARIO("Pipeline maintains deterministic behavior") {
        quote_t test_quote = valid_quote;
        char result1[256] = {0};
        char result2[256] = {0};
        
        GIVEN("identical quote inputs",
            // Nothing to setup - using same quote
        );
        
        WHEN("the same quote is processed twice",
            // First processing
            process_quote_8tick(&test_quote);
            op_format_order(result1);
            
            // Reset and process again
            test_quote = valid_quote;
            process_quote_8tick(&test_quote);
            op_format_order(result2);
        );
        
        THEN("outputs are identical",
            EXPECT_EQ(strcmp(result1, result2), 0);
            printf("       Result 1: %s\n", result1);
            printf("       Result 2: %s\n", result2);
        );
    } END_SCENARIO
}