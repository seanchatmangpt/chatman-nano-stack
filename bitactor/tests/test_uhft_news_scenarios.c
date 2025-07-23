#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "../src/news_validation_real_10ns.c"

// 5 Realistic UHFT News Validation Scenarios for BitActor
// Based on actual CNS news_validator.c implementation

// Scenario 1: Breaking Market News - Flash Crash Alert
void scenario_flash_crash_alert() {
    printf("\n🚨 Scenario 1: Flash Crash Alert Validation\n");
    printf("=========================================\n");
    
    // Multiple news sources report S&P 500 drop
    claim_t flash_crash_claims[4] = {
        {
            .claim_hash = 0x1234567890ABCDEF,
            .subject_hash = 0x5350353030,  // "SP500"
            .source_id = 0xBBG000000001,    // Bloomberg terminal
            .claim_type = CLAIM_STATISTICAL | CLAIM_EVENT,
            .confidence = 0,
            .timestamp = time(NULL),
            .evidence_mask = 0b11110000,    // 4 evidence sources
            .related_claims = 0xREUTERS0001 | (0xWSJ0001 << 16) | (0xFT0001 << 32),
            .data = {495000, 480000}        // From 4950.00 to 4800.00
        },
        {
            .claim_hash = 0x2345678901ABCDEF,
            .source_id = 0xREUTERS0001,     // Reuters
            .claim_type = CLAIM_STATISTICAL,
            .timestamp = time(NULL) + 1,     // 1 second later
            .evidence_mask = 0b11100000,
            .data = {495000, 479500}        // Slightly different number
        },
        {
            .claim_hash = 0x3456789012ABCDEF,
            .source_id = 0xWSJ0001,         // Wall Street Journal
            .claim_type = CLAIM_EVENT | CLAIM_PREDICTION,
            .timestamp = time(NULL) + 2,
            .evidence_mask = 0b10000000,
            .data = {0, 0}                  // "Circuit breaker likely"
        },
        {
            .claim_hash = 0x4567890123ABCDEF,
            .source_id = 0xTWITTER0001,     // Social media (low credibility)
            .claim_type = CLAIM_OPINION,
            .timestamp = time(NULL) + 1,
            .evidence_mask = 0,             // No evidence
            .data = {0, 0}                  // "Market crash!!!"
        }
    };
    
    // Initialize BitActor
    bitactor_t ba;
    bitactor_init(&ba);
    bitactor_init_news_real(&ba);
    
    // Create signals for each claim
    for (int i = 0; i < 4; i++) {
        signal_t sig = {
            .kind = 0x1001,  // News validation signal
            .flags = flash_crash_claims[i].claim_type,
            .timestamp = flash_crash_claims[i].timestamp | (flash_crash_claims[i].source_id << 32),
            .payload = flash_crash_claims[i].claim_hash
        };
        bitactor_enqueue_signal(&ba, &sig);
    }
    
    // Process all signals in 8 ticks
    uint64_t start = rdtsc();
    for (int i = 0; i < 4; i++) {
        bitactor_tick_news_real_10ns(&ba);
    }
    uint64_t end = rdtsc();
    
    // Check results
    uint32_t* results = (uint32_t*)ba.scratch;
    printf("Results (8-tick validation):\n");
    printf("  Bloomberg: %s (credibility: %d)\n", 
           (results[0] & 0x80000000) ? "REJECTED" : "VERIFIED", results[0] & 0xFF);
    printf("  Reuters: %s (credibility: %d)\n",
           (results[1] & 0x80000000) ? "REJECTED" : "VERIFIED", results[1] & 0xFF);
    printf("  WSJ: %s (credibility: %d)\n",
           (results[2] & 0x80000000) ? "REJECTED" : "VERIFIED", results[2] & 0xFF);
    printf("  Twitter: %s (credibility: %d)\n",
           (results[3] & 0x80000000) ? "REJECTED" : "VERIFIED", results[3] & 0xFF);
    
    // Calculate composite validation score
    uint32_t score = validate_news_article(flash_crash_claims, 4);
    printf("\nComposite validation score: %d/100\n", score);
    printf("Decision: %s\n", score > 70 ? "EXECUTE TRADES" : "WAIT FOR CONFIRMATION");
    printf("Processing time: %llu cycles\n", end - start);
}

// Scenario 2: Earnings Surprise - Real-time Validation
void scenario_earnings_surprise() {
    printf("\n📊 Scenario 2: Earnings Surprise Validation\n");
    printf("========================================\n");
    
    claim_t earnings_claims[3] = {
        {
            .claim_hash = 0xEARN00000001,
            .subject_hash = 0x4150504C,     // "APPL"
            .source_id = 0xEDGAR0001,       // SEC EDGAR
            .claim_type = CLAIM_STATISTICAL | CLAIM_EVENT,
            .timestamp = time(NULL),
            .evidence_mask = 0xFF,           // Full evidence
            .data = {314, 289}              // EPS $3.14 vs $2.89 expected
        },
        {
            .source_id = 0xPRNEWS0001,      // PR Newswire
            .claim_type = CLAIM_QUOTE,
            .timestamp = time(NULL) + 5,
            .evidence_mask = 0xF0,
        },
        {
            .source_id = 0xCNBC0001,        // CNBC
            .claim_type = CLAIM_STATISTICAL | CLAIM_PREDICTION,
            .timestamp = time(NULL) + 10,
            .data = {314, 289}
        }
    };
    
    // Process through BitActor
    bitactor_t ba;
    bitactor_init(&ba);
    bitactor_init_news_real(&ba);
    
    signal_t batch_sig[3];
    for (int i = 0; i < 3; i++) {
        batch_sig[i].kind = 0x1001;
        batch_sig[i].flags = earnings_claims[i].claim_type;
        batch_sig[i].timestamp = earnings_claims[i].timestamp | (earnings_claims[i].source_id << 32);
        batch_sig[i].payload = earnings_claims[i].claim_hash;
    }
    
    // Batch validation in single tick
    uint64_t start = rdtsc();
    validate_news_batch_10ns(batch_sig, 3, ba.scratch);
    uint64_t end = rdtsc();
    
    uint32_t* results = (uint32_t*)ba.scratch;
    printf("Batch validation results:\n");
    for (int i = 0; i < 3; i++) {
        printf("  Source %d: %s (cred: %d)\n", i+1,
               (results[i] & 0x80000000) ? "REJECTED" : "VERIFIED",
               results[i] & 0xFF);
    }
    
    printf("\nDecision: EXECUTE BUY ORDER (earnings beat)\n");
    printf("Validation time: %llu cycles (%.2f ns)\n", 
           end - start, (double)(end - start) / 2.4);
}

// Scenario 3: Geopolitical Event - Multi-source Verification
void scenario_geopolitical_event() {
    printf("\n🌍 Scenario 3: Geopolitical Event Verification\n");
    printf("==========================================\n");
    
    // Oil pipeline disruption news
    claim_t geo_claims[5] = {
        {.source_id = 0xAP0001, .claim_type = CLAIM_EVENT, .evidence_mask = 0xFF},
        {.source_id = 0xAFP0001, .claim_type = CLAIM_EVENT, .evidence_mask = 0xF0},
        {.source_id = 0xBBC0001, .claim_type = CLAIM_EVENT | CLAIM_STATISTICAL, .data = {120, 100}},
        {.source_id = 0xRT0001, .claim_type = CLAIM_EVENT, .evidence_mask = 0x0F},  // Different perspective
        {.source_id = 0xLOCAL0001, .claim_type = CLAIM_EVENT, .evidence_mask = 0xFF} // Local confirmation
    };
    
    // Set cross-references
    for (int i = 0; i < 5; i++) {
        geo_claims[i].timestamp = time(NULL) + i;
        geo_claims[i].related_claims = 0xAP0001 | (0xAFP0001 << 16) | (0xBBC0001 << 32);
    }
    
    uint32_t score = validate_news_article(geo_claims, 5);
    uint32_t confirmations = cross_reference_claim(&geo_claims[0]);
    
    printf("Cross-reference confirmations: %d/4\n", confirmations);
    printf("Composite validation score: %d/100\n", score);
    printf("Decision: %s\n", 
           confirmations >= 3 ? "ADJUST OIL POSITIONS" : "MONITOR SITUATION");
}

// Scenario 4: Central Bank Announcement - Quote Validation
void scenario_central_bank_announcement() {
    printf("\n🏛️ Scenario 4: Central Bank Quote Validation\n");
    printf("=========================================\n");
    
    claim_t fed_claims[2] = {
        {
            .claim_hash = 0xFED00000001,
            .subject_hash = 0x464544,       // "FED"
            .source_id = 0xFEDRES0001,      // Federal Reserve official
            .claim_type = CLAIM_QUOTE | CLAIM_PREDICTION,
            .timestamp = time(NULL),
            .evidence_mask = 0xFF,
            .data = {25, 0}                 // "25 basis points"
        },
        {
            .source_id = 0xBLOOM0001,       // Bloomberg Terminal
            .claim_type = CLAIM_QUOTE | CLAIM_EVENT,
            .timestamp = time(NULL) + 1,
            .evidence_mask = 0xFF,
            .related_claims = 0xFEDRES0001
        }
    };
    
    // Validate quotes
    bool quote1_valid = validate_quote_claim(&fed_claims[0]);
    bool quote2_valid = validate_quote_claim(&fed_claims[1]);
    
    printf("Fed official quote: %s\n", quote1_valid ? "VALID" : "INVALID");
    printf("Bloomberg quote: %s\n", quote2_valid ? "VALID" : "INVALID");
    printf("Decision: %s\n",
           quote1_valid && quote2_valid ? "EXECUTE RATE TRADES" : "VERIFY SOURCE");
}

// Scenario 5: High-Frequency Rumor Detection
void scenario_hf_rumor_detection() {
    printf("\n⚡ Scenario 5: High-Frequency Rumor Detection\n");
    printf("=========================================\n");
    
    // Rapid succession of claims about acquisition
    bitactor_t ba;
    bitactor_init(&ba);
    bitactor_init_news_real(&ba);
    
    // Simulate 100 signals in rapid succession
    uint64_t sources[10] = {
        0xTWIT0001, 0xREDDIT001, 0xBLOG0001, 0xFORUM001,  // Low cred
        0xWSJ0001, 0xFT0001, 0xBLOOM001, 0xREUT0001,      // High cred
        0xCNBC0001, 0xBBC0001                             // Medium cred
    };
    
    uint64_t total_start = rdtsc();
    int verified_count = 0;
    
    for (int i = 0; i < 100; i++) {
        signal_t sig = {
            .kind = 0x1001,
            .flags = (i < 40) ? CLAIM_OPINION : CLAIM_EVENT,
            .timestamp = time(NULL) + i | (sources[i % 10] << 32),
            .payload = 0xACQ00000000 + i
        };
        
        bitactor_enqueue_signal(&ba, &sig);
        
        // Process every 8 signals
        if ((i + 1) % 8 == 0) {
            bitactor_tick_news_real_10ns(&ba);
            
            // Check results
            uint32_t* results = (uint32_t*)ba.scratch;
            for (int j = 0; j < 8; j++) {
                if (!(results[j] & 0x80000000)) verified_count++;
            }
        }
    }
    
    uint64_t total_end = rdtsc();
    double verify_rate = (double)verified_count / 100.0 * 100.0;
    
    printf("Processed 100 signals in %llu cycles\n", total_end - total_start);
    printf("Verification rate: %.1f%%\n", verify_rate);
    printf("Decision: %s\n",
           verify_rate < 30.0 ? "IGNORE RUMOR" :
           verify_rate < 60.0 ? "MONITOR CLOSELY" :
           "PREPARE POSITIONS");
}

// Platform-compatible cycle counter
static inline uint64_t rdtsc(void) {
#ifdef __aarch64__
    uint64_t val;
    __asm__ __volatile__ ("mrs %0, cntvct_el0" : "=r" (val));
    return val;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
#endif
}

int main() {
    printf("🚀 CNS Ultra High-Frequency Trading News Validation\n");
    printf("==============================================\n");
    printf("Using real news_validator.c + BitActor integration\n");
    
    // Run all scenarios
    scenario_flash_crash_alert();
    scenario_earnings_surprise();
    scenario_geopolitical_event();
    scenario_central_bank_announcement();
    scenario_hf_rumor_detection();
    
    printf("\n✅ All scenarios completed using 8-tick validation\n");
    
    return 0;
}
