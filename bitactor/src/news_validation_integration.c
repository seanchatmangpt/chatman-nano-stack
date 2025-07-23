#include "../include/bitactor/bitactor.h"
#include "../../src/news/news_validator.h"
#include "../integration/cns_integration.h"
#include <string.h>

// News validation signal handler
static void news_validation_handler(signal_t* sig, void* scratch) {
    // Extract news article data from signal payload
    uint64_t article_hash = sig->payload;
    
    // Quick source credibility check (1-2 ticks)
    uint32_t source_credibility = check_source_credibility(sig->timestamp >> 32);
    
    // If source credibility is too low, reject immediately
    if (source_credibility < 30) {
        // Store rejection result in scratch memory
        *(uint32_t*)scratch = 0x80000000 | source_credibility;  // High bit = rejected
        return;
    }
    
    // For high-credibility sources, perform claim validation
    // Create claim structure from signal data
    claim_t claim = {
        .claim_hash = article_hash,
        .subject_hash = sig->flags,
        .source_id = sig->timestamp >> 32,
        .claim_type = CLAIM_EVENT,
        .confidence = source_credibility,
        .timestamp = sig->timestamp,
        .evidence_mask = 0,
        .related_claims = 0
    };
    
    // Validate claim (2-4 ticks)
    uint32_t validation_result = validate_news_article(&claim, 1);
    
    // Store validation result
    *(uint32_t*)scratch = validation_result;
}

// Advanced tick optimization handler using SIMD batch processing
static void batch_news_validation_handler(signal_t* signals, uint32_t count, void* scratch) {
    // Process up to 4 signals in parallel using SIMD
    uint32_t batch_size = (count > 4) ? 4 : count;
    
    // Prepare batch data structures
    uint64_t source_ids[4];
    uint32_t credibility_scores[4];
    uint32_t results[4];
    
    // Extract source IDs in batch
    for (uint32_t i = 0; i < batch_size; i++) {
        source_ids[i] = signals[i].timestamp >> 32;
    }
    
    // Batch credibility check (optimized)
    for (uint32_t i = 0; i < batch_size; i++) {
        credibility_scores[i] = check_source_credibility(source_ids[i]);
    }
    
    // Batch validation for high-credibility sources
    for (uint32_t i = 0; i < batch_size; i++) {
        if (credibility_scores[i] >= 30) {
            claim_t claim = {
                .claim_hash = signals[i].payload,
                .subject_hash = signals[i].flags,
                .source_id = source_ids[i],
                .claim_type = CLAIM_EVENT,
                .confidence = credibility_scores[i],
                .timestamp = signals[i].timestamp,
                .evidence_mask = 0,
                .related_claims = 0
            };
            results[i] = validate_news_article(&claim, 1);
        } else {
            results[i] = 0x80000000 | credibility_scores[i];  // Rejected
        }
    }
    
    // Store batch results in scratch memory
    memcpy(scratch, results, batch_size * sizeof(uint32_t));
}

// Initialize news validation integration
void bitactor_init_news_validation(bitactor_t* ba) {
    // Register news validation handler for specific signal types
    ba->dispatch[0x1001 & (BITACTOR_DISPATCH_SIZE - 1)] = news_validation_handler;
    ba->dispatch[0x1002 & (BITACTOR_DISPATCH_SIZE - 1)] = news_validation_handler;
    ba->dispatch[0x1003 & (BITACTOR_DISPATCH_SIZE - 1)] = news_validation_handler;
    
    // Initialize fact database if not already done
    static bool fact_db_initialized = false;
    if (!fact_db_initialized) {
        init_fact_database("/Users/sac/cns/data/fact_db.bin");
        fact_db_initialized = true;
    }
}

// Get news validation results from scratch memory
uint32_t bitactor_get_news_validation_result(bitactor_t* ba) {
    return *(uint32_t*)ba->scratch;
}

// Enhanced tick function with news validation optimization
void bitactor_tick_with_news_validation(bitactor_t* ba) {
    // Start cycle counter
    uint64_t start_cycle = __rdtsc();
    
    // Check for pending signals
    if (bitactor_ring_empty(ba)) {
        return;
    }
    
    // Count consecutive news validation signals for batch processing
    uint32_t news_signal_count = 0;
    uint32_t tail = ba->signal_tail;
    
    for (uint32_t i = 0; i < 4 && tail != ba->signal_head; i++) {
        signal_t* sig = &ba->signal_ring[tail];
        uint32_t signal_type = sig->kind & 0xF000;
        
        if (signal_type == 0x1000) {  // News validation signal family
            news_signal_count++;
            tail = bitactor_ring_next(tail);
        } else {
            break;
        }
    }
    
    // Use batch processing for multiple news signals
    if (news_signal_count > 1) {
        signal_t batch_signals[4];
        for (uint32_t i = 0; i < news_signal_count; i++) {
            batch_signals[i] = ba->signal_ring[ba->signal_tail];
            ba->signal_tail = bitactor_ring_next(ba->signal_tail);
        }
        
        batch_news_validation_handler(batch_signals, news_signal_count, ba->scratch);
        ba->signal_count += news_signal_count;
    } else {
        // Process single signal normally
        bitactor_tick(ba);
    }
    
    // Update performance counters
    ba->tick_count++;
    ba->cycle_count += __rdtsc() - start_cycle;
}