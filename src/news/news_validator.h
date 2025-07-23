#ifndef NEWS_VALIDATOR_H
#define NEWS_VALIDATOR_H

#include <stdint.h>
#include <stdbool.h>

// Claim structure for news validation
typedef struct __attribute__((packed, aligned(64))) {
    uint64_t claim_hash;       // Fast lookup
    uint64_t subject_hash;     // What claim is about
    uint64_t source_id;        // Who made claim
    uint32_t claim_type;       // Bit flags
    uint32_t confidence;       // 0-100
    uint64_t timestamp;        
    uint64_t evidence_mask;    // Bit mask of evidence types
    uint64_t related_claims;   // Pointer to related
} claim_t;

// Public API for news validation

// Validate a news article in 8 CPU cycles
uint32_t validate_news_article(const claim_t* claims, uint32_t claim_count);

// Check source credibility in 1 cycle
uint32_t check_source_credibility(uint64_t source_id);

// Initialize fact database
void init_fact_database(const char* db_path);

// Process real-time fact updates
void process_fact_stream(const claim_t* new_facts, uint32_t count);

// Claim type flags
#define CLAIM_STATISTICAL  0x01
#define CLAIM_QUOTE       0x02  
#define CLAIM_EVENT       0x04
#define CLAIM_SCIENTIFIC  0x08
#define CLAIM_PREDICTION  0x10
#define CLAIM_OPINION     0x20

#endif