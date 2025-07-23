/*
 * Mock implementations for testing existing CNS code
 * Provides minimal implementations to make tests compile and run
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>

/* Mock BitActor functions for CNS pipeline testing */
typedef struct {
    uint64_t capability;
    uint64_t hash;
} fast_proof_t;

bool bitactor_verify_fast(fast_proof_t* proof) {
    // Simple mock verification - just check if capability is non-zero
    return proof && proof->capability != 0;
}

/* Mock news validation function */
#ifdef MOCK_NEWS_VALIDATOR

// Claim type flags
#define CLAIM_STATISTICAL  0x01
#define CLAIM_QUOTE       0x02  
#define CLAIM_EVENT       0x04
#define CLAIM_SCIENTIFIC  0x08
#define CLAIM_PREDICTION  0x10
#define CLAIM_OPINION     0x20

// Verification status flags
#define STATUS_VERIFIED     0x01
#define STATUS_DISPUTED     0x02
#define STATUS_UNVERIFIED   0x08

typedef struct {
    uint64_t source_id;
    uint32_t credibility;
    uint32_t accuracy_rate;
    uint64_t last_verified;
    uint64_t total_articles;
    uint64_t false_articles;
    uint64_t corrections;
    uint64_t flags;
} source_info_t;

typedef struct {
    uint64_t claim_hash;
    uint64_t subject_hash;
    uint64_t source_id;
    uint32_t claim_type;
    uint32_t confidence;
    uint64_t timestamp;
    uint64_t evidence_mask;
    uint64_t related_claims;
} claim_t;

uint32_t validate_claim_8tick(claim_t* claim, source_info_t* source) {
    if (!claim || !source) {
        return STATUS_UNVERIFIED;
    }
    
    uint32_t result = 0;
    
    // Simple validation logic based on source credibility and claim confidence
    if (source->credibility >= 80 && claim->confidence >= 70) {
        result |= STATUS_VERIFIED;
    } else if (source->credibility < 50 || claim->confidence < 60) {
        result |= STATUS_DISPUTED;
    } else {
        result |= STATUS_UNVERIFIED;
    }
    
    // Consider evidence mask - more evidence = more likely to be verified
    if (claim->evidence_mask == 0 && (result & STATUS_VERIFIED)) {
        result &= ~STATUS_VERIFIED;
        result |= STATUS_UNVERIFIED;
    }
    
    return result;
}

#endif /* MOCK_NEWS_VALIDATOR */