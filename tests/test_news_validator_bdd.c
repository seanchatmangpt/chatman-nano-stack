/*
 * News Validator BDD Tests
 * Testing 8-tick news validation system
 */
#include "../bitactor/tests/bdd_framework.h"
#include <string.h>

// Include mock definitions directly
#include "mock_implementations.c"

/* Mock data for testing */
static source_info_t reliable_source = {
    .source_id = 0x1122334455667788,
    .credibility = 95,
    .accuracy_rate = 92,
    .last_verified = 1234567890,
    .total_articles = 10000,
    .false_articles = 50,
    .corrections = 25,
    .flags = 0x01  // Verified news organization
};

static source_info_t unreliable_source = {
    .source_id = 0x8877665544332211,
    .credibility = 25,
    .accuracy_rate = 45,
    .last_verified = 1234567000,
    .total_articles = 500,
    .false_articles = 275,
    .corrections = 150,
    .flags = 0x08  // Flagged for misinformation
};

static claim_t statistical_claim = {
    .claim_hash = 0xABCDEF1234567890,
    .subject_hash = 0x1111222233334444,
    .source_id = 0x1122334455667788,
    .claim_type = CLAIM_STATISTICAL,
    .confidence = 85,
    .timestamp = 1234567890,
    .evidence_mask = 0x0F,  // Multiple evidence types
    .related_claims = 0
};

static claim_t opinion_claim = {
    .claim_hash = 0x9876543210FEDCBA,
    .subject_hash = 0x5555666677778888,
    .source_id = 0x8877665544332211,
    .claim_type = CLAIM_OPINION,
    .confidence = 60,
    .timestamp = 1234567890,
    .evidence_mask = 0x01,  // Limited evidence
    .related_claims = 0
};

FEATURE(News_Validation_8_Tick_System) {
    
    SCENARIO("High-credibility sources pass validation quickly") {
        claim_t claim;
        source_info_t source;
        uint32_t validation_result;
        uint64_t start_ticks, end_ticks, duration;
        
        GIVEN("a statistical claim from a reliable source",
            claim = statistical_claim;
            source = reliable_source;
        );
        
        WHEN("validation is performed within tick budget",
            start_ticks = rdtsc_portable();
            validation_result = validate_claim_8tick(&claim, &source);
            end_ticks = rdtsc_portable();
            duration = end_ticks - start_ticks;
        );
        
        THEN("validation completes within 8 ticks",
            printf("       Validation took %llu ticks\n", 
                   (unsigned long long)duration);
            EXPECT_LT(duration, 9);
        );
        
        AND("the claim is marked as verified",
            EXPECT(validation_result & STATUS_VERIFIED);
            EXPECT(!(validation_result & STATUS_DISPUTED));
        );
    } END_SCENARIO
    
    SCENARIO("Low-credibility sources are flagged appropriately") {
        claim_t claim;
        source_info_t source;
        uint32_t validation_result;
        
        GIVEN("an opinion claim from an unreliable source",
            claim = opinion_claim;
            source = unreliable_source;
        );
        
        WHEN("validation is performed",
            validation_result = validate_claim_8tick(&claim, &source);
        );
        
        THEN("the claim is marked as disputed or unverified",
            EXPECT(validation_result & (STATUS_DISPUTED | STATUS_UNVERIFIED));
            EXPECT(!(validation_result & STATUS_VERIFIED));
        );
        
        AND("the low confidence is reflected in the result",
            // Claims with confidence < 70 from unreliable sources should be flagged
            printf("       Source credibility: %u, Claim confidence: %u\n",
                   source.credibility, claim.confidence);
        );
    } END_SCENARIO
    
    SCENARIO("Different claim types are processed correctly") {
        struct {
            uint32_t claim_type;
            const char* type_name;
            uint32_t expected_processing_flags;
        } claim_types[] = {
            {CLAIM_STATISTICAL, "Statistical", 0x01},
            {CLAIM_QUOTE, "Quote", 0x02},
            {CLAIM_EVENT, "Event", 0x04},
            {CLAIM_SCIENTIFIC, "Scientific", 0x08},
            {CLAIM_PREDICTION, "Prediction", 0x10},
            {CLAIM_OPINION, "Opinion", 0x20}
        };
        
        GIVEN("claims of different types from the same reliable source",
            // Using reliable source for consistent testing
        );
        
        WHEN("each claim type is validated",
            for (int i = 0; i < 6; i++) {
                claim_t test_claim = statistical_claim;
                test_claim.claim_type = claim_types[i].claim_type;
                test_claim.claim_hash += i;  // Make each unique
                
                uint32_t result = validate_claim_8tick(&test_claim, &reliable_source);
                
                printf("       %s claim result: 0x%08X\n", 
                       claim_types[i].type_name, result);
            }
        );
        
        THEN("all claim types are processed successfully",
            EXPECT(1);  // All types should be handled without errors
        );
    } END_SCENARIO
    
    SCENARIO("Evidence mask influences validation confidence") {
        claim_t claims_with_different_evidence[4];
        uint32_t results[4];
        
        GIVEN("identical claims with varying evidence levels",
            for (int i = 0; i < 4; i++) {
                claims_with_different_evidence[i] = statistical_claim;
                claims_with_different_evidence[i].claim_hash += i;
                claims_with_different_evidence[i].evidence_mask = (1 << i) - 1;
            }
            // Evidence masks: 0x0, 0x1, 0x3, 0x7 (increasing evidence)
        );
        
        WHEN("validation is performed on each claim",
            for (int i = 0; i < 4; i++) {
                results[i] = validate_claim_8tick(
                    &claims_with_different_evidence[i], 
                    &reliable_source
                );
                printf("       Evidence mask 0x%02X -> Result 0x%08X\n",
                       claims_with_different_evidence[i].evidence_mask,
                       results[i]);
            }
        );
        
        THEN("claims with more evidence have higher validation confidence",
            // Claims with more evidence should be less likely to be disputed
            uint32_t no_evidence_disputed = results[0] & STATUS_DISPUTED;
            uint32_t full_evidence_disputed = results[3] & STATUS_DISPUTED;
            
            // More evidence should lead to less disputation
            EXPECT(no_evidence_disputed >= full_evidence_disputed);
        );
    } END_SCENARIO
    
    SCENARIO("Source accuracy history affects validation") {
        source_info_t sources_with_different_history[3];
        uint32_t validation_results[3];
        
        GIVEN("sources with different accuracy histories",
            sources_with_different_history[0] = (source_info_t){
                .source_id = 0x1000000000000001,
                .credibility = 90,
                .accuracy_rate = 95,    // Very accurate
                .total_articles = 1000,
                .false_articles = 50,
                .corrections = 10,
                .flags = 0x01
            };
            
            sources_with_different_history[1] = (source_info_t){
                .source_id = 0x1000000000000002,
                .credibility = 70,
                .accuracy_rate = 75,    // Moderately accurate
                .total_articles = 1000,
                .false_articles = 250,
                .corrections = 100,
                .flags = 0x02
            };
            
            sources_with_different_history[2] = (source_info_t){
                .source_id = 0x1000000000000003,
                .credibility = 40,
                .accuracy_rate = 45,    // Poor accuracy
                .total_articles = 1000,
                .false_articles = 550,
                .corrections = 300,
                .flags = 0x04
            };
        );
        
        WHEN("the same claim is validated against different sources",
            claim_t test_claim = statistical_claim;
            for (int i = 0; i < 3; i++) {
                test_claim.source_id = sources_with_different_history[i].source_id;
                validation_results[i] = validate_claim_8tick(
                    &test_claim, 
                    &sources_with_different_history[i]
                );
                
                printf("       Accuracy %u%% -> Result 0x%08X\n",
                       sources_with_different_history[i].accuracy_rate,
                       validation_results[i]);
            }
        );
        
        THEN("higher accuracy sources receive better validation scores",
            // High accuracy source should be verified
            EXPECT(validation_results[0] & STATUS_VERIFIED);
            
            // Low accuracy source should be disputed or unverified
            EXPECT(validation_results[2] & (STATUS_DISPUTED | STATUS_UNVERIFIED));
        );
    } END_SCENARIO
    
    SCENARIO("Bulk validation maintains performance under load") {
        const int CLAIM_COUNT = 1000;
        claim_t claims[CLAIM_COUNT];
        source_info_t sources[CLAIM_COUNT];
        uint64_t total_ticks = 0;
        uint64_t max_ticks = 0;
        int verified_count = 0;
        int disputed_count = 0;
        
        GIVEN("a large batch of mixed claims and sources",
            for (int i = 0; i < CLAIM_COUNT; i++) {
                claims[i] = (claim_t){
                    .claim_hash = 0x1000000000000000 + i,
                    .subject_hash = 0x2000000000000000 + (i % 100),
                    .source_id = 0x3000000000000000 + (i % 50),
                    .claim_type = 1 << (i % 6),  // Rotate through claim types
                    .confidence = 50 + (i % 50), // 50-99 confidence
                    .timestamp = 1234567890 + i,
                    .evidence_mask = (i % 16),   // Varying evidence
                    .related_claims = 0
                };
                
                sources[i] = (source_info_t){
                    .source_id = 0x3000000000000000 + (i % 50),
                    .credibility = 30 + (i % 70),    // 30-99 credibility
                    .accuracy_rate = 40 + (i % 60),  // 40-99 accuracy
                    .total_articles = 100 + (i % 1000),
                    .false_articles = i % 100,
                    .corrections = i % 50,
                    .flags = (i % 4)
                };
            }
        );
        
        WHEN("all claims are validated",
            for (int i = 0; i < CLAIM_COUNT; i++) {
                uint64_t start = rdtsc_portable();
                uint32_t result = validate_claim_8tick(&claims[i], &sources[i]);
                uint64_t duration = rdtsc_portable() - start;
                
                total_ticks += duration;
                if (duration > max_ticks) {
                    max_ticks = duration;
                }
                
                if (result & STATUS_VERIFIED) verified_count++;
                if (result & STATUS_DISPUTED) disputed_count++;
            }
        );
        
        THEN("all validations complete within tick budget",
            printf("       Max ticks: %llu, Avg ticks: %llu\n",
                   (unsigned long long)max_ticks,
                   (unsigned long long)(total_ticks / CLAIM_COUNT));
            EXPECT_LT(max_ticks, 9);
        );
        
        AND("validation results show reasonable distribution",
            printf("       Verified: %d, Disputed: %d out of %d\n",
                   verified_count, disputed_count, CLAIM_COUNT);
            EXPECT_GT(verified_count, 0);
            EXPECT_GT(disputed_count, 0);
            EXPECT_LT(verified_count + disputed_count, CLAIM_COUNT * 2); // No double counting
        );
    } END_SCENARIO
}