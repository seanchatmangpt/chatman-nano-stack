/**
 * CNS Forge 80/20 Test Suite
 * Validates TTL-driven execution and integration
 */

#include "cns_forge_8020.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) \
    do { \
        printf("Running test: %s... ", #name); \
        tests_run++; \
        if (test_##name()) { \
            printf("✅ PASS\n"); \
            tests_passed++; \
        } else { \
            printf("❌ FAIL\n"); \
        } \
    } while(0)

/* Test: Basic token creation */
bool test_token_creation() {
    cns_forge_token_t token;
    const char* payload = "test_payload";
    
    bool result = cns_forge_create_token(&token, 1, payload, strlen(payload));
    
    return result && 
           token.ttl_hops == CNS_FORGE_MAX_TTL_HOPS &&
           token.payload_size == strlen(payload) &&
           memcmp(token.payload, payload, strlen(payload)) == 0;
}

/* Test: TTL decrementation */
bool test_ttl_decrementation() {
    cns_forge_token_t token;
    cns_forge_create_token(&token, 1, "test", 4);
    
    uint32_t initial_ttl = token.ttl_hops;
    bool decremented = cns_forge_decrement_ttl(&token);
    
    return decremented && token.ttl_hops == (initial_ttl - 1);
}

/* Test: TTL expiration */
bool test_ttl_expiration() {
    cns_forge_token_t token;
    cns_forge_create_token(&token, 1, "test", 4);
    
    /* Decrement TTL to 0 */
    for (int i = 0; i < CNS_FORGE_MAX_TTL_HOPS; i++) {
        cns_forge_decrement_ttl(&token);
    }
    
    return cns_forge_token_expired(&token);
}

/* Test: Hop processing */
bool test_hop_processing() {
    cns_forge_token_t token;
    cns_forge_create_token(&token, 1, "test", 4);
    
    /* Process a hop */
    cns_forge_hop_result_t result = cns_forge_process_hop(&token);
    
    return result == CNS_FORGE_HOP_SUCCESS && token.ttl_hops == (CNS_FORGE_MAX_TTL_HOPS - 1);
}

/* Test: Complete workflow */
bool test_complete_workflow() {
    cns_forge_token_t token;
    cns_forge_create_token(&token, 1, "workflow_test", 13);
    
    /* Process all hops */
    cns_forge_hop_result_t result;
    int hops = 0;
    
    do {
        result = cns_forge_process_hop(&token);
        hops++;
    } while (result == CNS_FORGE_HOP_SUCCESS && hops < 20); /* Safety limit */
    
    return result == CNS_FORGE_HOP_COMPLETED || result == CNS_FORGE_HOP_TTL_EXPIRED;
}

/* Test: Telemetry collection */
bool test_telemetry() {
    cns_forge_init(); /* Reset telemetry */
    
    cns_forge_telemetry_t initial_telemetry;
    cns_forge_get_telemetry(&initial_telemetry);
    
    /* Create and process a token */
    cns_forge_token_t token;
    cns_forge_create_token(&token, 1, "telemetry_test", 14);
    cns_forge_process_hop(&token);
    
    cns_forge_telemetry_t final_telemetry;
    cns_forge_get_telemetry(&final_telemetry);
    
    return final_telemetry.workflows_executed > initial_telemetry.workflows_executed &&
           final_telemetry.hops_processed > initial_telemetry.hops_processed;
}

int main() {
    printf("🧪 CNS Forge 80/20 Test Suite\n");
    printf("==============================\n");
    
    cns_forge_init();
    
    TEST(token_creation);
    TEST(ttl_decrementation);
    TEST(ttl_expiration);
    TEST(hop_processing);
    TEST(complete_workflow);
    TEST(telemetry);
    
    printf("\n📊 Test Results: %d/%d tests passed\n", tests_passed, tests_run);
    
    if (tests_passed == tests_run) {
        printf("✅ All tests passed!\n");
        return 0;
    } else {
        printf("❌ Some tests failed.\n");
        return 1;
    }
}

#ifdef CNS_FORGE_DEMO
/* Demo main function */
int main() {
    return cns_forge_demo_workflow();
}
#endif
