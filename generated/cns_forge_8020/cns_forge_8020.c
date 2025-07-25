/**
 * CNS Forge 80/20 BitActor C Implementation
 * Integrates with existing BitActor infrastructure
 */

#include "cns_forge_8020.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

/* Global telemetry data */
static cns_forge_telemetry_t g_telemetry = {0};

bool cns_forge_init(void) {
    memset(&g_telemetry, 0, sizeof(cns_forge_telemetry_t));
    g_telemetry.start_time = (uint64_t)time(NULL);
    return true;
}

bool cns_forge_create_token(cns_forge_token_t* token, uint32_t workflow_type,
                           const void* payload, size_t payload_size) {
    if (!token || payload_size > CNS_FORGE_TOKEN_SIZE) {
        return false;
    }
    
    memset(token, 0, sizeof(cns_forge_token_t));
    token->ttl_hops = CNS_FORGE_MAX_TTL_HOPS;
    token->transaction_id = (uint64_t)time(NULL) * 1000000 + rand();
    token->created_at = (uint64_t)time(NULL);
    token->workflow_type = workflow_type;
    token->payload_size = (uint32_t)payload_size;
    
    if (payload && payload_size > 0) {
        memcpy(token->payload, payload, payload_size);
    }
    
    g_telemetry.workflows_executed++;
    cns_forge_emit_pulse_log(token->transaction_id, "token_created", token);
    
    return true;
}

cns_forge_hop_result_t cns_forge_process_hop(cns_forge_token_t* token) {
    if (!token) {
        return CNS_FORGE_HOP_ERROR;
    }
    
    /* Check TTL before processing */
    if (cns_forge_token_expired(token)) {
        g_telemetry.ttl_expirations++;
        cns_forge_emit_pulse_log(token->transaction_id, "ttl_expired", token);
        return CNS_FORGE_HOP_TTL_EXPIRED;
    }
    
    /* Decrement TTL (hop-based execution) */
    if (!cns_forge_decrement_ttl(token)) {
        return CNS_FORGE_HOP_TTL_EXPIRED;
    }
    
    /* Process the hop (simplified for 80/20 implementation) */
    /* In production, this would call existing BitActor infrastructure */
    
    g_telemetry.hops_processed++;
    cns_forge_emit_pulse_log(token->transaction_id, "hop_processed", token);
    
    /* Check if workflow completed */
    if (token->ttl_hops == 0) {
        g_telemetry.successful_completions++;
        return CNS_FORGE_HOP_COMPLETED;
    }
    
    return CNS_FORGE_HOP_SUCCESS;
}

bool cns_forge_emit_pulse_log(uint64_t transaction_id, const char* event,
                             const cns_forge_token_t* token) {
    /* CNS Forge Universal Observability - Pulse Log */
    printf("[PULSE] TxID:%llu Event:%s TTL:%u Timestamp:%llu\n",
           transaction_id, event, token->ttl_hops, (uint64_t)time(NULL));
    
    /* In production, this would integrate with OTEL/telemetry infrastructure */
    return true;
}

void cns_forge_get_telemetry(cns_forge_telemetry_t* telemetry) {
    if (telemetry) {
        *telemetry = g_telemetry;
    }
}

/* Demonstration function */
int cns_forge_demo_workflow(void) {
    printf("🚀 CNS Forge 80/20 Demo Workflow\n");
    
    cns_forge_init();
    
    /* Create initial token */
    cns_forge_token_t token;
    const char* payload = "demo_request";
    if (!cns_forge_create_token(&token, 1, payload, strlen(payload))) {
        printf("❌ Failed to create token\n");
        return 1;
    }
    
    printf("✅ Created token with TTL=%u\n", token.ttl_hops);
    
    /* Process hops until completion or TTL expiration */
    int hop_count = 0;
    while (true) {
        cns_forge_hop_result_t result = cns_forge_process_hop(&token);
        hop_count++;
        
        switch (result) {
            case CNS_FORGE_HOP_SUCCESS:
                printf("  Hop %d: Success (TTL remaining: %u)\n", hop_count, token.ttl_hops);
                break;
            case CNS_FORGE_HOP_COMPLETED:
                printf("  Hop %d: Workflow completed successfully\n", hop_count);
                goto workflow_done;
            case CNS_FORGE_HOP_TTL_EXPIRED:
                printf("  Hop %d: TTL expired\n", hop_count);
                goto workflow_done;
            case CNS_FORGE_HOP_ERROR:
                printf("  Hop %d: Error occurred\n", hop_count);
                return 1;
        }
    }
    
workflow_done:
    /* Print telemetry */
    {
        cns_forge_telemetry_t telemetry;
        cns_forge_get_telemetry(&telemetry);
        
        printf("\n📊 Telemetry:\n");
        printf("  Workflows executed: %llu\n", telemetry.workflows_executed);
        printf("  Hops processed: %llu\n", telemetry.hops_processed);
        printf("  TTL expirations: %llu\n", telemetry.ttl_expirations);
        printf("  Successful completions: %llu\n", telemetry.successful_completions);
    }
    
    return 0;
}

#ifdef CNS_FORGE_DEMO
/* Demo main function */
int main() {
    return cns_forge_demo_workflow();
}
#endif
