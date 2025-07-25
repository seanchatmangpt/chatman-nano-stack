/**
 * CNS Forge 80/20 BitActor C Integration
 * TTL-driven execution with 8-hop budget
 * @copyright 2025 CNS - Technology Applications, Inc.
 */

#ifndef CNS_FORGE_8020_H
#define CNS_FORGE_8020_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <time.h>

/* CNS Forge Constants */
#define CNS_FORGE_MAX_TTL_HOPS    8
#define CNS_FORGE_TICK_BUDGET     8
#define CNS_FORGE_MAX_SIGNALS     512
#define CNS_FORGE_TOKEN_SIZE      512

/* TTL Token Structure */
typedef struct {
    uint32_t ttl_hops;              /* Remaining hops (starts at 8) */
    uint64_t transaction_id;        /* Unique transaction ID */
    uint64_t created_at;           /* Creation timestamp */
    uint32_t workflow_type;        /* Type of workflow */
    uint32_t payload_size;         /* Payload size */
    uint8_t payload[CNS_FORGE_TOKEN_SIZE]; /* Serialized payload */
} cns_forge_token_t;

/* Hop Processing Result */
typedef enum {
    CNS_FORGE_HOP_SUCCESS = 0,
    CNS_FORGE_HOP_TTL_EXPIRED,
    CNS_FORGE_HOP_ERROR,
    CNS_FORGE_HOP_COMPLETED
} cns_forge_hop_result_t;

/* Telemetry Structure */
typedef struct {
    uint64_t workflows_executed;
    uint64_t hops_processed;
    uint64_t ttl_expirations;
    uint64_t successful_completions;
    uint64_t start_time;
} cns_forge_telemetry_t;

/* Core API */
bool cns_forge_init(void);
bool cns_forge_create_token(cns_forge_token_t* token, uint32_t workflow_type, 
                           const void* payload, size_t payload_size);
cns_forge_hop_result_t cns_forge_process_hop(cns_forge_token_t* token);
bool cns_forge_emit_pulse_log(uint64_t transaction_id, const char* event, 
                             const cns_forge_token_t* token);
void cns_forge_get_telemetry(cns_forge_telemetry_t* telemetry);

/* Utility Functions */
static inline bool cns_forge_token_expired(const cns_forge_token_t* token) {
    return token->ttl_hops == 0;
}

static inline bool cns_forge_decrement_ttl(cns_forge_token_t* token) {
    if (token->ttl_hops > 0) {
        token->ttl_hops--;
        return true;
    }
    return false;
}

#endif /* CNS_FORGE_8020_H */
