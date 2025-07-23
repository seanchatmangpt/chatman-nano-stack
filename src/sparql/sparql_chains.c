// Generated SPARQL chains - DO NOT EDIT
#include <stdint.h>
#include <stdbool.h>
#include "sparql_to_bitactor.h"

// Forward declarations for validators
extern bool always_true(uint64_t* data);
extern bool validate_exists(uint64_t* data);
extern bool validate_not_expired(uint64_t* data);
extern bool filter_gt(uint64_t* data);
extern bool filter_lt(uint64_t* data);

// Generated from compliance_check.rq
static const proof_chain_t compliance_check_chain = {
    .hops = {
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 0},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 8},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 16},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 24},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 32},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 40},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 48},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 56}
    }
};

bool execute_compliance_check(void* data) {
    return bitactor_execute_8hop(&compliance_check_chain, data);
}

// Generated from market_access.rq
static const proof_chain_t market_access_chain = {
    .hops = {
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 0},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 8},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 16},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 24},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 32},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 40},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 48},
        {.capability_id = 0x0000000000000000, .validation_fn = (uint64_t)&always_true, .data_offset = 56}
    }
};

bool execute_market_access(void* data) {
    return bitactor_execute_8hop(&market_access_chain, data);
}

// 8-tick execution engine
bool bitactor_execute_8hop(const proof_chain_t* chain, void* data) {
    uint64_t result = 0xFFFFFFFFFFFFFFFF;
    
    // Unrolled loop - exactly 8 iterations, 1 tick each
    #pragma unroll 8
    for (int i = 0; i < 8; i++) {
        const hop_template_t* hop = &chain->hops[i];
        uint64_t* hop_data = (uint64_t*)((char*)data + hop->data_offset);
        bool (*validator)(uint64_t*) = (void*)hop->validation_fn;
        
        uint64_t valid = validator(hop_data);
        result &= -valid;
    }
    
    return result != 0;
}
