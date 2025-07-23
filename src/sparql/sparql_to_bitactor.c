#include "sparql_to_bitactor.h"
#include <string.h>
#include <time.h>

uint64_t hash_predicate(const char* predicate) {
    uint64_t hash = 0x1234567890ABCDEF;
    for (const char* p = predicate; *p; p++) {
        hash = hash * 31 + *p;
        hash ^= (hash >> 32);
    }
    return hash;
}

bool always_true(uint64_t* data) {
    return true;
}

bool validate_exists(uint64_t* data) {
    return *data != 0;
}

bool validate_not_expired(uint64_t* data) {
    time_t now = time(NULL);
    return *data > (uint64_t)now;
}

bool filter_gt(uint64_t* data) {
    return data[0] > data[1];
}

bool filter_lt(uint64_t* data) {
    return data[0] < data[1];
}

static uint64_t get_validator_for_filter(filter_t* filter) {
    if (!filter) return (uint64_t)validate_exists;
    
    switch (filter->type) {
        case FILTER_GT:
            return (uint64_t)filter_gt;
        case FILTER_LT:
            return (uint64_t)filter_lt;
        case FILTER_NOW_MINUS:
            return (uint64_t)validate_not_expired;
        default:
            return (uint64_t)validate_exists;
    }
}

void compile_sparql_to_hops(sparql_ast_t* ast, hop_template_t* hops) {
    for (int i = 0; i < ast->pattern_count && i < 8; i++) {
        sparql_pattern_t* p = &ast->patterns[i];
        
        hops[i].capability_id = hash_predicate(p->predicate);
        hops[i].validation_fn = get_validator_for_filter(p->filter);
        hops[i].data_offset = i * 8;
    }
    
    for (int i = ast->pattern_count; i < 8; i++) {
        hops[i].capability_id = CAP_IDENTITY;
        hops[i].validation_fn = (uint64_t)&always_true;
        hops[i].data_offset = i * 8;
    }
}