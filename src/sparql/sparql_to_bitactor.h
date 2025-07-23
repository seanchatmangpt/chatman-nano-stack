#ifndef SPARQL_TO_BITACTOR_H
#define SPARQL_TO_BITACTOR_H

#include "sparql_ast.h"
#include <stdint.h>

#define CAP_IDENTITY 0x0000000000000000
#define VALIDATOR_ALWAYS_TRUE 0x400100

typedef struct {
    uint64_t capability_id;
    uint64_t validation_fn;
    uint64_t data_offset;
} hop_template_t;

typedef struct {
    hop_template_t hops[8];
} proof_chain_t;

void compile_sparql_to_hops(sparql_ast_t* ast, hop_template_t* hops);
uint64_t hash_predicate(const char* predicate);

bool always_true(uint64_t* data);
bool validate_exists(uint64_t* data);
bool validate_not_expired(uint64_t* data);
bool filter_gt(uint64_t* data);
bool filter_lt(uint64_t* data);
bool bitactor_execute_8hop(const proof_chain_t* chain, void* data);

#endif