#ifndef CNS_PIPELINE_H
#define CNS_PIPELINE_H

#include "tick_parallel.h"
#include "bitactor_lite.h"

typedef struct {
    volatile uint64_t* rx_ring;
    volatile uint64_t* tx_ring;
} fast_nic_t;

typedef struct {
    uint64_t mask;
    uint64_t threshold;
    uint64_t action;
} rule_t;

void op_validate_quote(void* data);
void op_arena_alloc(void* data);
void op_bitactor_verify(void* data);
void op_apply_rule(void* data);
void op_calculate_price(void* data);
void op_risk_check(void* data);
void op_format_order(void* data);
void op_send_order(void* data);

void process_quote_8tick(quote_t* quote);

#endif