#ifndef CNS_PIPELINE_H
#define CNS_PIPELINE_H

#include "tick_parallel.h"
#include "bitactor_lite.h"

// Market data structures
typedef struct {
    uint64_t symbol;
    uint64_t price;
    uint64_t volume;
    uint64_t timestamp;
} quote_t;

typedef struct {
    uint64_t symbol;
    uint64_t price;
    uint64_t quantity;
    uint64_t flags;
} order_t;

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