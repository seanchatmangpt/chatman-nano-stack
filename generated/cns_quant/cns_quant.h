#ifndef CNS_QUANT_H
#define CNS_QUANT_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Signal definitions for cns_quant */
typedef enum {
    CNS_QUANT_SIGNAL_MARKET_DATA = 1,
    CNS_QUANT_SIGNAL_TRADE_EXECUTED = 2,
    CNS_QUANT_SIGNAL_RISK_CALCULATED = 3,
    CNS_QUANT_SIGNAL_COMPLIANCE_CHECK = 4,
    CNS_QUANT_SIGNAL_PORTFOLIO_UPDATED = 5
} cns_quant_signal_t;

/* BitActor structure */
typedef struct {
    uint32_t state;
    uint64_t tick_count;
    uint64_t signal_count;
    uint8_t scratch[4096];
} cns_quant_bitactor_t;

/* API functions */
bool cns_quant_init(cns_quant_bitactor_t* actor);
bool cns_quant_tick(cns_quant_bitactor_t* actor);
bool cns_quant_emit(cns_quant_bitactor_t* actor, cns_quant_signal_t signal);

#endif /* CNS_QUANT_H */
