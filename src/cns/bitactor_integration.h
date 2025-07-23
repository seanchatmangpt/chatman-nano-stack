#ifndef BITACTOR_INTEGRATION_H
#define BITACTOR_INTEGRATION_H

#include "cns_pipeline.h"
#include "bitactor_parallel.h"
#include "bitactor_dispatch.h"
#include "bitactor_telemetry.h"

// CNS BitActor integration context
typedef struct {
    bitactor_parallel_t parallel;
    dispatch_table_t dispatch;
    telemetry_manager_t telemetry;
    
    // Integration with CNS pipeline
    tick_unit_t* pipeline_units;
    uint32_t pipeline_count;
    
    // Market data handlers
    void (*quote_handler)(quote_t* quote);
    void (*order_handler)(order_t* order);
    
    // Performance tracking
    uint64_t quotes_processed;
    uint64_t orders_generated;
    uint64_t average_latency;
} cns_bitactor_t;

// Integration API
void cns_bitactor_init(cns_bitactor_t* cba);
void cns_bitactor_register_handlers(cns_bitactor_t* cba);
void cns_bitactor_process_quote(cns_bitactor_t* cba, quote_t* quote);
void cns_bitactor_process_order(cns_bitactor_t* cba, order_t* order);
void cns_bitactor_tick(cns_bitactor_t* cba);

// Handler implementations for market data
void bitactor_handle_quote_validate(signal_t* sig, void* scratch);
void bitactor_handle_risk_check(signal_t* sig, void* scratch);
void bitactor_handle_order_format(signal_t* sig, void* scratch);

// Pipeline integration
void bitactor_integrate_pipeline(cns_bitactor_t* cba, tick_unit_t* units, uint32_t count);
void bitactor_pipeline_tick(cns_bitactor_t* cba);

// Performance monitoring
typedef struct {
    uint64_t tick_count;
    uint64_t signal_count;
    uint64_t cycle_total;
    uint64_t cycle_min;
    uint64_t cycle_max;
    double cycle_avg;
    double efficiency;
} bitactor_stats_t;

void bitactor_get_stats(const cns_bitactor_t* cba, bitactor_stats_t* stats);
void bitactor_reset_stats(cns_bitactor_t* cba);

#endif