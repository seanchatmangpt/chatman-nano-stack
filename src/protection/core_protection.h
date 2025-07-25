#ifndef CORE_PROTECTION_H
#define CORE_PROTECTION_H

#include <stdint.h>
#include <stdbool.h>
#include <time.h>

// Core 20% protection mechanisms that prevent 80% of account failures
typedef struct {
    // Position sizing limits (prevents 40% of failures)
    double max_position_risk_percent;  // 1% hard limit
    double current_exposure;           // Total exposure tracking
    
    // Daily loss circuit breaker (prevents 30% of failures)
    double daily_loss_limit_percent;   // 2% circuit breaker
    double daily_pnl;                  // Current day P&L
    bool trading_halted;               // Circuit breaker state
    
    // Stop loss enforcement (prevents 20% of failures)
    bool require_stop_loss;            // 100% compliance
    double default_stop_percent;       // Default 2% stop
    
    // Emergency kill switch (prevents 10% of failures)
    bool kill_switch_enabled;          // Emergency halt capability
    uint64_t kill_switch_timestamp;    // When activated
    uint32_t max_response_time_ms;     // 100ms requirement
} core_protection_t;

// Trade request with protection validation
typedef struct {
    char symbol[16];
    double position_size;
    double entry_price;
    double stop_loss;
    double account_balance;
    uint64_t timestamp;
} trade_request_t;

// Protection validation result
typedef struct {
    bool approved;
    const char* rejection_reason;
    double adjusted_size;
    double required_stop_loss;
} protection_result_t;

// Core protection functions (the critical 20%)
protection_result_t validate_trade_protection(core_protection_t* protection, 
                                            trade_request_t* request);

bool check_position_size_limit(core_protection_t* protection, 
                              trade_request_t* request);

bool check_daily_loss_circuit(core_protection_t* protection);

bool enforce_stop_loss(trade_request_t* request, 
                      double default_stop_percent);

bool activate_kill_switch(core_protection_t* protection);

bool is_trading_allowed(core_protection_t* protection);

void update_daily_pnl(core_protection_t* protection, double pnl_change);

void reset_daily_counters(core_protection_t* protection);

// Performance monitoring
typedef struct {
    uint64_t validation_start;
    uint64_t validation_end;
    uint32_t response_time_us;
    uint32_t violations_prevented;
} protection_metrics_t;

protection_metrics_t* get_protection_metrics(void);

#endif // CORE_PROTECTION_H