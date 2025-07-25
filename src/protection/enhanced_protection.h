#ifndef ENHANCED_PROTECTION_H
#define ENHANCED_PROTECTION_H

#include "core_protection.h"
#include <stdatomic.h>

// Position entry for tracking correlated positions
typedef struct position_entry {
    char symbol[16];
    double notional_size;
    double correlation_factor;
    uint64_t timestamp;
    struct position_entry* next;
} position_entry_t;

// Enhanced protection with volatility detection and position aggregation
typedef struct {
    // Core protection (inherited)
    core_protection_t core;
    
    // Volatility detection (fixes flash crash vulnerability)
    double volatility_threshold_percent;   // 2% spike threshold
    uint64_t volatility_window_ms;         // 60 second window
    double price_history[120];             // 2 minutes of price data
    int price_history_index;
    atomic_bool high_volatility_detected;
    
    // Position aggregation (fixes manipulation vulnerability) 
    struct position_entry* positions_head;
    double correlated_exposure[32];        // Track by currency
    atomic_uint_fast32_t position_count;
    
    // Enhanced circuit breaker (fixes race condition)
    atomic_bool circuit_breaker_active;
    atomic_uint_fast64_t circuit_trigger_time;
    atomic_int_fast32_t halt_reason;       // Reason code for halt
    
    // Advanced metrics
    atomic_uint_fast64_t volatility_events;
    atomic_uint_fast64_t correlation_violations;
    atomic_uint_fast64_t manipulation_attempts;
    
    // Anti-manipulation timing
    atomic_uint_fast64_t last_request_time;
    
} enhanced_protection_t;

// Halt reason codes
typedef enum {
    HALT_NONE = 0,
    HALT_DAILY_LOSS = 1,
    HALT_VOLATILITY_SPIKE = 2,
    HALT_CORRELATION_BREACH = 3,
    HALT_KILL_SWITCH = 4,
    HALT_MANIPULATION_DETECTED = 5
} halt_reason_t;

// Enhanced validation result
typedef struct {
    protection_result_t base;
    double volatility_score;
    double correlation_risk;
    halt_reason_t recommended_halt;
    const char* security_warning;
} enhanced_result_t;

// Main enhanced validation function
enhanced_result_t validate_trade_enhanced(enhanced_protection_t* protection, 
                                        trade_request_t* request);

// Volatility detection functions
bool detect_price_volatility(enhanced_protection_t* protection, 
                           double current_price, const char* symbol);
void update_price_history(enhanced_protection_t* protection, 
                         double price, const char* symbol);
double calculate_volatility_score(const double* prices, int count);

// Position aggregation functions
bool check_position_correlation(enhanced_protection_t* protection, 
                              trade_request_t* request);
void add_position_tracking(enhanced_protection_t* protection, 
                         trade_request_t* request);
void remove_position_tracking(enhanced_protection_t* protection, 
                            const char* symbol);
double get_currency_exposure(enhanced_protection_t* protection, 
                           const char* currency);

// Enhanced circuit breaker functions
bool trigger_enhanced_circuit_breaker(enhanced_protection_t* protection, 
                                     halt_reason_t reason);
bool is_trading_halted(enhanced_protection_t* protection);
void reset_circuit_breaker(enhanced_protection_t* protection);

// Security functions to prevent manipulation
bool detect_manipulation_attempt(enhanced_protection_t* protection, 
                               trade_request_t* request);
bool validate_request_timing(enhanced_protection_t* protection, 
                           trade_request_t* request);
bool check_suspicious_patterns(enhanced_protection_t* protection, 
                             trade_request_t* request);

// Performance monitoring
typedef struct {
    atomic_uint_fast64_t validation_start;
    atomic_uint_fast64_t validation_end;
    atomic_uint_fast32_t response_time_us;
    atomic_uint_fast32_t violations_prevented;
    atomic_uint_fast32_t volatility_detections;
    atomic_uint_fast32_t correlation_blocks;
    atomic_uint_fast32_t manipulation_blocks;
} enhanced_metrics_t;

enhanced_metrics_t* get_enhanced_metrics(void);

// Initialization and cleanup
enhanced_protection_t* create_enhanced_protection(void);
void destroy_enhanced_protection(enhanced_protection_t* protection);

#endif // ENHANCED_PROTECTION_H