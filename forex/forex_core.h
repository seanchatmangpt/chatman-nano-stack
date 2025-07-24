/*
 * FOREX CORE: Leveraging CNS BitActor for 50x Leverage Trading
 * Reuses ALL existing CNS infrastructure intelligently
 */

#ifndef FOREX_CORE_H
#define FOREX_CORE_H

#include "../bitactor/include/bitactor/bitactor.h"
#include "../src/cns/bitactor_telemetry.h"
#include <stdint.h>
#include <stdbool.h>

// LEVERAGE EXISTING: BitActor Signal System for Forex Ticks
typedef struct {
    bitactor_signal_t base;        // Reuse existing signal infrastructure
    uint32_t currency_pair;        // EUR_USD = 0x45555255 (perfect hash ready)
    int32_t bid_price;            // In 0.00001 units (5-digit precision)
    int32_t ask_price;
    uint32_t bid_volume;
    uint32_t ask_volume;
    uint64_t timestamp_ns;        // High-precision timing from BitActor
    uint8_t tier;                 // Tier-1 bank = 0, ECN = 1, etc.
    uint8_t flags;               // Last-look, RFQ, etc.
} forex_tick_t;

// LEVERAGE EXISTING: Zero-tick optimization for forex noise filtering
typedef struct {
    uint32_t pair_mask;          // Which pairs to filter
    uint32_t noise_threshold;    // Spread threshold for zero-tick
    uint32_t zero_tick_count;    // Reuse existing zero-tick counters
    double min_spread_change;    // Minimum spread change to process
    bool heartbeat_filter;       // Use existing heartbeat filtering
} forex_filter_config_t;

// INTEGRATE WITH: Existing risk management structures
typedef struct {
    double balance;
    double equity;
    double margin_used;
    double margin_free;
    double margin_level;         // (equity/margin_used) * 100
    uint16_t leverage;           // 50x = 50
    bool margin_call;           // Trigger at 50%
    bool stop_out;              // Force close at 20%
    uint32_t open_positions;
    double daily_pnl;
    double max_drawdown;
} forex_account_t;

// REUSE: BitActor messaging for position management
typedef struct {
    uint32_t position_id;
    uint32_t currency_pair;
    int64_t size;               // Positive = long, negative = short
    double entry_price;
    double current_price;
    double stop_loss;
    double take_profit;
    double unrealized_pnl;
    double margin_required;
    uint64_t open_time;
    uint8_t status;             // Open, pending, closed
} forex_position_t;

// LEVERAGE: SIMD correlation matrix from existing optimization
typedef struct {
    float correlation_matrix[28][28];  // Major pairs correlation
    uint64_t last_update_ns;
    bool matrix_valid;
    uint32_t calculation_cycles;       // Reuse existing cycle counting
} forex_correlation_t;

// INTEGRATE: Economic events with existing news pipeline
typedef struct {
    uint64_t event_time_ns;
    uint32_t currency;           // USD, EUR, GBP, etc.
    uint8_t impact;             // High=3, Medium=2, Low=1
    uint8_t event_type;         // NFP, FOMC, ECB, etc.
    bool pre_blackout;          // 30s before event
    bool post_blackout;         // 60s after event
    char description[64];
} forex_economic_event_t;

// REUSE: Existing strategy pipeline for forex strategies
typedef struct {
    uint32_t strategy_id;
    uint32_t currency_pair;
    double signal_strength;      // -1.0 to 1.0
    double confidence;          // 0.0 to 1.0
    uint32_t timeframe;         // M1, M5, H1, etc.
    double recommended_size;    // In lots
    double stop_distance;       // In pips
    uint64_t signal_time;
    bool validated;            // Passed risk checks
} forex_signal_t;

// MACRO DEFINITIONS FOR CURRENCY PAIRS (Perfect Hash Compatible)
#define EUR_USD 0x45555255
#define GBP_USD 0x47425055
#define USD_JPY 0x5553444A
#define USD_CHF 0x55534348
#define AUD_USD 0x41554455
#define USD_CAD 0x55534341
#define NZD_USD 0x4E5A4455
// ... 21 more major pairs

// LEVERAGE EXISTING: BitActor processing functions
int forex_init_engine(void);
int forex_process_tick(const forex_tick_t* tick);
int forex_update_position(forex_position_t* position, double new_price);
int forex_check_margin(forex_account_t* account);
int forex_apply_zero_tick_filter(forex_tick_t* tick, const forex_filter_config_t* config);

// REUSE: Existing SIMD optimizations
void forex_update_correlation_simd(forex_correlation_t* corr, const float* price_changes);
void forex_calculate_portfolio_risk_avx2(const forex_position_t* positions, 
                                         uint32_t count, double* total_risk);

// INTEGRATE: Economic calendar with existing news pipeline  
bool forex_is_news_blackout(uint32_t currency_pair, uint64_t current_time);
forex_signal_t forex_generate_signal(uint32_t pair, const forex_tick_t* tick);

#endif // FOREX_CORE_H