/*
 * FOREX BACKTESTING ENGINE: Real Historical Data Testing
 * Leverages ALL CNS components for authentic performance validation
 */

#ifndef BACKTESTING_ENGINE_H
#define BACKTESTING_ENGINE_H

#include "forex_core.h"
#include <stdio.h>
#include <time.h>

// HISTORICAL DATA STRUCTURES
typedef struct {
    uint64_t timestamp_ns;
    uint32_t currency_pair;
    double bid;
    double ask;
    uint64_t bid_volume;
    uint64_t ask_volume;
    uint8_t tier;              // Data source tier
    uint16_t spread_points;    // Spread in 0.1 pip units
} historical_tick_t;

typedef struct {
    char symbol[8];            // "EURUSD", "GBPUSD", etc.
    time_t start_date;
    time_t end_date;
    uint64_t total_ticks;
    double min_price;
    double max_price;
    double avg_spread;
    char data_source[32];      // "DukasCopy", "TrueFX", etc.
    char file_path[256];
} data_file_info_t;

// BACKTESTING CONFIGURATION
typedef struct {
    double initial_balance;
    uint16_t leverage;
    double commission_per_lot;     // In base currency
    double swap_long_rate;         // Daily swap for long positions
    double swap_short_rate;        // Daily swap for short positions
    uint32_t max_positions;
    double max_risk_per_trade;     // As percentage of balance
    bool use_zero_tick_filter;
    bool simulate_slippage;
    double slippage_points;        // Average slippage in points
} backtest_config_t;

// BACKTESTING RESULTS
typedef struct {
    // Performance metrics
    double final_balance;
    double total_return_pct;
    double max_drawdown_pct;
    double sharpe_ratio;
    double sortino_ratio;
    double calmar_ratio;
    
    // Trading statistics
    uint32_t total_trades;
    uint32_t winning_trades;
    uint32_t losing_trades;
    double win_rate_pct;
    double avg_win;
    double avg_loss;
    double profit_factor;
    double largest_win;
    double largest_loss;
    
    // Risk metrics
    double var_95;             // Value at Risk (95%)
    double max_consecutive_losses;
    double avg_trade_duration_hours;
    double avg_monthly_return;
    double volatility_annualized;
    
    // CNS-specific metrics
    uint64_t total_ticks_processed;
    uint64_t zero_ticks_filtered;
    double zero_tick_ratio;
    uint64_t total_processing_time_ns;
    double avg_tick_processing_ns;
    
    // Time analysis
    time_t backtest_start;
    time_t backtest_end;
    uint32_t total_days;
    uint32_t trading_days;
} backtest_results_t;

// TRADE RECORD FOR ANALYSIS
typedef struct {
    uint32_t trade_id;
    uint32_t currency_pair;
    time_t open_time;
    time_t close_time;
    double entry_price;
    double exit_price;
    int64_t position_size;     // Positive = long, negative = short
    double commission;
    double swap;
    double net_profit;
    double balance_after;
    uint16_t strategy_id;
    char close_reason[16];     // "TP", "SL", "Manual", "EOD"
    double max_favorable_excursion;
    double max_adverse_excursion;
    uint32_t duration_minutes;
} trade_record_t;

// STRATEGY INTERFACE FOR BACKTESTING
typedef struct {
    uint16_t strategy_id;
    char name[32];
    
    // Strategy callbacks
    forex_signal_t (*generate_signal)(const historical_tick_t* tick, void* state);
    void* (*init_state)(const backtest_config_t* config);
    void (*update_state)(void* state, const historical_tick_t* tick);
    void (*cleanup_state)(void* state);
    
    // Strategy parameters (strategy-specific)
    void* parameters;
} strategy_interface_t;

// MAIN BACKTESTING ENGINE
typedef struct {
    backtest_config_t config;
    strategy_interface_t* strategy;
    
    // Current state
    double current_balance;
    double current_equity;
    forex_position_t* positions;
    uint32_t position_count;
    uint32_t max_position_count;
    
    // Historical tracking
    trade_record_t* trade_history;
    uint32_t trade_count;
    uint32_t max_trades;
    
    // Daily snapshots for analysis
    double* daily_balances;
    double* daily_drawdowns;
    uint32_t daily_count;
    
    // CNS integration
    forex_account_t forex_account;
    forex_correlation_t correlation_matrix;
    bool use_cns_optimizations;
    
    // Progress tracking
    uint64_t ticks_processed;
    uint64_t total_ticks_expected;
    time_t last_progress_update;
} backtesting_engine_t;

// FUNCTION DECLARATIONS

// Engine lifecycle
backtesting_engine_t* backtest_engine_create(const backtest_config_t* config,
                                            strategy_interface_t* strategy);
void backtest_engine_destroy(backtesting_engine_t* engine);

// Data loading and processing
int backtest_load_data_file(const char* file_path, historical_tick_t** ticks, 
                           uint64_t* tick_count, data_file_info_t* info);
int backtest_process_tick(backtesting_engine_t* engine, const historical_tick_t* tick);

// Main backtesting execution
backtest_results_t backtest_run_period(backtesting_engine_t* engine,
                                      const char* data_directory,
                                      time_t start_date, time_t end_date,
                                      const char* currency_pairs[],
                                      uint32_t pair_count);

// Results analysis
void backtest_calculate_metrics(backtesting_engine_t* engine, backtest_results_t* results);
void backtest_print_results(const backtest_results_t* results);
void backtest_export_trades_csv(const backtesting_engine_t* engine, const char* filename);
void backtest_export_equity_curve_csv(const backtesting_engine_t* engine, const char* filename);

// Built-in strategies for testing
strategy_interface_t* create_moving_average_strategy(int fast_period, int slow_period);
strategy_interface_t* create_mean_reversion_strategy(double deviation_threshold);
strategy_interface_t* create_breakout_strategy(int lookback_period);
strategy_interface_t* create_news_trading_strategy(void);

// Utility functions
time_t parse_date_string(const char* date_str);  // "2024-01-01"
const char* format_date_string(time_t timestamp, char* buffer, size_t size);
double calculate_correlation(const double* series1, const double* series2, uint32_t length);
double calculate_sharpe_ratio(const double* returns, uint32_t length, double risk_free_rate);

// CNS Integration functions
void backtest_enable_cns_optimizations(backtesting_engine_t* engine);
void backtest_update_correlation_matrix(backtesting_engine_t* engine, 
                                       const historical_tick_t* tick);
bool backtest_apply_zero_tick_filter(const historical_tick_t* tick);

// Risk management during backtest
bool backtest_check_risk_limits(backtesting_engine_t* engine, const forex_signal_t* signal);
void backtest_close_position(backtesting_engine_t* engine, uint32_t position_index, 
                            const historical_tick_t* tick, const char* reason);
void backtest_update_stops_and_targets(backtesting_engine_t* engine, 
                                      const historical_tick_t* tick);

// Data validation
bool validate_historical_data(const historical_tick_t* ticks, uint64_t tick_count);
void remove_outliers(historical_tick_t* ticks, uint64_t* tick_count, double z_threshold);
void fill_data_gaps(historical_tick_t* ticks, uint64_t* tick_count, uint32_t max_gap_seconds);

#endif // BACKTESTING_ENGINE_H