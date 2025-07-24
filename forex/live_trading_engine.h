/*
 * LIVE FOREX TRADING ENGINE: Real 50x Competition System
 * This is NOT backtesting - this is REAL money with REAL brokers
 */

#ifndef LIVE_TRADING_ENGINE_H
#define LIVE_TRADING_ENGINE_H

#include "forex_core.h"
#include <curl/curl.h>      // For REST API calls
#include <websockets.h>     // For real-time data streams
#include <json-c/json.h>    // For API message parsing

// REAL BROKER CONNECTION (Not simulation)
typedef struct {
    char broker_name[32];           // "OANDA", "FXCM", "InteractiveBrokers"
    char api_base_url[256];         // "https://api-fxtrade.oanda.com/v3"
    char stream_url[256];           // "wss://stream-fxtrade.oanda.com/v3"
    char auth_token[512];           // Bearer token for authentication
    char account_id[64];            // Real account number
    bool is_live_account;           // true = real money, false = demo
    
    // Connection status
    bool rest_api_connected;        // HTTP API status
    bool stream_connected;          // WebSocket stream status
    uint64_t last_heartbeat_ns;     // Last successful ping
    uint32_t reconnect_attempts;    // Connection retry count
    uint32_t api_calls_today;       // Rate limiting tracking
    uint32_t max_api_calls_per_day; // Broker's limit
    
    // Account state (retrieved from broker)
    double account_balance;         // Real balance from broker
    double account_equity;          // Real equity from broker
    double margin_used;             // Real margin from broker
    double margin_available;        // Real available margin
    double daily_pnl;              // Today's P&L from broker
    uint32_t open_positions;        // Position count from broker
    
    // Network performance
    uint64_t avg_api_latency_ns;    // REST API round-trip time
    uint64_t avg_stream_latency_ns; // WebSocket message delay
    uint32_t api_error_count;       // Failed API calls
    uint32_t stream_error_count;    // Stream disconnections
} live_broker_t;

// REAL MARKET DATA (Not synthetic)
typedef struct {
    uint64_t timestamp_ns;          // Exchange timestamp (not our timestamp)
    uint32_t currency_pair;         // EUR_USD, GBP_USD, etc.
    double bid;                     // Best bid across all venues
    double ask;                     // Best ask across all venues
    uint64_t bid_volume;            // Liquidity available at bid
    uint64_t ask_volume;            // Liquidity available at ask
    
    // Market microstructure
    uint8_t venue_count;            // Number of ECNs contributing
    bool is_indicative;             // true = no execution possible
    double effective_spread;        // Actual tradeable spread
    market_condition_t condition;   // NORMAL, VOLATILE, HALTED, THIN
    
    // Latency tracking
    uint64_t received_at_ns;        // When we received this tick
    uint64_t processing_delay_ns;   // Network + processing delay
    bool is_stale;                  // true = data older than threshold
} live_market_data_t;

// REAL ORDER EXECUTION (Not perfect fills)
typedef enum {
    ORDER_PENDING,                  // Submitted, waiting for execution
    ORDER_WORKING,                  // Partially filled
    ORDER_FILLED,                   // Completely filled
    ORDER_REJECTED,                 // Broker rejected
    ORDER_EXPIRED,                  // Timed out
    ORDER_CANCELLED                 // Manually cancelled
} order_status_t;

typedef struct {
    uint64_t our_order_id;          // Our internal tracking ID
    char broker_order_id[64];       // Broker's order ID
    uint32_t currency_pair;         // Which pair
    
    // Order details
    double requested_price;         // What we wanted
    double executed_price;          // What we got (includes slippage)
    int64_t requested_size;         // Position size requested
    int64_t executed_size;          // Actually executed (partial fills)
    order_type_t type;              // MARKET, LIMIT, STOP
    
    // Timing
    uint64_t submit_time_ns;        // When we sent to broker
    uint64_t ack_time_ns;           // When broker acknowledged
    uint64_t execution_time_ns;     // When trade executed
    uint64_t total_latency_ns;      // Submit to execution time
    
    // Execution quality
    double slippage_pips;           // Price difference from request
    double commission_paid;         // Actual commission charged
    order_status_t status;          // Current status
    char rejection_reason[128];     // Why order failed (if rejected)
    
    // Risk tracking
    bool exceeds_risk_limits;       // Position too large
    bool market_conditions_poor;    // Wide spreads, low liquidity
} live_order_t;

// REAL POSITION MANAGEMENT (Not simulated P&L)
typedef struct {
    char position_id[64];           // Broker's position ID
    uint32_t currency_pair;         // Which pair
    int64_t size;                   // Current position size
    double avg_entry_price;         // Average fill price
    double current_market_price;    // Real-time market price
    
    // P&L (from broker, not calculated)
    double unrealized_pnl;          // From broker API
    double realized_pnl;            // From broker API
    double total_commission;        // Actual commissions paid
    double swap_charges;            // Overnight financing charges
    
    // Risk management
    double stop_loss_price;         // Protective stop
    double take_profit_price;       // Profit target
    bool has_guaranteed_stop;       // Guaranteed stop order
    double margin_required;         // Actual margin from broker
    
    // Performance tracking
    uint64_t open_time_ns;          // When position opened
    double max_favorable_excursion; // Best unrealized profit
    double max_adverse_excursion;   // Worst unrealized loss
    uint32_t duration_seconds;      // How long position held
} live_position_t;

// REAL RISK MANAGEMENT (Circuit breakers that work)
typedef struct {
    // Account limits
    double max_account_drawdown_pct;    // 15% = emergency liquidation
    double max_daily_loss_usd;          // Stop trading for today
    double max_position_size_usd;       // Never exceed this amount
    double max_leverage_ratio;          // Never exceed broker's limit
    
    // Position limits  
    uint32_t max_positions;             // Maximum concurrent positions
    double max_correlated_exposure;     // Max USD exposure across pairs
    double max_single_pair_exposure;    // Max exposure to any one pair
    uint32_t max_consecutive_losses;    // Stop after X losing trades
    
    // Market condition limits
    double max_spread_pips;             // Don't trade if spread > X pips
    uint64_t min_liquidity_volume;      // Don't trade if volume < X
    bool news_trading_disabled;         // Disable during high-impact news
    uint32_t volatility_circuit_breaker; // Halt if volatility > X%
    
    // Emergency actions
    bool emergency_liquidation_active;  // Force close all positions
    bool trading_halted;               // Stop all new trades
    uint64_t halt_until_timestamp;     // Resume trading after this time
    emergency_reason_t halt_reason;    // Why trading was halted
    
    // Real-time monitoring
    bool margin_call_active;           // Broker issued margin call
    double current_margin_level;       // Real margin level from broker
    uint32_t failed_orders_today;      // Count of rejected orders
    uint64_t last_risk_check_ns;       // When we last checked limits
} live_risk_manager_t;

// REAL NEWS/EVENTS (Not simulated)
typedef struct {
    uint64_t event_time_ns;            // Exact event time
    char currency[4];                  // "USD", "EUR", "GBP"
    event_impact_t impact;             // HIGH, MEDIUM, LOW
    event_type_t type;                 // NFP, FOMC, ECB, CPI, etc.
    char description[128];             // "Non-Farm Payrolls"
    
    // Actual vs Expected (if available)
    double actual_value;               // Released value
    double expected_value;             // Consensus forecast
    double previous_value;             // Previous month/quarter
    bool has_values;                   // true if numbers available
    
    // Trading impact
    bool pre_event_position_reduction; // Reduce positions 30min before
    bool post_event_trading_halt;      // Stop trading 60sec after
    uint32_t volatility_window_seconds; // Expected volatility duration
    double expected_range_pips;        // Expected price movement
} live_economic_event_t;

// MAIN LIVE TRADING ENGINE
typedef struct {
    // Broker connections
    live_broker_t* primary_broker;     // Main trading account
    live_broker_t* backup_broker;      // Failover account
    bool using_backup;                 // Currently on backup broker
    
    // Market data
    live_market_data_t* current_prices; // Real-time prices for all pairs
    uint32_t subscribed_pairs;         // How many pairs we're watching
    uint64_t last_price_update_ns;     // When we last got market data
    
    // Position management
    live_position_t* positions;        // Array of current positions
    uint32_t position_count;           // Number of open positions
    uint32_t max_positions;            // Maximum allowed positions
    
    // Order management
    live_order_t* pending_orders;      // Orders waiting for execution
    uint32_t pending_order_count;      // Number of pending orders
    live_order_t* order_history;       // Completed orders (for analysis)
    uint32_t order_history_count;      // Number of historical orders
    
    // Risk management
    live_risk_manager_t risk_manager;  // Real-time risk controls
    bool risk_override_enabled;        // Manual risk override (dangerous!)
    
    // Performance tracking
    double starting_balance;           // Account balance at start
    double current_balance;            // Current account balance
    double daily_high_water_mark;      // Highest balance today
    double max_drawdown_today;         // Largest loss today
    uint32_t trades_today;             // Number of trades executed
    uint32_t winning_trades_today;     // Profitable trades today
    
    // System health
    bool system_healthy;               // Overall system status
    uint64_t last_health_check_ns;     // When we last checked system
    uint32_t error_count_today;        // System errors today
    char last_error_message[256];      // Most recent error
    
    // Strategy interface
    void* strategy_state;              // Strategy-specific data
    forex_signal_t (*generate_signal)(const live_market_data_t* data, void* state);
    bool (*should_trade)(const live_market_data_t* data, const live_risk_manager_t* risk);
} live_trading_engine_t;

// FUNCTION DECLARATIONS

// Engine lifecycle
live_trading_engine_t* live_engine_create(const char* broker_name, 
                                          const char* api_token,
                                          bool is_live_account);
void live_engine_destroy(live_trading_engine_t* engine);

// Broker integration
int live_broker_connect(live_broker_t* broker);
int live_broker_disconnect(live_broker_t* broker);
int live_broker_authenticate(live_broker_t* broker);
int live_broker_get_account_info(live_broker_t* broker);
int live_broker_subscribe_prices(live_broker_t* broker, const char* pairs[], uint32_t count);

// Order execution (REAL MONEY)
live_order_t* live_place_market_order(live_trading_engine_t* engine,
                                     uint32_t currency_pair,
                                     int64_t size,
                                     double stop_loss,
                                     double take_profit);
int live_cancel_order(live_trading_engine_t* engine, uint64_t order_id);
int live_modify_position(live_trading_engine_t* engine, const char* position_id,
                        double new_stop_loss, double new_take_profit);
int live_close_position(live_trading_engine_t* engine, const char* position_id);

// Risk management (REAL CIRCUIT BREAKERS)
bool live_check_risk_limits(live_trading_engine_t* engine, 
                           uint32_t currency_pair, int64_t size);
void live_emergency_liquidation(live_trading_engine_t* engine, const char* reason);
bool live_is_news_blackout(live_trading_engine_t* engine, uint32_t currency_pair);
void live_update_risk_limits(live_trading_engine_t* engine);

// Market data processing
int live_process_price_update(live_trading_engine_t* engine, 
                             const live_market_data_t* data);
bool live_is_market_tradeable(const live_market_data_t* data);
double live_calculate_effective_spread(const live_market_data_t* data);

// System monitoring
void live_health_check(live_trading_engine_t* engine);
void live_log_performance_metrics(live_trading_engine_t* engine);
void live_handle_connection_failure(live_trading_engine_t* engine);

// Strategy integration
void live_set_strategy(live_trading_engine_t* engine,
                      forex_signal_t (*signal_func)(const live_market_data_t*, void*),
                      bool (*should_trade_func)(const live_market_data_t*, const live_risk_manager_t*),
                      void* strategy_state);

// Main trading loop
int live_trading_main_loop(live_trading_engine_t* engine);

// OANDA-specific implementations
int oanda_connect(live_broker_t* broker);
int oanda_place_order(live_broker_t* broker, live_order_t* order);
int oanda_get_positions(live_broker_t* broker, live_position_t* positions, uint32_t* count);
int oanda_stream_prices(live_broker_t* broker, 
                       void (*callback)(const live_market_data_t*));

// Emergency functions (for 50x leverage)
void emergency_close_all_positions(live_trading_engine_t* engine);
void emergency_halt_trading(live_trading_engine_t* engine, const char* reason);
bool emergency_margin_call_active(live_trading_engine_t* engine);

#endif // LIVE_TRADING_ENGINE_H