# 📊 CNS Forex Backtesting Guide: Real Historical Data Testing

## 🎯 Overview

The CNS Forex Backtesting Engine provides comprehensive historical data testing with all CNS optimizations enabled. Test your strategies against real market conditions with microsecond precision.

---

## 🚀 Quick Start

### Build the Backtesting System
```bash
cd /Users/sac/cns/forex
make clean && make all
```

### Run Demo Backtests
```bash
# Simple moving average strategy
./backtest_demo_app --simple

# Multi-currency pair testing
./backtest_demo_app --multipair

# High leverage stress test (50x)
./backtest_demo_app --stress

# CNS performance comparison
./backtest_demo_app --performance

# Or run all tests
make backtest
```

---

## 📁 Data Format Requirements

### CSV File Format
```csv
timestamp,bid,ask,bid_volume,ask_volume
1672531200000,1.0542,1.0543,1000000,1500000
1672531260000,1.0541,1.0544,1200000,1100000
...
```

### File Naming Convention
```
SYMBOL_TIMEFRAME.csv
Examples:
- EURUSD_M1.csv (EUR/USD 1-minute data)
- GBPUSD_H1.csv (GBP/USD 1-hour data)
- USDJPY_D1.csv (USD/JPY daily data)
```

### Data Sources
- **DukasCopy**: High-quality tick data
- **TrueFX**: Free historical data
- **FXCM**: Historical data via API
- **Interactive Brokers**: TWS historical data
- **MetaTrader**: Export from MT4/MT5

---

## 🛠️ Backtesting Configuration

### Basic Configuration
```c
backtest_config_t config = {
    .initial_balance = 10000.0,        // Starting capital
    .leverage = 50,                    // 50x leverage
    .commission_per_lot = 7.0,         // $7 per standard lot
    .swap_long_rate = -0.5,            // Daily swap for long positions
    .swap_short_rate = 0.2,            // Daily swap for short positions
    .max_positions = 5,                // Maximum concurrent positions
    .max_risk_per_trade = 2.0,         // 2% risk per trade
    .use_zero_tick_filter = true,      // Enable CNS optimization
    .simulate_slippage = true,         // Realistic slippage simulation
    .slippage_points = 0.5             // Average 0.5 pip slippage
};
```

### Advanced Risk Management
```c
backtest_config_t advanced_config = {
    .initial_balance = 50000.0,
    .leverage = 25,                    // Lower leverage for safety
    .max_positions = 10,
    .max_risk_per_trade = 1.0,         // 1% risk per trade
    .commission_per_lot = 5.0,
    .use_zero_tick_filter = true,
    .simulate_slippage = true,
    .slippage_points = 1.0
};
```

---

## 📈 Built-in Strategies

### 1. Moving Average Crossover
```c
strategy_interface_t* strategy = create_moving_average_strategy(10, 20);
// Fast MA: 10 periods, Slow MA: 20 periods
// Signal: Buy when fast > slow, Sell when fast < slow
```

### 2. Mean Reversion
```c
strategy_interface_t* strategy = create_mean_reversion_strategy(2.0);
// Signal when price deviates 2 standard deviations from mean
```

### 3. Breakout Strategy
```c
strategy_interface_t* strategy = create_breakout_strategy(20);
// Trade breakouts from 20-period high/low
```

### 4. News Trading
```c
strategy_interface_t* strategy = create_news_trading_strategy();
// Trade momentum around economic announcements
```

---

## 📊 Results Analysis

### Key Performance Metrics

#### Profitability
- **Total Return**: Overall percentage gain/loss
- **Profit Factor**: Gross profit / Gross loss
- **Win Rate**: Percentage of winning trades
- **Average Win/Loss**: Average profit per winning/losing trade

#### Risk Assessment
- **Maximum Drawdown**: Largest peak-to-trough decline
- **Sharpe Ratio**: Risk-adjusted returns
- **Sortino Ratio**: Downside risk-adjusted returns
- **Value at Risk (VaR)**: Potential loss at 95% confidence

#### CNS Performance
- **Zero-Tick Ratio**: Percentage of ticks filtered by CNS
- **Processing Speed**: Average nanoseconds per tick
- **Memory Efficiency**: Peak memory usage
- **SIMD Utilization**: Correlation matrix update speed

### Sample Results Output
```
📊 BACKTESTING RESULTS
==============================================

💰 PERFORMANCE SUMMARY:
   Initial Balance:     $10,000.00
   Final Balance:       $12,450.75
   Total Return:        24.51%
   Max Drawdown:        8.32%
   Sharpe Ratio:        1.85

📈 TRADING STATISTICS:
   Total Trades:        127
   Winning Trades:      89 (70.1%)
   Losing Trades:       38 (29.9%)
   Average Win:         $87.43
   Average Loss:        $42.18
   Profit Factor:       2.07
   Largest Win:         $245.67
   Largest Loss:        $89.23

⚡ CNS PERFORMANCE:
   Ticks Processed:     1,247,856
   Avg Processing:      156 ns/tick
   Zero-Tick Ratio:     78.3%

🎯 PERFORMANCE RATING:
   ✅ EXCELLENT: High returns with low drawdown
```

---

## 🔍 Advanced Analysis Features

### Equity Curve Analysis
```bash
# Generate equity curve CSV
./backtest_demo_app --simple
# Creates: demo_equity_curve.csv

# Plot with Python/Excel/R
python plot_equity_curve.py demo_equity_curve.csv
```

### Trade-by-Trade Analysis
```bash
# Generate detailed trade log
# Creates: demo_trades.csv with columns:
# trade_id, currency_pair, open_time, close_time, entry_price, 
# exit_price, position_size, commission, swap, net_profit, 
# balance_after, close_reason, duration_minutes
```

### Monte Carlo Simulation
```c
// Run multiple backtests with randomized parameters
for (int i = 0; i < 1000; i++) {
    // Randomize start date, strategy parameters, market conditions
    backtest_results_t result = backtest_run_period(...);
    // Collect statistics for distribution analysis
}
```

---

## 🎯 Strategy Development Workflow

### 1. Strategy Interface Implementation
```c
typedef struct {
    // Strategy-specific parameters
    int period1;
    int period2;
    double threshold;
    double* price_buffer;
    uint32_t buffer_index;
} my_strategy_state_t;

forex_signal_t my_generate_signal(const historical_tick_t* tick, void* state) {
    my_strategy_state_t* s = (my_strategy_state_t*)state;
    forex_signal_t signal = {0};
    
    // Implement your strategy logic here
    double current_price = (tick->bid + tick->ask) / 2.0;
    
    // Example: Simple momentum strategy
    if (/* your condition */) {
        signal.signal_strength = 0.8;
        signal.confidence = 0.9;
        signal.recommended_size = 0.1; // 0.1 lots
        signal.stop_distance = 20;     // 20 pips
        signal.validated = true;
    }
    
    return signal;
}

void* my_init_state(const backtest_config_t* config) {
    my_strategy_state_t* state = calloc(1, sizeof(my_strategy_state_t));
    state->period1 = 10;
    state->period2 = 20;
    state->threshold = 2.0;
    state->price_buffer = calloc(100, sizeof(double));
    return state;
}

void my_update_state(void* state, const historical_tick_t* tick) {
    my_strategy_state_t* s = (my_strategy_state_t*)state;
    double price = (tick->bid + tick->ask) / 2.0;
    
    // Update your indicators/buffers
    s->price_buffer[s->buffer_index % 100] = price;
    s->buffer_index++;
}

void my_cleanup_state(void* state) {
    my_strategy_state_t* s = (my_strategy_state_t*)state;
    free(s->price_buffer);
    free(s);
}

strategy_interface_t* create_my_strategy(void) {
    strategy_interface_t* strategy = calloc(1, sizeof(strategy_interface_t));
    strategy->strategy_id = 999;
    strcpy(strategy->name, "My Custom Strategy");
    strategy->generate_signal = my_generate_signal;
    strategy->init_state = my_init_state;
    strategy->update_state = my_update_state;
    strategy->cleanup_state = my_cleanup_state;
    return strategy;
}
```

### 2. Parameter Optimization
```c
// Grid search for optimal parameters
double best_return = -999.0;
int best_fast = 0, best_slow = 0;

for (int fast = 5; fast <= 20; fast += 5) {
    for (int slow = 20; slow <= 50; slow += 10) {
        strategy_interface_t* strategy = create_moving_average_strategy(fast, slow);
        backtesting_engine_t* engine = backtest_engine_create(&config, strategy);
        
        backtest_results_t results = backtest_run_period(engine, data_dir, 
                                                        start_date, end_date,
                                                        pairs, pair_count);
        
        if (results.total_return_pct > best_return) {
            best_return = results.total_return_pct;
            best_fast = fast;
            best_slow = slow;
        }
        
        backtest_engine_destroy(engine);
        free(strategy);
    }
}

printf("Optimal parameters: Fast=%d, Slow=%d, Return=%.2f%%\n",
       best_fast, best_slow, best_return);
```

---

## 🛡️ Risk Management Features

### Position Sizing
```c
// Kelly Criterion implementation
double kelly_fraction = calculate_kelly_fraction(win_rate, avg_win, avg_loss);
double position_size = balance * kelly_fraction / leverage;

// Fixed fractional method
double risk_per_trade = 0.02; // 2%
double stop_loss_distance = 20; // 20 pips
double position_size = (balance * risk_per_trade) / (stop_loss_distance * pip_value);
```

### Portfolio Risk Management
```c
// Correlation-based position limits
double correlation = get_pair_correlation(EUR_USD, GBP_USD);
if (correlation > 0.8) {
    // Reduce position size for highly correlated pairs
    position_size *= 0.5;
}

// Maximum exposure per currency
double usd_exposure = calculate_currency_exposure(USD);
if (usd_exposure > 0.5) { // 50% max exposure to USD
    // Reject new USD trades
    return false;
}
```

### Dynamic Stop Loss
```c
// Volatility-based stops
double atr = calculate_atr(price_data, 14); // 14-period ATR
double stop_distance = atr * 2.0; // 2x ATR stop

// Trailing stops
if (unrealized_profit > (stop_distance * 2)) {
    // Move stop to breakeven + 1 ATR
    new_stop = entry_price + (atr * (position_size > 0 ? 1 : -1));
}
```

---

## 🚀 Advanced Features

### Walk-Forward Analysis
```c
// Test strategy on rolling windows
time_t window_size = 365 * 24 * 3600; // 1 year
time_t step_size = 30 * 24 * 3600;    // 1 month

for (time_t start = initial_date; start < final_date; start += step_size) {
    time_t end = start + window_size;
    
    // Optimize parameters on first 80% of window
    time_t optimize_end = start + (window_size * 0.8);
    optimize_strategy_parameters(start, optimize_end);
    
    // Test on remaining 20%
    backtest_results_t results = backtest_run_period(engine, data_dir,
                                                    optimize_end, end,
                                                    pairs, pair_count);
    
    store_walk_forward_result(results);
}
```

### Multi-Timeframe Analysis
```c
// Combine M1, M5, H1, D1 signals
forex_signal_t m1_signal = strategy_m1->generate_signal(tick_m1, state_m1);
forex_signal_t m5_signal = strategy_m5->generate_signal(tick_m5, state_m5);
forex_signal_t h1_signal = strategy_h1->generate_signal(tick_h1, state_h1);

// Confluence scoring
double signal_strength = 0.0;
if (m1_signal.signal_strength > 0.7) signal_strength += 0.3;
if (m5_signal.signal_strength > 0.7) signal_strength += 0.4;
if (h1_signal.signal_strength > 0.7) signal_strength += 0.3;

final_signal.validated = (signal_strength >= 0.7);
```

### Market Regime Detection
```c
typedef enum {
    TRENDING_UP,
    TRENDING_DOWN,
    RANGING,
    HIGH_VOLATILITY,
    LOW_VOLATILITY
} market_regime_t;

market_regime_t detect_market_regime(const historical_tick_t* ticks, uint32_t count) {
    double volatility = calculate_volatility(ticks, count);
    double trend_strength = calculate_trend_strength(ticks, count);
    
    if (volatility > high_vol_threshold) return HIGH_VOLATILITY;
    if (volatility < low_vol_threshold) return LOW_VOLATILITY;
    if (trend_strength > 0.7) return TRENDING_UP;
    if (trend_strength < -0.7) return TRENDING_DOWN;
    return RANGING;
}

// Adapt strategy based on market regime
forex_signal_t generate_adaptive_signal(const historical_tick_t* tick, void* state) {
    market_regime_t regime = detect_market_regime(recent_ticks, 100);
    
    switch (regime) {
        case TRENDING_UP:
        case TRENDING_DOWN:
            return trend_following_signal(tick, state);
        case RANGING:
            return mean_reversion_signal(tick, state);
        case HIGH_VOLATILITY:
            return breakout_signal(tick, state);
        case LOW_VOLATILITY:
            return carry_trade_signal(tick, state);
    }
}
```

---

## 📊 Performance Optimization

### CNS Integration Benefits
1. **Zero-Tick Filtering**: 60-80% CPU cycle reduction
2. **SIMD Correlation Matrix**: Real-time multi-pair analysis
3. **Parallel Processing**: Multiple currency pairs simultaneously
4. **Memory Optimization**: Efficient tick data handling

### Backtesting Speed Optimization
```c
// Enable all CNS optimizations
engine->use_cns_optimizations = true;
config.use_zero_tick_filter = true;

// Pre-allocate buffers
engine->max_trades = expected_trade_count * 1.2;
engine->daily_count = test_duration_days + 10;

// Batch data loading
load_multiple_pairs_parallel(data_directory, currency_pairs, pair_count);
```

### Memory Management
```c
// Stream large datasets
typedef struct {
    FILE* file;
    historical_tick_t buffer[10000];
    uint32_t buffer_pos;
    uint32_t buffer_size;
} tick_stream_t;

bool stream_next_tick(tick_stream_t* stream, historical_tick_t* tick) {
    if (stream->buffer_pos >= stream->buffer_size) {
        // Reload buffer from file
        stream->buffer_size = load_tick_batch(stream->file, stream->buffer, 10000);
        stream->buffer_pos = 0;
        
        if (stream->buffer_size == 0) return false; // EOF
    }
    
    *tick = stream->buffer[stream->buffer_pos++];
    return true;
}
```

---

## 📝 Best Practices

### Data Quality
1. **Validate timestamps**: Ensure chronological order
2. **Check for gaps**: Fill or flag missing data
3. **Remove outliers**: Filter obvious data errors
4. **Verify spreads**: Realistic bid/ask spreads
5. **Volume consistency**: Reasonable volume levels

### Strategy Testing
1. **Out-of-sample testing**: Reserve 20-30% for final validation
2. **Multiple time periods**: Test across different market conditions
3. **Currency diversification**: Test on multiple pairs
4. **Transaction costs**: Include realistic spreads and commissions
5. **Slippage modeling**: Account for execution delays

### Risk Management
1. **Position sizing**: Never risk more than 2% per trade
2. **Stop losses**: Always use protective stops
3. **Correlation limits**: Avoid over-concentration
4. **Drawdown limits**: Set maximum acceptable drawdown
5. **Leverage limits**: Start conservative, increase gradually

---

## 🎯 Getting Started Checklist

- [ ] Build backtesting system: `make clean && make all`
- [ ] Run demo: `./backtest_demo_app --simple`
- [ ] Verify CNS optimizations: `./backtest_demo_app --performance`
- [ ] Test high leverage: `./backtest_demo_app --stress`
- [ ] Download real historical data
- [ ] Implement custom strategy
- [ ] Run parameter optimization
- [ ] Perform walk-forward analysis
- [ ] Deploy to live paper trading

---

**Ready to backtest your strategies against real historical data with microsecond precision and CNS optimizations!**