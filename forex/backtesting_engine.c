/*
 * FOREX BACKTESTING ENGINE: Real Historical Data Implementation
 * Leverages ALL CNS components for authentic performance validation
 */

#include "backtesting_engine.h"
#include "../bitactor/include/bitactor/bitactor.h"
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

/*
 * ENGINE LIFECYCLE MANAGEMENT
 */
backtesting_engine_t* backtest_engine_create(const backtest_config_t* config,
                                            strategy_interface_t* strategy) {
    backtesting_engine_t* engine = calloc(1, sizeof(backtesting_engine_t));
    if (!engine) return NULL;
    
    // Copy configuration
    engine->config = *config;
    engine->strategy = strategy;
    
    // Initialize account state
    engine->current_balance = config->initial_balance;
    engine->current_equity = config->initial_balance;
    
    // Initialize forex account for CNS integration
    engine->forex_account.balance = config->initial_balance;
    engine->forex_account.equity = config->initial_balance;
    engine->forex_account.leverage = config->leverage;
    engine->forex_account.margin_used = 0.0;
    engine->forex_account.margin_call = false;
    engine->forex_account.stop_out = false;
    
    // Allocate position tracking
    engine->max_position_count = config->max_positions;
    engine->positions = calloc(engine->max_position_count, sizeof(forex_position_t));
    
    // Allocate trade history
    engine->max_trades = 10000;  // Adjust based on expected trade volume
    engine->trade_history = calloc(engine->max_trades, sizeof(trade_record_t));
    
    // Allocate daily tracking (assume max 5 years of data)
    engine->daily_balances = calloc(365 * 5, sizeof(double));
    engine->daily_drawdowns = calloc(365 * 5, sizeof(double));
    
    // Initialize strategy state
    if (strategy && strategy->init_state) {
        strategy->parameters = strategy->init_state(config);
    }
    
    // Enable CNS optimizations by default
    engine->use_cns_optimizations = true;
    
    printf("✅ Backtesting engine initialized\n");
    printf("💰 Initial balance: $%.2f\n", config->initial_balance);
    printf("⚡ Leverage: %dx\n", config->leverage);
    printf("🎯 Strategy: %s\n", strategy ? strategy->name : "None");
    
    return engine;
}

void backtest_engine_destroy(backtesting_engine_t* engine) {
    if (!engine) return;
    
    // Cleanup strategy state
    if (engine->strategy && engine->strategy->cleanup_state) {
        engine->strategy->cleanup_state(engine->strategy->parameters);
    }
    
    // Free allocated memory
    free(engine->positions);
    free(engine->trade_history);
    free(engine->daily_balances);
    free(engine->daily_drawdowns);
    free(engine);
    
    printf("✅ Backtesting engine destroyed\n");
}

/*
 * HISTORICAL DATA LOADING
 */
int backtest_load_data_file(const char* file_path, historical_tick_t** ticks, 
                           uint64_t* tick_count, data_file_info_t* info) {
    FILE* file = fopen(file_path, "r");
    if (!file) {
        printf("❌ Failed to open data file: %s\n", file_path);
        return -1;
    }
    
    // Parse filename to extract symbol and date info
    const char* filename = strrchr(file_path, '/');
    if (filename) filename++; else filename = file_path;
    
    // Example: EURUSD_20240101_20240131.csv
    sscanf(filename, "%7s", info->symbol);
    
    // Count lines first to allocate memory
    uint64_t line_count = 0;
    char buffer[256];
    
    // Skip header if present
    if (fgets(buffer, sizeof(buffer), file)) {
        if (strstr(buffer, "timestamp") || strstr(buffer, "date")) {
            // Has header, don't count this line
        } else {
            line_count = 1;  // First line is data
        }
    }
    
    while (fgets(buffer, sizeof(buffer), file)) {
        line_count++;
    }
    
    rewind(file);
    
    // Skip header again if present
    if (fgets(buffer, sizeof(buffer), file)) {
        if (strstr(buffer, "timestamp") || strstr(buffer, "date")) {
            fgets(buffer, sizeof(buffer), file); // Skip header
        } else {
            rewind(file); // Go back if no header
        }
    }
    
    // Allocate memory for ticks
    *ticks = calloc(line_count, sizeof(historical_tick_t));
    if (!*ticks) {
        fclose(file);
        return -1;
    }
    
    // Parse data lines
    uint64_t count = 0;
    double bid, ask;
    long long timestamp_ms;
    
    printf("📊 Loading historical data from %s...\n", file_path);
    
    while (fgets(buffer, sizeof(buffer), file) && count < line_count) {
        // Parse CSV format: timestamp,bid,ask,bid_volume,ask_volume
        // Example: 1672531200000,1.0542,1.0543,1000000,1500000
        
        int parsed = sscanf(buffer, "%lld,%lf,%lf,%lu,%lu", 
                           &timestamp_ms, &bid, &ask,
                           &(*ticks)[count].bid_volume,
                           &(*ticks)[count].ask_volume);
        
        if (parsed >= 3) {  // At minimum need timestamp, bid, ask
            (*ticks)[count].timestamp_ns = timestamp_ms * 1000000ULL; // Convert ms to ns
            (*ticks)[count].bid = bid;
            (*ticks)[count].ask = ask;
            (*ticks)[count].currency_pair = EUR_USD; // Default, should be parsed from filename
            (*ticks)[count].tier = 0; // Tier-1 data
            (*ticks)[count].spread_points = (uint16_t)((ask - bid) * 100000); // In 0.1 pip units
            
            // Set default volumes if not provided
            if (parsed < 4) (*ticks)[count].bid_volume = 1000000;
            if (parsed < 5) (*ticks)[count].ask_volume = 1000000;
            
            count++;
        }
        
        // Progress indicator
        if (count % 100000 == 0) {
            printf("📈 Loaded %lu ticks...\n", count);
        }
    }
    
    fclose(file);
    
    *tick_count = count;
    info->total_ticks = count;
    
    if (count > 0) {
        info->start_date = (*ticks)[0].timestamp_ns / 1000000000ULL;
        info->end_date = (*ticks)[count-1].timestamp_ns / 1000000000ULL;
        
        // Calculate min/max/avg
        double min_price = (*ticks)[0].bid;
        double max_price = (*ticks)[0].ask;
        double total_spread = 0.0;
        
        for (uint64_t i = 0; i < count; i++) {
            if ((*ticks)[i].bid < min_price) min_price = (*ticks)[i].bid;
            if ((*ticks)[i].ask > max_price) max_price = (*ticks)[i].ask;
            total_spread += ((*ticks)[i].ask - (*ticks)[i].bid);
        }
        
        info->min_price = min_price;
        info->max_price = max_price;
        info->avg_spread = total_spread / count;
        strcpy(info->data_source, "CSV_File");
        strncpy(info->file_path, file_path, sizeof(info->file_path) - 1);
    }
    
    printf("✅ Loaded %lu ticks from %s\n", count, info->symbol);
    printf("📊 Price range: %.5f - %.5f\n", info->min_price, info->max_price);
    printf("📏 Average spread: %.1f pips\n", info->avg_spread * 10000);
    
    return 0;
}

/*
 * TICK PROCESSING WITH CNS INTEGRATION
 */
int backtest_process_tick(backtesting_engine_t* engine, const historical_tick_t* tick) {
    engine->ticks_processed++;
    
    // STEP 1: Apply CNS zero-tick filter if enabled
    if (engine->use_cns_optimizations && backtest_apply_zero_tick_filter(tick)) {
        return 0; // Skip processing this tick (zero-tick optimization)
    }
    
    // STEP 2: Update correlation matrix using CNS SIMD
    if (engine->use_cns_optimizations) {
        backtest_update_correlation_matrix(engine, tick);
    }
    
    // STEP 3: Update strategy state
    if (engine->strategy && engine->strategy->update_state) {
        engine->strategy->update_state(engine->strategy->parameters, tick);
    }
    
    // STEP 4: Update existing positions with current prices
    for (uint32_t i = 0; i < engine->position_count; i++) {
        forex_position_t* pos = &engine->positions[i];
        if (pos->currency_pair == tick->currency_pair) {
            double current_price = (pos->size > 0) ? tick->bid : tick->ask;
            forex_update_position(pos, current_price);
            
            // Check for stop loss or take profit
            if ((pos->size > 0 && current_price <= pos->stop_loss) ||
                (pos->size < 0 && current_price >= pos->stop_loss)) {
                backtest_close_position(engine, i, tick, "SL");
                i--; // Adjust index since position was removed
            } else if ((pos->size > 0 && current_price >= pos->take_profit) ||
                      (pos->size < 0 && current_price <= pos->take_profit)) {
                backtest_close_position(engine, i, tick, "TP");
                i--; // Adjust index since position was removed
            }
        }
    }
    
    // STEP 5: Generate trading signal
    forex_signal_t signal = {0};
    if (engine->strategy && engine->strategy->generate_signal) {
        signal = engine->strategy->generate_signal(tick, engine->strategy->parameters);
    }
    
    // STEP 6: Risk management check using CNS
    if (signal.validated && backtest_check_risk_limits(engine, &signal)) {
        // Execute trade
        if (engine->position_count < engine->max_position_count) {
            forex_position_t* new_pos = &engine->positions[engine->position_count];
            new_pos->position_id = engine->trade_count + 1;
            new_pos->currency_pair = tick->currency_pair;
            new_pos->size = (int64_t)(signal.recommended_size * 100000); // Convert lots to units
            new_pos->entry_price = (new_pos->size > 0) ? tick->ask : tick->bid;
            new_pos->current_price = new_pos->entry_price;
            new_pos->stop_loss = new_pos->entry_price - (signal.stop_distance * 0.0001);
            new_pos->take_profit = new_pos->entry_price + (signal.stop_distance * 2 * 0.0001); // 2:1 RR
            new_pos->open_time = tick->timestamp_ns;
            new_pos->margin_required = (fabs(new_pos->size) * new_pos->entry_price) / engine->config.leverage;
            new_pos->status = 1; // Open
            
            engine->position_count++;
            
            printf("📈 Opened %s position: %.2f lots at %.5f\n", 
                   new_pos->size > 0 ? "LONG" : "SHORT",
                   fabs(new_pos->size) / 100000.0, new_pos->entry_price);
        }
    }
    
    // STEP 7: Update account equity and check margin
    double total_pnl = 0.0;
    double total_margin = 0.0;
    
    for (uint32_t i = 0; i < engine->position_count; i++) {
        total_pnl += engine->positions[i].unrealized_pnl;
        total_margin += engine->positions[i].margin_required;
    }
    
    engine->current_equity = engine->current_balance + total_pnl;
    engine->forex_account.equity = engine->current_equity;
    engine->forex_account.margin_used = total_margin;
    
    // Check for margin call using CNS risk management
    forex_check_margin(&engine->forex_account);
    
    return 1; // Tick processed successfully
}

/*
 * MAIN BACKTESTING EXECUTION
 */
backtest_results_t backtest_run_period(backtesting_engine_t* engine,
                                      const char* data_directory,
                                      time_t start_date, time_t end_date,
                                      const char* currency_pairs[],
                                      uint32_t pair_count) {
    backtest_results_t results = {0};
    
    printf("🚀 Starting backtest from %s\n", data_directory);
    char date_str[32];
    printf("📅 Period: %s to %s\n", 
           format_date_string(start_date, date_str, sizeof(date_str)),
           format_date_string(end_date, date_str, sizeof(date_str)));
    
    uint64_t start_time_ns = bitactor_get_timestamp_ns();
    
    // Load and process data for each currency pair
    for (uint32_t pair_idx = 0; pair_idx < pair_count; pair_idx++) {
        char file_path[512];
        snprintf(file_path, sizeof(file_path), "%s/%s_M1.csv", 
                data_directory, currency_pairs[pair_idx]);
        
        historical_tick_t* ticks = NULL;
        uint64_t tick_count = 0;
        data_file_info_t info = {0};
        
        if (backtest_load_data_file(file_path, &ticks, &tick_count, &info) == 0) {
            printf("🔄 Processing %lu ticks for %s...\n", tick_count, currency_pairs[pair_idx]);
            
            // Filter ticks by date range
            uint64_t processed_ticks = 0;
            for (uint64_t i = 0; i < tick_count; i++) {
                time_t tick_time = ticks[i].timestamp_ns / 1000000000ULL;
                
                if (tick_time >= start_date && tick_time <= end_date) {
                    backtest_process_tick(engine, &ticks[i]);
                    processed_ticks++;
                    
                    // Progress update every 50k ticks
                    if (processed_ticks % 50000 == 0) {
                        double progress = (double)processed_ticks / tick_count * 100;
                        printf("📊 Progress: %.1f%% (%lu/%lu ticks)\n", 
                               progress, processed_ticks, tick_count);
                    }
                }
            }
            
            results.total_ticks_processed += processed_ticks;
            free(ticks);
        } else {
            printf("⚠️ Could not load data for %s\n", currency_pairs[pair_idx]);
        }
    }
    
    uint64_t end_time_ns = bitactor_get_timestamp_ns();
    
    // Close any remaining open positions
    printf("🔄 Closing remaining open positions...\n");
    while (engine->position_count > 0) {
        historical_tick_t dummy_tick = {0};
        dummy_tick.timestamp_ns = end_date * 1000000000ULL;
        backtest_close_position(engine, 0, &dummy_tick, "EOD");
    }
    
    // Calculate final results
    backtest_calculate_metrics(engine, &results);
    
    // Set timing information
    results.backtest_start = start_date;
    results.backtest_end = end_date;
    results.total_processing_time_ns = end_time_ns - start_time_ns;
    results.avg_tick_processing_ns = (double)results.total_processing_time_ns / results.total_ticks_processed;
    
    printf("\n✅ Backtest completed!\n");
    printf("⏱️ Processing time: %.2f seconds\n", results.total_processing_time_ns / 1000000000.0);
    printf("📊 Average tick processing: %.0f ns\n", results.avg_tick_processing_ns);
    
    return results;
}

/*
 * CNS INTEGRATION FUNCTIONS
 */
bool backtest_apply_zero_tick_filter(const historical_tick_t* tick) {
    // Apply zero-tick filter based on spread and price movement
    double spread = tick->ask - tick->bid;
    
    // Filter out ticks with spreads smaller than 0.5 pips (noise)
    if (spread < 0.00005) {
        return true; // Zero-tick (filtered)
    }
    
    return false; // Process this tick
}

void backtest_update_correlation_matrix(backtesting_engine_t* engine, 
                                       const historical_tick_t* tick) {
    // Update correlation matrix using CNS SIMD optimizations
    float price_change = (float)((tick->bid + tick->ask) / 2.0);
    
    if (engine->use_cns_optimizations) {
        forex_update_correlation_simd(&engine->correlation_matrix, &price_change);
    }
}

/*
 * RESULTS CALCULATION AND REPORTING
 */
void backtest_calculate_metrics(backtesting_engine_t* engine, backtest_results_t* results) {
    results->final_balance = engine->current_balance;
    results->total_return_pct = ((engine->current_balance / engine->config.initial_balance) - 1.0) * 100.0;
    results->total_trades = engine->trade_count;
    
    if (engine->trade_count == 0) {
        printf("⚠️ No trades executed during backtest period\n");
        return;
    }
    
    // Calculate win/loss statistics
    uint32_t wins = 0;
    double total_profit = 0.0;
    double total_loss = 0.0;
    double max_win = 0.0;
    double max_loss = 0.0;
    
    for (uint32_t i = 0; i < engine->trade_count; i++) {
        trade_record_t* trade = &engine->trade_history[i];
        if (trade->net_profit > 0) {
            wins++;
            total_profit += trade->net_profit;
            if (trade->net_profit > max_win) max_win = trade->net_profit;
        } else {
            total_loss += fabs(trade->net_profit);
            if (trade->net_profit < max_loss) max_loss = trade->net_profit;
        }
    }
    
    results->winning_trades = wins;
    results->losing_trades = engine->trade_count - wins;
    results->win_rate_pct = ((double)wins / engine->trade_count) * 100.0;
    results->avg_win = wins > 0 ? total_profit / wins : 0.0;
    results->avg_loss = (engine->trade_count - wins) > 0 ? total_loss / (engine->trade_count - wins) : 0.0;
    results->profit_factor = total_loss > 0 ? total_profit / total_loss : 0.0;
    results->largest_win = max_win;
    results->largest_loss = max_loss;
    
    // Calculate drawdown
    double peak_balance = engine->config.initial_balance;
    double max_dd = 0.0;
    
    for (uint32_t i = 0; i < engine->trade_count; i++) {
        double balance = engine->trade_history[i].balance_after;
        if (balance > peak_balance) {
            peak_balance = balance;
        }
        double drawdown = (peak_balance - balance) / peak_balance * 100.0;
        if (drawdown > max_dd) {
            max_dd = drawdown;
        }
    }
    
    results->max_drawdown_pct = max_dd;
    
    // Calculate Sharpe ratio (simplified)
    if (engine->trade_count > 1) {
        double* returns = calloc(engine->trade_count, sizeof(double));
        for (uint32_t i = 0; i < engine->trade_count; i++) {
            returns[i] = engine->trade_history[i].net_profit / engine->config.initial_balance;
        }
        results->sharpe_ratio = calculate_sharpe_ratio(returns, engine->trade_count, 0.02); // 2% risk-free rate
        free(returns);
    }
    
    // CNS-specific metrics
    results->zero_tick_ratio = 0.0; // Calculate from processed vs total ticks
    results->avg_tick_processing_ns = 0.0; // Set by calling function
}

void backtest_print_results(const backtest_results_t* results) {
    printf("\n📊 BACKTESTING RESULTS\n");
    printf("==============================================\n");
    
    // Performance Summary
    printf("💰 PERFORMANCE SUMMARY:\n");
    printf("   Initial Balance:     $%.2f\n", results->final_balance / (1 + results->total_return_pct/100));
    printf("   Final Balance:       $%.2f\n", results->final_balance);
    printf("   Total Return:        %.2f%%\n", results->total_return_pct);
    printf("   Max Drawdown:        %.2f%%\n", results->max_drawdown_pct);
    printf("   Sharpe Ratio:        %.2f\n", results->sharpe_ratio);
    
    // Trading Statistics
    printf("\n📈 TRADING STATISTICS:\n");
    printf("   Total Trades:        %u\n", results->total_trades);
    printf("   Winning Trades:      %u (%.1f%%)\n", results->winning_trades, results->win_rate_pct);
    printf("   Losing Trades:       %u (%.1f%%)\n", results->losing_trades, 100.0 - results->win_rate_pct);
    printf("   Average Win:         $%.2f\n", results->avg_win);
    printf("   Average Loss:        $%.2f\n", results->avg_loss);
    printf("   Profit Factor:       %.2f\n", results->profit_factor);
    printf("   Largest Win:         $%.2f\n", results->largest_win);
    printf("   Largest Loss:        $%.2f\n", results->largest_loss);
    
    // CNS Performance
    printf("\n⚡ CNS PERFORMANCE:\n");
    printf("   Ticks Processed:     %lu\n", results->total_ticks_processed);
    printf("   Avg Processing:      %.0f ns/tick\n", results->avg_tick_processing_ns);
    printf("   Zero-Tick Ratio:     %.1f%%\n", results->zero_tick_ratio);
    
    // Performance Rating
    printf("\n🎯 PERFORMANCE RATING:\n");
    if (results->total_return_pct > 20 && results->max_drawdown_pct < 10) {
        printf("   ✅ EXCELLENT: High returns with low drawdown\n");
    } else if (results->total_return_pct > 10 && results->max_drawdown_pct < 20) {
        printf("   ✅ GOOD: Solid performance\n");
    } else if (results->total_return_pct > 0) {
        printf("   ⚠️ AVERAGE: Positive but needs improvement\n");
    } else {
        printf("   ❌ POOR: Negative returns\n");
    }
}

/*
 * BUILT-IN STRATEGIES FOR TESTING
 */
typedef struct {
    int fast_period;
    int slow_period;
    double* fast_ma_buffer;
    double* slow_ma_buffer;
    uint32_t buffer_size;
    uint32_t current_index;
} ma_strategy_state_t;

forex_signal_t ma_generate_signal(const historical_tick_t* tick, void* state) {
    ma_strategy_state_t* ma_state = (ma_strategy_state_t*)state;
    forex_signal_t signal = {0};
    signal.currency_pair = tick->currency_pair;
    signal.signal_time = tick->timestamp_ns;
    signal.strategy_id = 1;
    
    if (ma_state->current_index < ma_state->slow_period) {
        return signal; // Not enough data yet
    }
    
    // Calculate current moving averages
    double fast_sum = 0.0, slow_sum = 0.0;
    for (int i = 0; i < ma_state->fast_period; i++) {
        int idx = (ma_state->current_index - i) % ma_state->buffer_size;
        fast_sum += ma_state->fast_ma_buffer[idx];
    }
    for (int i = 0; i < ma_state->slow_period; i++) {
        int idx = (ma_state->current_index - i) % ma_state->buffer_size;
        slow_sum += ma_state->slow_ma_buffer[idx];
    }
    
    double fast_ma = fast_sum / ma_state->fast_period;
    double slow_ma = slow_sum / ma_state->slow_period;
    
    // Generate signal based on MA crossover
    if (fast_ma > slow_ma) {
        signal.signal_strength = 0.8;
        signal.confidence = 0.7;
        signal.recommended_size = 0.1; // 0.1 lots
        signal.stop_distance = 20; // 20 pips
        signal.validated = true;
    } else if (fast_ma < slow_ma) {
        signal.signal_strength = -0.8;
        signal.confidence = 0.7;
        signal.recommended_size = 0.1;
        signal.stop_distance = 20;
        signal.validated = true;
    }
    
    return signal;
}

void* ma_init_state(const backtest_config_t* config) {
    ma_strategy_state_t* state = calloc(1, sizeof(ma_strategy_state_t));
    state->fast_period = 10;
    state->slow_period = 20;
    state->buffer_size = 100;
    state->fast_ma_buffer = calloc(state->buffer_size, sizeof(double));
    state->slow_ma_buffer = calloc(state->buffer_size, sizeof(double));
    return state;
}

void ma_update_state(void* state, const historical_tick_t* tick) {
    ma_strategy_state_t* ma_state = (ma_strategy_state_t*)state;
    double price = (tick->bid + tick->ask) / 2.0;
    
    uint32_t idx = ma_state->current_index % ma_state->buffer_size;
    ma_state->fast_ma_buffer[idx] = price;
    ma_state->slow_ma_buffer[idx] = price;
    ma_state->current_index++;
}

void ma_cleanup_state(void* state) {
    ma_strategy_state_t* ma_state = (ma_strategy_state_t*)state;
    free(ma_state->fast_ma_buffer);
    free(ma_state->slow_ma_buffer);
    free(ma_state);
}

strategy_interface_t* create_moving_average_strategy(int fast_period, int slow_period) {
    strategy_interface_t* strategy = calloc(1, sizeof(strategy_interface_t));
    strategy->strategy_id = 1;
    strcpy(strategy->name, "Moving Average Crossover");
    strategy->generate_signal = ma_generate_signal;
    strategy->init_state = ma_init_state;
    strategy->update_state = ma_update_state;
    strategy->cleanup_state = ma_cleanup_state;
    return strategy;
}

/*
 * UTILITY FUNCTIONS
 */
double calculate_sharpe_ratio(const double* returns, uint32_t length, double risk_free_rate) {
    if (length < 2) return 0.0;
    
    double mean_return = 0.0;
    for (uint32_t i = 0; i < length; i++) {
        mean_return += returns[i];
    }
    mean_return /= length;
    
    double variance = 0.0;
    for (uint32_t i = 0; i < length; i++) {
        variance += (returns[i] - mean_return) * (returns[i] - mean_return);
    }
    variance /= (length - 1);
    
    double std_dev = sqrt(variance);
    return std_dev > 0 ? (mean_return - risk_free_rate) / std_dev : 0.0;
}

const char* format_date_string(time_t timestamp, char* buffer, size_t size) {
    struct tm* tm_info = localtime(&timestamp);
    strftime(buffer, size, "%Y-%m-%d", tm_info);
    return buffer;
}

void backtest_close_position(backtesting_engine_t* engine, uint32_t position_index, 
                            const historical_tick_t* tick, const char* reason) {
    if (position_index >= engine->position_count) return;
    
    forex_position_t* pos = &engine->positions[position_index];
    
    // Create trade record
    trade_record_t trade = {0};
    trade.trade_id = engine->trade_count + 1;
    trade.currency_pair = pos->currency_pair;
    trade.open_time = pos->open_time / 1000000000ULL; // Convert to seconds
    trade.close_time = tick->timestamp_ns / 1000000000ULL;
    trade.entry_price = pos->entry_price;
    trade.exit_price = pos->current_price;
    trade.position_size = pos->size;
    trade.commission = engine->config.commission_per_lot * (fabs(pos->size) / 100000.0);
    trade.net_profit = pos->unrealized_pnl - trade.commission;
    trade.balance_after = engine->current_balance + trade.net_profit;
    strcpy(trade.close_reason, reason);
    trade.duration_minutes = (trade.close_time - trade.open_time) / 60;
    
    // Update balance
    engine->current_balance = trade.balance_after;
    
    // Store trade record
    if (engine->trade_count < engine->max_trades) {
        engine->trade_history[engine->trade_count] = trade;
        engine->trade_count++;
    }
    
    printf("🔄 Closed %s position: %.2f lots, P&L: $%.2f (%s)\n",
           pos->size > 0 ? "LONG" : "SHORT",
           fabs(pos->size) / 100000.0, trade.net_profit, reason);
    
    // Remove position from array
    for (uint32_t i = position_index; i < engine->position_count - 1; i++) {
        engine->positions[i] = engine->positions[i + 1];
    }
    engine->position_count--;
}

bool backtest_check_risk_limits(backtesting_engine_t* engine, const forex_signal_t* signal) {
    // Check if we're within risk limits
    double position_risk = signal->recommended_size * 100000 * signal->stop_distance * 0.0001;
    double max_risk = engine->current_balance * (engine->config.max_risk_per_trade / 100.0);
    
    return position_risk <= max_risk;
}