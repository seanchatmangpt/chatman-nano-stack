#include "enhanced_protection.h"
#include <string.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <pthread.h>

// Global enhanced metrics
static enhanced_metrics_t enhanced_metrics = {0};

// Mutex for position list operations (minimal locking)
static pthread_mutex_t position_mutex = PTHREAD_MUTEX_INITIALIZER;

// Currency correlation matrix (simplified for major pairs)
static const double CURRENCY_CORRELATION[8][8] = {
    // USD, EUR, GBP, JPY, AUD, CAD, CHF, NZD
    {1.00, -0.75, -0.65, 0.25, -0.70, -0.80, 0.85, -0.68}, // USD
    {-0.75, 1.00, 0.80, -0.15, 0.65, 0.60, -0.70, 0.62},   // EUR
    {-0.65, 0.80, 1.00, -0.10, 0.55, 0.45, -0.60, 0.50},   // GBP
    {0.25, -0.15, -0.10, 1.00, -0.20, -0.25, 0.30, -0.18}, // JPY
    {-0.70, 0.65, 0.55, -0.20, 1.00, 0.75, -0.60, 0.85},   // AUD
    {-0.80, 0.60, 0.45, -0.25, 0.75, 1.00, -0.70, 0.70},   // CAD
    {0.85, -0.70, -0.60, 0.30, -0.60, -0.70, 1.00, -0.58}, // CHF
    {-0.68, 0.62, 0.50, -0.18, 0.85, 0.70, -0.58, 1.00},   // NZD
};

// Currency code to index mapping
static int get_currency_index(const char* currency) {
    if (strncmp(currency, "USD", 3) == 0) return 0;
    if (strncmp(currency, "EUR", 3) == 0) return 1;
    if (strncmp(currency, "GBP", 3) == 0) return 2;
    if (strncmp(currency, "JPY", 3) == 0) return 3;
    if (strncmp(currency, "AUD", 3) == 0) return 4;
    if (strncmp(currency, "CAD", 3) == 0) return 5;
    if (strncmp(currency, "CHF", 3) == 0) return 6;
    if (strncmp(currency, "NZD", 3) == 0) return 7;
    return -1; // Unknown currency
}

// Get current timestamp in microseconds
static uint64_t get_timestamp_us(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000ULL + ts.tv_nsec / 1000;
}

// Main enhanced validation function
enhanced_result_t validate_trade_enhanced(enhanced_protection_t* protection, 
                                        trade_request_t* request) {
    enhanced_result_t result = {0};
    
    atomic_store(&enhanced_metrics.validation_start, get_timestamp_us());
    
    // 1. Run base protection validation first
    result.base = validate_trade_protection(&protection->core, request);
    
    // If base validation failed, return early
    if (!result.base.approved) {
        atomic_fetch_add(&enhanced_metrics.violations_prevented, 1);
        goto done;
    }
    
    // 2. Check for volatility spikes (prevents flash crash attacks)
    if (detect_price_volatility(protection, request->entry_price, request->symbol)) {
        result.base.approved = false;
        result.base.rejection_reason = "High volatility detected - trading halted";
        result.recommended_halt = HALT_VOLATILITY_SPIKE;
        result.security_warning = "Flash crash protection activated";
        
        // Trigger enhanced circuit breaker
        trigger_enhanced_circuit_breaker(protection, HALT_VOLATILITY_SPIKE);
        atomic_fetch_add(&enhanced_metrics.volatility_detections, 1);
        goto done;
    }
    
    // 3. Check position correlation to prevent manipulation
    if (!check_position_correlation(protection, request)) {
        result.base.approved = false;
        result.base.rejection_reason = "Excessive correlated exposure detected";
        result.recommended_halt = HALT_CORRELATION_BREACH;
        result.security_warning = "Position manipulation attempt blocked";
        
        atomic_fetch_add(&enhanced_metrics.correlation_blocks, 1);
        goto done;
    }
    
    // 4. Detect manipulation attempts
    if (detect_manipulation_attempt(protection, request)) {
        result.base.approved = false;
        result.base.rejection_reason = "Suspicious trading pattern detected";
        result.recommended_halt = HALT_MANIPULATION_DETECTED;
        result.security_warning = "Potential manipulation blocked";
        
        atomic_fetch_add(&enhanced_metrics.manipulation_blocks, 1);
        goto done;
    }
    
    // 5. Check if trading is halted by enhanced circuit breaker
    if (is_trading_halted(protection)) {
        result.base.approved = false;
        result.base.rejection_reason = "Trading halted by enhanced protection";
        goto done;
    }
    
    // 6. If approved, add to position tracking
    if (result.base.approved) {
        add_position_tracking(protection, request);
    }
    
done:
    atomic_store(&enhanced_metrics.validation_end, get_timestamp_us());
    uint64_t response_time = atomic_load(&enhanced_metrics.validation_end) - 
                            atomic_load(&enhanced_metrics.validation_start);
    atomic_store(&enhanced_metrics.response_time_us, (uint32_t)response_time);
    return result;
}

// Volatility detection (fixes flash crash vulnerability)
bool detect_price_volatility(enhanced_protection_t* protection, 
                           double current_price, const char* symbol) {
    // Update price history
    update_price_history(protection, current_price, symbol);
    
    // Calculate volatility over recent window
    double volatility = calculate_volatility_score(protection->price_history, 
                                                  protection->price_history_index);
    
    // Check against threshold
    if (volatility > protection->volatility_threshold_percent) {
        atomic_store(&protection->high_volatility_detected, true);
        atomic_fetch_add(&protection->volatility_events, 1);
        
        printf("⚠️ VOLATILITY ALERT: %.2f%% spike detected in %s (threshold: %.2f%%)\n",
               volatility * 100, symbol, protection->volatility_threshold_percent * 100);
        return true;
    }
    
    return false;
}

void update_price_history(enhanced_protection_t* protection, 
                         double price, const char* symbol) {
    // Simple circular buffer for price history
    int index = protection->price_history_index % 120;
    protection->price_history[index] = price;
    protection->price_history_index++;
}

double calculate_volatility_score(const double* prices, int count) {
    if (count < 2) return 0.0;
    
    // Calculate recent volatility (last 10 data points)
    int recent_count = count > 10 ? 10 : count;
    int start_idx = count - recent_count;
    
    double sum = 0.0;
    double sum_squared = 0.0;
    
    for (int i = start_idx; i < count; i++) {
        sum += prices[i % 120];
        sum_squared += prices[i % 120] * prices[i % 120];
    }
    
    double mean = sum / recent_count;
    double variance = (sum_squared / recent_count) - (mean * mean);
    double std_dev = sqrt(variance);
    
    // Return volatility as percentage of mean
    return std_dev / mean;
}

// Position correlation checking (fixes manipulation vulnerability)
bool check_position_correlation(enhanced_protection_t* protection, 
                              trade_request_t* request) {
    // Extract currencies from symbol (e.g., "EURUSD" -> EUR, USD)
    char base_currency[4] = {0};
    char quote_currency[4] = {0};
    strncpy(base_currency, request->symbol, 3);
    strncpy(quote_currency, request->symbol + 3, 3);
    
    int base_idx = get_currency_index(base_currency);
    int quote_idx = get_currency_index(quote_currency);
    
    if (base_idx == -1 || quote_idx == -1) {
        // Unknown currency - be conservative
        return true;
    }
    
    // Check current exposure for both currencies
    double base_exposure = get_currency_exposure(protection, base_currency);
    double quote_exposure = get_currency_exposure(protection, quote_currency);
    
    // Calculate correlation risk
    double new_base_exposure = base_exposure + request->position_size;
    double new_quote_exposure = quote_exposure + request->position_size;
    
    // Check if any currency exposure exceeds limits
    double account_balance = request->account_balance;
    if (new_base_exposure > account_balance * 0.15) {  // 15% max per currency
        return false;
    }
    if (new_quote_exposure > account_balance * 0.15) {
        return false;
    }
    
    // Check correlation between existing positions
    pthread_mutex_lock(&position_mutex);
    position_entry_t* pos = protection->positions_head;
    double total_correlation_risk = 0.0;
    
    while (pos != NULL) {
        // Calculate correlation with existing position
        char pos_base[4] = {0};
        char pos_quote[4] = {0};
        strncpy(pos_base, pos->symbol, 3);
        strncpy(pos_quote, pos->symbol + 3, 3);
        
        int pos_base_idx = get_currency_index(pos_base);
        int pos_quote_idx = get_currency_index(pos_quote);
        
        if (pos_base_idx != -1 && base_idx != -1) {
            double correlation = CURRENCY_CORRELATION[base_idx][pos_base_idx];
            total_correlation_risk += fabs(correlation) * pos->notional_size;
        }
        
        pos = pos->next;
    }
    pthread_mutex_unlock(&position_mutex);
    
    // If total correlation risk is too high, reject
    if (total_correlation_risk > account_balance * 0.25) {  // 25% correlation limit
        return false;
    }
    
    return true;
}

void add_position_tracking(enhanced_protection_t* protection, 
                         trade_request_t* request) {
    position_entry_t* new_pos = malloc(sizeof(position_entry_t));
    if (!new_pos) return;
    
    strncpy(new_pos->symbol, request->symbol, sizeof(new_pos->symbol) - 1);
    new_pos->notional_size = request->position_size;
    new_pos->timestamp = get_timestamp_us();
    new_pos->correlation_factor = 1.0; // Default
    
    pthread_mutex_lock(&position_mutex);
    new_pos->next = protection->positions_head;
    protection->positions_head = new_pos;
    atomic_fetch_add(&protection->position_count, 1);
    pthread_mutex_unlock(&position_mutex);
}

void remove_position_tracking(enhanced_protection_t* protection, 
                            const char* symbol) {
    pthread_mutex_lock(&position_mutex);
    position_entry_t** current = &protection->positions_head;
    
    while (*current != NULL) {
        if (strcmp((*current)->symbol, symbol) == 0) {
            position_entry_t* to_remove = *current;
            *current = (*current)->next;
            free(to_remove);
            atomic_fetch_sub(&protection->position_count, 1);
            break;
        }
        current = &(*current)->next;
    }
    pthread_mutex_unlock(&position_mutex);
}

double get_currency_exposure(enhanced_protection_t* protection, 
                           const char* currency) {
    double total_exposure = 0.0;
    
    pthread_mutex_lock(&position_mutex);
    position_entry_t* pos = protection->positions_head;
    
    while (pos != NULL) {
        // Check if position involves this currency
        if (strncmp(pos->symbol, currency, 3) == 0 || 
            strncmp(pos->symbol + 3, currency, 3) == 0) {
            total_exposure += pos->notional_size;
        }
        pos = pos->next;
    }
    pthread_mutex_unlock(&position_mutex);
    
    return total_exposure;
}

// Enhanced circuit breaker (fixes race condition)
bool trigger_enhanced_circuit_breaker(enhanced_protection_t* protection, 
                                     halt_reason_t reason) {
    atomic_store(&protection->circuit_breaker_active, true);
    atomic_store(&protection->circuit_trigger_time, get_timestamp_us());
    atomic_store(&protection->halt_reason, reason);
    
    const char* reason_str = "Unknown";
    switch (reason) {
        case HALT_DAILY_LOSS: reason_str = "Daily Loss Limit"; break;
        case HALT_VOLATILITY_SPIKE: reason_str = "Volatility Spike"; break;
        case HALT_CORRELATION_BREACH: reason_str = "Correlation Breach"; break;
        case HALT_KILL_SWITCH: reason_str = "Kill Switch"; break;
        case HALT_MANIPULATION_DETECTED: reason_str = "Manipulation Detected"; break;
        default: break;
    }
    
    printf("🚨 ENHANCED CIRCUIT BREAKER: %s (Reason: %s)\n", 
           "Trading Halted", reason_str);
    
    return true;
}

bool is_trading_halted(enhanced_protection_t* protection) {
    return atomic_load(&protection->circuit_breaker_active) || 
           atomic_load(&protection->high_volatility_detected);
}

void reset_circuit_breaker(enhanced_protection_t* protection) {
    atomic_store(&protection->circuit_breaker_active, false);
    atomic_store(&protection->high_volatility_detected, false);
    atomic_store(&protection->halt_reason, HALT_NONE);
    
    printf("✅ Enhanced circuit breaker reset - trading resumed\n");
}

// Security functions to prevent manipulation
bool detect_manipulation_attempt(enhanced_protection_t* protection, 
                               trade_request_t* request) {
    // Check for suspicious patterns
    if (check_suspicious_patterns(protection, request)) {
        return true;
    }
    
    // Check request timing
    if (!validate_request_timing(protection, request)) {
        return true;
    }
    
    // Check for rapid-fire requests (sign of bot attack)
    uint64_t current_time = get_timestamp_us();
    uint64_t last_time = atomic_load(&protection->last_request_time);
    
    if (last_time != 0 && current_time - last_time < 1000) {  // Less than 1ms apart
        atomic_fetch_add(&protection->manipulation_attempts, 1);
        return true;
    }
    
    atomic_store(&protection->last_request_time, current_time);
    return false;
}

bool validate_request_timing(enhanced_protection_t* protection, 
                           trade_request_t* request) {
    // Check if request timestamp is reasonable
    uint64_t current_time = time(NULL);
    uint64_t request_time = request->timestamp;
    
    // Allow 5 seconds of clock drift
    if (llabs((int64_t)(current_time - request_time)) > 5) {
        return false;  // Timestamp too far off
    }
    
    return true;
}

bool check_suspicious_patterns(enhanced_protection_t* protection, 
                             trade_request_t* request) {
    // Pattern 1: Identical position sizes (sign of scripted trading)
    pthread_mutex_lock(&position_mutex);
    position_entry_t* pos = protection->positions_head;
    int identical_size_count = 0;
    
    while (pos != NULL) {
        if (fabs(pos->notional_size - request->position_size) < 0.01) {
            identical_size_count++;
        }
        pos = pos->next;
    }
    pthread_mutex_unlock(&position_mutex);
    
    if (identical_size_count > 5) {  // More than 5 identical sizes
        return true;
    }
    
    // Pattern 2: Unrealistic stop loss (too tight)
    double stop_distance = fabs(request->entry_price - request->stop_loss);
    double stop_percent = stop_distance / request->entry_price;
    
    if (stop_percent < 0.001) {  // Less than 0.1% stop
        return true;  // Unrealistic
    }
    
    return false;
}

// Initialization and cleanup
enhanced_protection_t* create_enhanced_protection(void) {
    enhanced_protection_t* protection = malloc(sizeof(enhanced_protection_t));
    if (!protection) return NULL;
    
    // Initialize core protection
    protection->core.max_position_risk_percent = 0.01;
    protection->core.daily_loss_limit_percent = 0.02;
    protection->core.current_exposure = 0;
    protection->core.daily_pnl = 0;
    protection->core.trading_halted = false;
    protection->core.require_stop_loss = true;
    protection->core.default_stop_percent = 0.02;
    protection->core.kill_switch_enabled = false;
    protection->core.kill_switch_timestamp = 0;
    protection->core.max_response_time_ms = 50;  // Tighter requirement
    
    // Initialize enhanced features
    protection->volatility_threshold_percent = 0.02;  // 2% volatility threshold
    protection->volatility_window_ms = 60000;         // 60 second window
    protection->price_history_index = 0;
    atomic_init(&protection->high_volatility_detected, false);
    
    protection->positions_head = NULL;
    atomic_init(&protection->position_count, 0);
    
    atomic_init(&protection->circuit_breaker_active, false);
    atomic_init(&protection->circuit_trigger_time, 0);
    atomic_init(&protection->halt_reason, HALT_NONE);
    
    atomic_init(&protection->volatility_events, 0);
    atomic_init(&protection->correlation_violations, 0);
    atomic_init(&protection->manipulation_attempts, 0);
    atomic_init(&protection->last_request_time, 0);
    
    // Clear price history
    memset(protection->price_history, 0, sizeof(protection->price_history));
    memset(protection->correlated_exposure, 0, sizeof(protection->correlated_exposure));
    
    return protection;
}

void destroy_enhanced_protection(enhanced_protection_t* protection) {
    if (!protection) return;
    
    // Clean up position list
    pthread_mutex_lock(&position_mutex);
    position_entry_t* pos = protection->positions_head;
    while (pos != NULL) {
        position_entry_t* next = pos->next;
        free(pos);
        pos = next;
    }
    pthread_mutex_unlock(&position_mutex);
    
    free(protection);
}

// Get enhanced metrics
enhanced_metrics_t* get_enhanced_metrics(void) {
    return &enhanced_metrics;
}