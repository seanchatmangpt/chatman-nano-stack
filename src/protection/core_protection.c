#include "core_protection.h"
#include <string.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Performance metrics
static protection_metrics_t metrics = {0};

// Get current timestamp in microseconds
static uint64_t get_timestamp_us(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000000ULL + ts.tv_nsec / 1000;
}

// Core validation function - implements the critical 20% 
protection_result_t validate_trade_protection(core_protection_t* protection, 
                                            trade_request_t* request) {
    protection_result_t result = {
        .approved = true,
        .rejection_reason = NULL,
        .adjusted_size = request->position_size,  // Start with original
        .required_stop_loss = request->stop_loss
    };
    
    uint64_t start_time = get_timestamp_us();
    
    // 1. Emergency kill switch check (highest priority)
    if (!is_trading_allowed(protection)) {
        result.approved = false;
        result.rejection_reason = "Trading halted - kill switch active";
        metrics.violations_prevented++;
        goto done;
    }
    
    // 2. Daily loss circuit breaker check
    if (!check_daily_loss_circuit(protection)) {
        result.approved = false;
        result.rejection_reason = "Daily loss limit reached";
        metrics.violations_prevented++;
        goto done;
    }
    
    // 3. Stop loss enforcement (do this BEFORE position size check)
    if (!enforce_stop_loss(request, protection->default_stop_percent)) {
        // Force stop loss
        result.required_stop_loss = request->entry_price * 
                                   (1.0 - protection->default_stop_percent);
        if (request->stop_loss == 0) {
            request->stop_loss = result.required_stop_loss;
        }
    }
    
    // 4. Position size validation (AFTER stop loss is set)
    if (!check_position_size_limit(protection, request)) {
        // Adjust position size to comply
        double max_risk = request->account_balance * protection->max_position_risk_percent;
        double stop_distance = fabs(request->entry_price - request->stop_loss);
        
        if (stop_distance > 0) {
            result.adjusted_size = max_risk / stop_distance;
            if (result.adjusted_size < 1) {  // Minimum viable position
                result.approved = false;
                result.rejection_reason = "Position too small after risk adjustment";
                metrics.violations_prevented++;
                goto done;
            }
            request->position_size = result.adjusted_size;  // Apply adjustment
            // Keep the adjusted size in result for caller reference
        }
    }
    
done:
    metrics.validation_end = get_timestamp_us();
    metrics.response_time_us = metrics.validation_end - start_time;
    
    // Ensure we meet the 100ms response time requirement
    if (metrics.response_time_us > protection->max_response_time_ms * 1000) {
        fprintf(stderr, "WARNING: Protection validation took %u us (limit: %u ms)\n",
                metrics.response_time_us, protection->max_response_time_ms);
    }
    
    return result;
}

// Check position size limits (prevents overleveraging)
bool check_position_size_limit(core_protection_t* protection, 
                              trade_request_t* request) {
    if (!protection || !request) return false;
    
    // Calculate position risk
    double stop_distance = fabs(request->entry_price - request->stop_loss);
    
    // Prevent division by zero
    if (request->entry_price == 0) return false;
    
    double position_risk = (stop_distance / request->entry_price) * request->position_size;
    double risk_percent = position_risk / request->account_balance;
    
    // Debug print
    #ifdef DEBUG
    printf("Position risk calculation: stop_distance=%.4f, position_size=%.2f, risk=%.4f%%, current_exposure=%.2f\n",
           stop_distance, request->position_size, risk_percent * 100, protection->current_exposure);
    #endif
    
    // Check against 1% limit
    if (risk_percent > protection->max_position_risk_percent) {
        return false;
    }
    
    // Check total exposure as percentage of account
    double new_exposure_percent = (protection->current_exposure + request->position_size) / request->account_balance;
    
    #ifdef DEBUG
    printf("Exposure check: new_exposure_percent=%.4f%% (limit 5%%)\n", new_exposure_percent * 100);
    #endif
    
    if (new_exposure_percent > 0.05) {  // 5% max total exposure
        return false;
    }
    
    return true;
}

// Daily loss circuit breaker (prevents catastrophic drawdowns)
bool check_daily_loss_circuit(core_protection_t* protection) {
    if (!protection) return false;
    
    // Already halted
    if (protection->trading_halted) {
        return false;
    }
    
    // Check if daily loss exceeds limit
    if (protection->daily_pnl < 0) {
        double loss_percent = fabs(protection->daily_pnl) / 1000.0;  // Assume $1000 account
        if (loss_percent >= protection->daily_loss_limit_percent) {
            protection->trading_halted = true;
            protection->kill_switch_timestamp = get_timestamp_us();
            return false;
        }
    }
    
    return true;
}

// Enforce stop loss on all trades
bool enforce_stop_loss(trade_request_t* request, double default_stop_percent) {
    if (!request) return false;
    
    // Check if stop loss is set
    if (request->stop_loss == 0) {
        // Set default stop loss
        request->stop_loss = request->entry_price * (1.0 - default_stop_percent);
        return false;  // Had to modify
    }
    
    // Verify stop loss is reasonable
    double stop_distance = fabs(request->entry_price - request->stop_loss);
    double stop_percent = stop_distance / request->entry_price;
    
    // Stop loss too far (>5%)
    if (stop_percent > 0.05) {
        request->stop_loss = request->entry_price * (1.0 - default_stop_percent);
        return false;  // Had to modify
    }
    
    return true;
}

// Emergency kill switch activation
bool activate_kill_switch(core_protection_t* protection) {
    if (!protection) return false;
    
    protection->kill_switch_enabled = true;
    protection->kill_switch_timestamp = get_timestamp_us();
    protection->trading_halted = true;
    
    // Log critical event
    fprintf(stderr, "EMERGENCY: Kill switch activated at %llu\n", 
            protection->kill_switch_timestamp);
    
    return true;
}

// Check if trading is allowed
bool is_trading_allowed(core_protection_t* protection) {
    if (!protection) return false;
    
    // Check kill switch
    if (protection->kill_switch_enabled) {
        return false;
    }
    
    // Check circuit breaker
    if (protection->trading_halted) {
        return false;
    }
    
    return true;
}

// Update daily P&L tracking
void update_daily_pnl(core_protection_t* protection, double pnl_change) {
    if (!protection) return;
    
    protection->daily_pnl += pnl_change;
    
    // Auto-check circuit breaker
    check_daily_loss_circuit(protection);
}

// Reset daily counters (call at market open)
void reset_daily_counters(core_protection_t* protection) {
    if (!protection) return;
    
    protection->daily_pnl = 0;
    protection->trading_halted = false;
    // Don't reset kill switch - manual intervention required
}

// Get performance metrics
protection_metrics_t* get_protection_metrics(void) {
    return &metrics;
}