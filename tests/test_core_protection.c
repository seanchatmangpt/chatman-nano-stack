#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "../src/protection/core_protection.h"

// Test framework
#define TEST(name) void test_##name()
#define RUN_TEST(name) do { \
    printf("Running test: %s\n", #name); \
    test_##name(); \
    printf("✅ PASSED: %s\n", #name); \
} while(0)

// Initialize test protection system
core_protection_t* create_test_protection() {
    core_protection_t* protection = malloc(sizeof(core_protection_t));
    protection->max_position_risk_percent = 0.01;      // 1% max risk
    protection->daily_loss_limit_percent = 0.02;       // 2% daily limit
    protection->current_exposure = 0;
    protection->daily_pnl = 0;
    protection->trading_halted = false;
    protection->require_stop_loss = true;
    protection->default_stop_percent = 0.02;           // 2% default stop
    protection->kill_switch_enabled = false;
    protection->kill_switch_timestamp = 0;
    protection->max_response_time_ms = 100;            // 100ms max
    return protection;
}

// Test 1: Position size limits prevent overleveraging
TEST(position_size_limits) {
    core_protection_t* protection = create_test_protection();
    
    // Test case 1: Valid position size (0.5% risk)
    trade_request_t valid_trade = {
        .symbol = "EURUSD",
        .position_size = 50,     // 5% of account (reasonable for forex)
        .entry_price = 1.1000,
        .stop_loss = 1.0890,    // 1% stop
        .account_balance = 1000,
        .timestamp = 0
    };
    
    assert(check_position_size_limit(protection, &valid_trade) == true);
    
    // Test case 2: Oversized position (exceeds exposure limit) - should fail
    trade_request_t oversized_trade = {
        .symbol = "EURUSD",
        .position_size = 60,     // 6% exposure (exceeds 5% limit)
        .entry_price = 1.1000,
        .stop_loss = 1.0890,    // 1% stop
        .account_balance = 1000,
        .timestamp = 0
    };
    
    assert(check_position_size_limit(protection, &oversized_trade) == false);
    
    // Test case 3: Edge case - max allowed position (5% exposure limit)
    trade_request_t edge_trade = {
        .symbol = "EURUSD",
        .position_size = 50,     // 5% exposure (at limit)
        .entry_price = 1.1000,
        .stop_loss = 1.0890,    // 1% stop = 0.05% risk
        .account_balance = 1000,
        .timestamp = 0
    };
    
    assert(check_position_size_limit(protection, &edge_trade) == true);  // At exposure limit should pass
    
    // Test case 4: Total exposure limit (5% max)
    protection->current_exposure = 40;  // 4% already exposed
    trade_request_t exposure_test = {
        .symbol = "GBPUSD",
        .position_size = 20,    // Would make 6% total
        .entry_price = 1.3000,
        .stop_loss = 1.2870,
        .account_balance = 1000,
        .timestamp = 0
    };
    
    assert(check_position_size_limit(protection, &exposure_test) == false);
    
    free(protection);
}

// Test 2: Daily loss circuit breaker
TEST(daily_loss_circuit) {
    core_protection_t* protection = create_test_protection();
    
    // Test case 1: No loss - trading allowed
    assert(check_daily_loss_circuit(protection) == true);
    
    // Test case 2: Small loss (1%) - trading allowed
    protection->daily_pnl = -10;  // $10 loss on $1000
    assert(check_daily_loss_circuit(protection) == true);
    
    // Test case 3: At limit (2%) - should trigger circuit breaker
    protection->daily_pnl = -20;  // $20 loss on $1000
    assert(check_daily_loss_circuit(protection) == false);
    assert(protection->trading_halted == true);
    
    // Test case 4: Already halted - stays halted
    assert(check_daily_loss_circuit(protection) == false);
    
    // Test case 5: Reset and test again
    reset_daily_counters(protection);
    assert(protection->trading_halted == false);
    assert(protection->daily_pnl == 0);
    assert(check_daily_loss_circuit(protection) == true);
    
    free(protection);
}

// Test 3: Stop loss enforcement
TEST(stop_loss_enforcement) {
    // Test case 1: No stop loss - should set default
    trade_request_t no_stop = {
        .symbol = "EURUSD",
        .position_size = 1000,
        .entry_price = 1.1000,
        .stop_loss = 0,  // No stop!
        .account_balance = 1000,
        .timestamp = 0
    };
    
    assert(enforce_stop_loss(&no_stop, 0.02) == false);  // Returns false (modified)
    assert(no_stop.stop_loss > 0);  // Stop loss was set
    assert(fabs(no_stop.stop_loss - 1.0780) < 0.0001);  // 2% stop
    
    // Test case 2: Valid stop loss
    trade_request_t valid_stop = {
        .symbol = "EURUSD",
        .position_size = 1000,
        .entry_price = 1.1000,
        .stop_loss = 1.0890,  // 1% stop
        .account_balance = 1000,
        .timestamp = 0
    };
    
    assert(enforce_stop_loss(&valid_stop, 0.02) == true);
    
    // Test case 3: Stop too far (>5%)
    trade_request_t far_stop = {
        .symbol = "EURUSD",
        .position_size = 1000,
        .entry_price = 1.1000,
        .stop_loss = 1.0000,  // 9% stop!
        .account_balance = 1000,
        .timestamp = 0
    };
    
    assert(enforce_stop_loss(&far_stop, 0.02) == false);
    assert(fabs(far_stop.stop_loss - 1.0780) < 0.0001);  // Reset to 2%
}

// Test 4: Emergency kill switch
TEST(emergency_kill_switch) {
    core_protection_t* protection = create_test_protection();
    
    // Test case 1: Normal operation
    assert(is_trading_allowed(protection) == true);
    
    // Test case 2: Activate kill switch
    assert(activate_kill_switch(protection) == true);
    assert(protection->kill_switch_enabled == true);
    assert(protection->trading_halted == true);
    assert(protection->kill_switch_timestamp > 0);
    
    // Test case 3: Trading not allowed after kill switch
    assert(is_trading_allowed(protection) == false);
    
    // Test case 4: Daily reset doesn't clear kill switch
    reset_daily_counters(protection);
    assert(protection->kill_switch_enabled == true);  // Still active
    assert(is_trading_allowed(protection) == false);
    
    free(protection);
}

// Test 5: Full validation flow
TEST(full_validation_flow) {
    core_protection_t* protection = create_test_protection();
    
    // Test case 1: Valid trade
    trade_request_t valid_trade = {
        .symbol = "EURUSD",
        .position_size = 50,     // 5% exposure
        .entry_price = 1.1000,
        .stop_loss = 1.0890,
        .account_balance = 1000,
        .timestamp = 0
    };
    
    protection_result_t result = validate_trade_protection(protection, &valid_trade);
    assert(result.approved == true);
    assert(result.rejection_reason == NULL);
    
    // Test case 2: No stop loss - should be added
    trade_request_t no_stop_trade = {
        .symbol = "GBPUSD",
        .position_size = 50,
        .entry_price = 1.3000,
        .stop_loss = 0,
        .account_balance = 1000,
        .timestamp = 0
    };
    
    result = validate_trade_protection(protection, &no_stop_trade);
    assert(result.approved == true);
    assert(no_stop_trade.stop_loss > 0);  // Stop was added
    
    // Test case 3: Oversized trade - should be adjusted
    trade_request_t oversized_trade = {
        .symbol = "USDJPY",
        .position_size = 150,    // Too large for 1% risk with 0.9% stop
        .entry_price = 110.00,
        .stop_loss = 109.00,    // 0.9% stop
        .account_balance = 1000,
        .timestamp = 0
    };
    
    double original_size = oversized_trade.position_size;
    result = validate_trade_protection(protection, &oversized_trade);
    printf("Oversized trade result: approved=%d, original_size=%.2f, adjusted_size=%.2f, final_size=%.2f\n",
           result.approved, original_size, result.adjusted_size, oversized_trade.position_size);
    // The trade should either be rejected OR have an adjusted size
    assert(!result.approved || result.adjusted_size < original_size);
    
    // Test case 4: Daily loss limit hit
    protection->daily_pnl = -25;  // 2.5% loss
    
    trade_request_t after_loss_trade = {
        .symbol = "EURUSD",
        .position_size = 100,
        .entry_price = 1.1000,
        .stop_loss = 1.0980,
        .account_balance = 1000,
        .timestamp = 0
    };
    
    result = validate_trade_protection(protection, &after_loss_trade);
    assert(result.approved == false);
    assert(strstr(result.rejection_reason, "Daily loss limit") != NULL);
    
    // Test case 5: Kill switch active
    activate_kill_switch(protection);
    
    result = validate_trade_protection(protection, &valid_trade);
    assert(result.approved == false);
    assert(strstr(result.rejection_reason, "kill switch") != NULL);
    
    free(protection);
}

// Test 6: Response time validation
TEST(response_time_validation) {
    core_protection_t* protection = create_test_protection();
    protection->max_response_time_ms = 100;  // 100ms requirement
    
    trade_request_t test_trade = {
        .symbol = "EURUSD",
        .position_size = 50,
        .entry_price = 1.1000,
        .stop_loss = 1.0890,
        .account_balance = 1000,
        .timestamp = 0
    };
    
    // Run validation 1000 times to test performance
    for (int i = 0; i < 1000; i++) {
        protection_result_t result = validate_trade_protection(protection, &test_trade);
        assert(result.approved == true);
    }
    
    // Check metrics
    protection_metrics_t* metrics = get_protection_metrics();
    printf("Average response time: %u microseconds\n", metrics->response_time_us);
    assert(metrics->response_time_us < 100000);  // Under 100ms
    
    free(protection);
}

// Test 7: Concurrent position tracking
TEST(concurrent_position_tracking) {
    core_protection_t* protection = create_test_protection();
    
    // Add positions incrementally
    protection->current_exposure = 0;
    
    // Position 1: 2% exposure
    protection->current_exposure = 20;
    
    trade_request_t new_position = {
        .symbol = "EURUSD",
        .position_size = 35,    // Would be 5.5% total
        .entry_price = 1.1000,
        .stop_loss = 1.0890,
        .account_balance = 1000,
        .timestamp = 0
    };
    
    // Should fail due to 5% total exposure limit
    assert(check_position_size_limit(protection, &new_position) == false);
    
    // Smaller position should work
    new_position.position_size = 25;  // 4.5% total
    assert(check_position_size_limit(protection, &new_position) == true);
    
    free(protection);
}

// Test 8: Daily P&L updates
TEST(daily_pnl_updates) {
    core_protection_t* protection = create_test_protection();
    
    // Simulate trading day
    update_daily_pnl(protection, 5);    // +$5
    assert(protection->daily_pnl == 5);
    assert(protection->trading_halted == false);
    
    update_daily_pnl(protection, -10);   // -$5 net
    assert(protection->daily_pnl == -5);
    assert(protection->trading_halted == false);
    
    update_daily_pnl(protection, -10);   // -$15 net (1.5%)
    assert(protection->daily_pnl == -15);
    assert(protection->trading_halted == false);
    
    update_daily_pnl(protection, -6);    // -$21 net (2.1%) - HALT!
    assert(protection->daily_pnl == -21);
    assert(protection->trading_halted == true);
    
    free(protection);
}

// Main test runner
int main(void) {
    printf("=== Core Protection Unit Tests ===\n\n");
    
    RUN_TEST(position_size_limits);
    RUN_TEST(daily_loss_circuit);
    RUN_TEST(stop_loss_enforcement);
    RUN_TEST(emergency_kill_switch);
    RUN_TEST(full_validation_flow);
    RUN_TEST(response_time_validation);
    RUN_TEST(concurrent_position_tracking);
    RUN_TEST(daily_pnl_updates);
    
    printf("\n✅ All tests passed!\n");
    
    // Print summary metrics
    protection_metrics_t* metrics = get_protection_metrics();
    printf("\nProtection Metrics:\n");
    printf("- Violations prevented: %u\n", metrics->violations_prevented);
    printf("- Average response time: %u μs\n", metrics->response_time_us);
    
    return 0;
}