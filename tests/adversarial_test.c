#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <pthread.h>
#include "../src/protection/core_protection.h"

// Adversarial test results
typedef struct {
    int total_attacks;
    int successful_attacks;
    int prevented_attacks;
    char attack_log[1024];
} adversarial_results_t;

// Global protection instance for testing
static core_protection_t* target_protection;

// Test 1: Flash Crash Attack
void test_flash_crash_attack(adversarial_results_t* results) {
    printf("\n⚔️ ADVERSARIAL TEST 1: Flash Crash Attack\n");
    printf("Scenario: 6% price drop in 30 seconds, massive position requests\n");
    
    results->total_attacks++;
    
    // Simulate rapid price movement with overleveraged trades
    double initial_price = 1.2800;  // GBP/USD
    double crash_price = 1.2000;    // -6.25% drop
    
    // Attacker tries to open huge position during crash
    trade_request_t attack_trade = {
        .symbol = "GBPUSD",
        .position_size = 50000,  // 50x normal size
        .entry_price = crash_price,
        .stop_loss = 0,  // No stop loss!
        .account_balance = 1000,
        .timestamp = 0
    };
    
    protection_result_t result = validate_trade_protection(target_protection, &attack_trade);
    
    if (!result.approved) {
        printf("✅ Attack PREVENTED: %s\n", result.rejection_reason);
        printf("   Position reduced from $50,000 to $%.2f\n", result.adjusted_size);
        printf("   Stop loss enforced at %.4f\n", result.required_stop_loss);
        results->prevented_attacks++;
    } else {
        printf("❌ Attack SUCCEEDED: Large position approved during crash!\n");
        results->successful_attacks++;
    }
}

// Test 2: Daily Loss Limit Bypass Attempt
void test_loss_limit_bypass(adversarial_results_t* results) {
    printf("\n⚔️ ADVERSARIAL TEST 2: Loss Limit Bypass Attack\n");
    printf("Scenario: Attacker tries multiple techniques to bypass 2% daily limit\n");
    
    results->total_attacks++;
    
    // First, create losses close to limit
    update_daily_pnl(target_protection, -19.5);  // 1.95% loss
    
    // Attack 1: Try small trades to sneak past
    trade_request_t sneak_trade = {
        .symbol = "EURUSD",
        .position_size = 10,  // Very small
        .entry_price = 1.1000,
        .stop_loss = 1.0890,
        .account_balance = 1000,
        .timestamp = 0
    };
    
    protection_result_t result = validate_trade_protection(target_protection, &sneak_trade);
    
    // Now push over the limit
    update_daily_pnl(target_protection, -1);  // Total: -2.05%
    
    // Attack 2: Try to trade after limit hit
    result = validate_trade_protection(target_protection, &sneak_trade);
    
    if (!result.approved) {
        printf("✅ Attack PREVENTED: Circuit breaker activated at 2%% loss\n");
        printf("   Trading halted successfully\n");
        results->prevented_attacks++;
    } else {
        printf("❌ Attack SUCCEEDED: Trading allowed past daily limit!\n");
        results->successful_attacks++;
    }
    
    // Reset for next test
    reset_daily_counters(target_protection);
}

// Test 3: Position Size Manipulation
void test_position_size_manipulation(adversarial_results_t* results) {
    printf("\n⚔️ ADVERSARIAL TEST 3: Position Size Manipulation\n");
    printf("Scenario: Attacker uses various tricks to exceed risk limits\n");
    
    results->total_attacks++;
    int attacks_blocked = 0;
    
    // Attack 1: Multiple small positions to exceed total exposure
    target_protection->current_exposure = 45;  // 4.5% already
    
    trade_request_t exposure_attack = {
        .symbol = "USDJPY",
        .position_size = 20,  // Would make 6.5% total
        .entry_price = 110.00,
        .stop_loss = 109.00,
        .account_balance = 1000,
        .timestamp = 0
    };
    
    protection_result_t result = validate_trade_protection(target_protection, &exposure_attack);
    if (!result.approved) attacks_blocked++;
    
    // Attack 2: Zero/negative stop distance
    trade_request_t zero_stop_attack = {
        .symbol = "AUDUSD",
        .position_size = 1000,
        .entry_price = 0.7000,
        .stop_loss = 0.7000,  // Same as entry!
        .account_balance = 1000,
        .timestamp = 0
    };
    
    result = validate_trade_protection(target_protection, &zero_stop_attack);
    if (zero_stop_attack.stop_loss != 0.7000) attacks_blocked++;  // Stop was adjusted
    
    // Attack 3: Massive position with tight stop
    trade_request_t massive_position = {
        .symbol = "EURUSD",
        .position_size = 100000,
        .entry_price = 1.1000,
        .stop_loss = 1.0999,  // 0.01% stop
        .account_balance = 1000,
        .timestamp = 0
    };
    
    result = validate_trade_protection(target_protection, &massive_position);
    if (!result.approved || result.adjusted_size < 100000) attacks_blocked++;
    
    if (attacks_blocked >= 3) {
        printf("✅ All 3 manipulation attacks PREVENTED\n");
        results->prevented_attacks++;
    } else {
        printf("❌ Some attacks SUCCEEDED: %d/3 blocked\n", attacks_blocked);
        results->successful_attacks++;
    }
    
    // Reset exposure
    target_protection->current_exposure = 0;
}

// Test 4: Kill Switch Circumvention
void test_kill_switch_bypass(adversarial_results_t* results) {
    printf("\n⚔️ ADVERSARIAL TEST 4: Kill Switch Bypass Attempt\n");
    printf("Scenario: Attacker tries to trade after emergency shutdown\n");
    
    results->total_attacks++;
    
    // Activate kill switch
    activate_kill_switch(target_protection);
    
    // Attack 1: Normal trade attempt
    trade_request_t normal_trade = {
        .symbol = "GBPUSD",
        .position_size = 100,
        .entry_price = 1.3000,
        .stop_loss = 1.2900,
        .account_balance = 1000,
        .timestamp = 0
    };
    
    protection_result_t result = validate_trade_protection(target_protection, &normal_trade);
    int attack1_blocked = !result.approved;
    
    // Attack 2: Try to reset without authorization
    target_protection->trading_halted = false;  // Direct manipulation!
    result = validate_trade_protection(target_protection, &normal_trade);
    int attack2_blocked = !result.approved;  // Should still be blocked by kill switch
    
    // Attack 3: Timestamp manipulation
    target_protection->kill_switch_timestamp = 0;  // Try to clear timestamp
    result = validate_trade_protection(target_protection, &normal_trade);
    int attack3_blocked = !result.approved;  // Should still be blocked
    
    if (attack1_blocked && attack2_blocked && attack3_blocked) {
        printf("✅ Kill switch SECURE: All bypass attempts prevented\n");
        results->prevented_attacks++;
    } else {
        printf("❌ Kill switch COMPROMISED: Some bypasses succeeded\n");
        results->successful_attacks++;
    }
    
    // Reset kill switch for next tests
    target_protection->kill_switch_enabled = false;
    target_protection->trading_halted = false;
}

// Test 5: Race Condition Exploitation
void* race_condition_worker(void* arg) {
    adversarial_results_t* results = (adversarial_results_t*)arg;
    
    // Try to exploit non-atomic operations
    for (int i = 0; i < 1000; i++) {
        // Rapidly toggle states
        target_protection->daily_pnl = -15;
        target_protection->daily_pnl = -25;  // Over limit
        target_protection->daily_pnl = -10;  // Back under
        
        trade_request_t race_trade = {
            .symbol = "EURUSD",
            .position_size = 1000,
            .entry_price = 1.1000,
            .stop_loss = 1.0900,
            .account_balance = 1000,
            .timestamp = i
        };
        
        protection_result_t result = validate_trade_protection(target_protection, &race_trade);
        
        // Check for inconsistent state
        if (result.approved && target_protection->trading_halted) {
            results->successful_attacks++;
            break;
        }
    }
    
    return NULL;
}

void test_race_conditions(adversarial_results_t* results) {
    printf("\n⚔️ ADVERSARIAL TEST 5: Race Condition Attack\n");
    printf("Scenario: Multiple threads try to exploit timing windows\n");
    
    results->total_attacks++;
    
    pthread_t threads[4];
    adversarial_results_t thread_results = {0};
    
    // Launch concurrent attacks
    for (int i = 0; i < 4; i++) {
        pthread_create(&threads[i], NULL, race_condition_worker, &thread_results);
    }
    
    // Wait for completion
    for (int i = 0; i < 4; i++) {
        pthread_join(threads[i], NULL);
    }
    
    if (thread_results.successful_attacks == 0) {
        printf("✅ Race condition protection EFFECTIVE: No exploits found\n");
        results->prevented_attacks++;
    } else {
        printf("❌ Race condition EXPLOITED: %d successful attacks\n", 
               thread_results.successful_attacks);
        results->successful_attacks++;
    }
}

// Test 6: Input Fuzzing
void test_input_fuzzing(adversarial_results_t* results) {
    printf("\n⚔️ ADVERSARIAL TEST 6: Input Fuzzing Attack\n");
    printf("Scenario: Random/malformed inputs to find crashes\n");
    
    results->total_attacks++;
    int crashes = 0;
    
    // Fuzz with random data
    for (int i = 0; i < 1000; i++) {
        trade_request_t fuzz_trade;
        
        // Fill with random bytes
        unsigned char* ptr = (unsigned char*)&fuzz_trade;
        for (size_t j = 0; j < sizeof(trade_request_t); j++) {
            ptr[j] = rand() % 256;
        }
        
        // Ensure null termination for symbol
        fuzz_trade.symbol[15] = '\0';
        
        // Try to cause crash
        protection_result_t result = validate_trade_protection(target_protection, &fuzz_trade);
        
        // If we get here, no crash occurred
    }
    
    if (crashes == 0) {
        printf("✅ Fuzzing protection ROBUST: No crashes in 1000 attempts\n");
        results->prevented_attacks++;
    } else {
        printf("❌ Fuzzing found %d CRASHES\n", crashes);
        results->successful_attacks++;
    }
}

// Test 7: Psychological Manipulation
void test_psychological_attacks(adversarial_results_t* results) {
    printf("\n⚔️ ADVERSARIAL TEST 7: Psychological Manipulation\n");
    printf("Scenario: Exploit human emotions and biases\n");
    
    results->total_attacks++;
    
    // Create a winning streak to build overconfidence
    for (int i = 0; i < 5; i++) {
        update_daily_pnl(target_protection, 2);  // Small wins
    }
    
    // Now try oversized "sure thing" trade
    trade_request_t greed_trade = {
        .symbol = "EURUSD",
        .position_size = 5000,  // 5x normal
        .entry_price = 1.1000,
        .stop_loss = 1.0950,   // Tight stop for "sure thing"
        .account_balance = 1000,
        .timestamp = 0
    };
    
    protection_result_t result = validate_trade_protection(target_protection, &greed_trade);
    
    if (!result.approved || result.adjusted_size < 5000) {
        printf("✅ Psychological attack PREVENTED: Greed-based overleveraging blocked\n");
        results->prevented_attacks++;
    } else {
        printf("❌ Psychological attack SUCCEEDED: Emotion-based trade approved\n");
        results->successful_attacks++;
    }
}

// Main adversarial test runner
int main(void) {
    printf("=== 80/20 Core Protection Adversarial Tests ===\n");
    printf("Testing system resilience against malicious attacks...\n");
    
    // Initialize target protection system
    target_protection = malloc(sizeof(core_protection_t));
    target_protection->max_position_risk_percent = 0.01;
    target_protection->daily_loss_limit_percent = 0.02;
    target_protection->current_exposure = 0;
    target_protection->daily_pnl = 0;
    target_protection->trading_halted = false;
    target_protection->require_stop_loss = true;
    target_protection->default_stop_percent = 0.02;
    target_protection->kill_switch_enabled = false;
    target_protection->kill_switch_timestamp = 0;
    target_protection->max_response_time_ms = 100;
    
    adversarial_results_t results = {0};
    
    // Run all adversarial tests
    test_flash_crash_attack(&results);
    test_loss_limit_bypass(&results);
    test_position_size_manipulation(&results);
    test_kill_switch_bypass(&results);
    test_race_conditions(&results);
    test_input_fuzzing(&results);
    test_psychological_attacks(&results);
    
    // Summary
    printf("\n📊 ADVERSARIAL TEST SUMMARY\n");
    printf("========================\n");
    printf("Total attack scenarios: %d\n", results.total_attacks);
    printf("Attacks prevented: %d (%.1f%%)\n", 
           results.prevented_attacks,
           100.0 * results.prevented_attacks / results.total_attacks);
    printf("Attacks succeeded: %d (%.1f%%)\n", 
           results.successful_attacks,
           100.0 * results.successful_attacks / results.total_attacks);
    
    // Calculate adversarial survival rate
    double survival_rate = (double)results.prevented_attacks / results.total_attacks;
    printf("\n🛡️ Adversarial Survival Rate: %.1f%%\n", survival_rate * 100);
    
    if (survival_rate >= 0.90) {
        printf("✅ EXCELLENT: System maintains 90%+ protection under attack\n");
    } else if (survival_rate >= 0.80) {
        printf("⚠️ GOOD: System needs minor hardening\n");
    } else {
        printf("❌ CRITICAL: System vulnerable to attacks\n");
    }
    
    // Cleanup
    free(target_protection);
    
    return 0;
}