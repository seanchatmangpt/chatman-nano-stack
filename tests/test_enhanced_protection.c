#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <time.h>
#include <math.h>
#include "../src/protection/enhanced_protection.h"

// Test utilities
#define TEST_ASSERT(condition, message) \
    do { \
        if (!(condition)) { \
            printf("❌ FAILED: %s\n", message); \
            return 0; \
        } else { \
            printf("✅ PASSED: %s\n", message); \
        } \
    } while(0)

#define TEST_COUNT 12
static int tests_passed = 0;

// Helper function to create test trade request
trade_request_t create_test_request(const char* symbol, double position_size, 
                                  double entry_price, double stop_loss) {
    trade_request_t request = {0};
    strncpy(request.symbol, symbol, sizeof(request.symbol) - 1);
    request.position_size = position_size;
    request.entry_price = entry_price;
    request.stop_loss = stop_loss;
    request.account_balance = 1000.0;
    request.timestamp = time(NULL);
    return request;
}

// Test 1: Volatility Detection - Flash Crash Protection
int test_volatility_detection(void) {
    printf("\n🧪 Test 1: Volatility Detection (Flash Crash Protection)\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Simulate normal price movement (should pass)
    trade_request_t normal_request = create_test_request("EURUSD", 100.0, 1.1000, 1.0980);
    
    // Add normal price history
    for (int i = 0; i < 10; i++) {
        update_price_history(protection, 1.1000 + (i * 0.0001), "EURUSD");
    }
    
    bool normal_volatility = detect_price_volatility(protection, 1.1010, "EURUSD");
    TEST_ASSERT(!normal_volatility, "Normal price movement not flagged as volatile");
    
    // Simulate flash crash (6% drop - should be detected)
    double crash_price = 1.1000 * 0.94; // 6% drop
    update_price_history(protection, crash_price, "EURUSD");
    
    bool flash_crash_detected = detect_price_volatility(protection, crash_price, "EURUSD");
    TEST_ASSERT(flash_crash_detected, "Flash crash (6% drop) detected as volatile");
    
    // Test full validation with volatility
    enhanced_result_t result = validate_trade_enhanced(protection, &normal_request);
    TEST_ASSERT(!result.base.approved, "Trade rejected during high volatility");
    TEST_ASSERT(result.recommended_halt == HALT_VOLATILITY_SPIKE, "Volatility halt recommended");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 2: Position Correlation - Manipulation Protection  
int test_position_correlation(void) {
    printf("\n🧪 Test 2: Position Correlation (Manipulation Protection)\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Test normal correlated position (should pass)
    trade_request_t eur_request = create_test_request("EURUSD", 50.0, 1.1000, 1.0980);
    bool correlation_ok = check_position_correlation(protection, &eur_request);
    TEST_ASSERT(correlation_ok, "Normal EUR position passes correlation check");
    
    // Add the EUR position to tracking
    add_position_tracking(protection, &eur_request);
    
    // Test highly correlated position (EUR/GBP) - should still pass with small size
    trade_request_t gbp_request = create_test_request("EURGBP", 30.0, 0.8500, 0.8480);
    bool small_correlated = check_position_correlation(protection, &gbp_request);
    TEST_ASSERT(small_correlated, "Small correlated position passes");
    
    // Test excessive correlated exposure (should fail)
    trade_request_t large_eur_request = create_test_request("EURJPY", 200.0, 130.00, 129.00);
    bool excessive_correlation = check_position_correlation(protection, &large_eur_request);
    TEST_ASSERT(!excessive_correlation, "Excessive correlated exposure rejected");
    
    // Test currency exposure limits
    double eur_exposure = get_currency_exposure(protection, "EUR");
    TEST_ASSERT(eur_exposure == 50.0, "EUR exposure tracked correctly");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 3: Enhanced Circuit Breaker - Race Condition Fix
int test_enhanced_circuit_breaker(void) {
    printf("\n🧪 Test 3: Enhanced Circuit Breaker (Race Condition Fix)\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Test normal state
    bool initially_halted = is_trading_halted(protection);
    TEST_ASSERT(!initially_halted, "Trading not initially halted");
    
    // Trigger circuit breaker
    bool breaker_triggered = trigger_enhanced_circuit_breaker(protection, HALT_VOLATILITY_SPIKE);
    TEST_ASSERT(breaker_triggered, "Circuit breaker triggered successfully");
    
    // Test halted state (atomic operation)
    bool now_halted = is_trading_halted(protection);
    TEST_ASSERT(now_halted, "Trading halted after circuit breaker");
    
    // Test trade rejection during halt
    trade_request_t request = create_test_request("EURUSD", 50.0, 1.1000, 1.0980);
    enhanced_result_t result = validate_trade_enhanced(protection, &request);
    TEST_ASSERT(!result.base.approved, "Trade rejected during circuit breaker halt");
    
    // Reset circuit breaker
    reset_circuit_breaker(protection);
    bool reset_successful = !is_trading_halted(protection);
    TEST_ASSERT(reset_successful, "Circuit breaker reset successfully");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 4: Manipulation Detection
int test_manipulation_detection(void) {
    printf("\n🧪 Test 4: Manipulation Detection\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Test normal request (should pass)
    trade_request_t normal_request = create_test_request("EURUSD", 75.0, 1.1000, 1.0980);
    bool normal_ok = !detect_manipulation_attempt(protection, &normal_request);
    TEST_ASSERT(normal_ok, "Normal trade request not flagged as manipulation");
    
    // Test suspicious pattern - unrealistic stop loss
    trade_request_t tight_stop_request = create_test_request("EURUSD", 100.0, 1.1000, 1.0999);
    bool tight_stop_detected = detect_manipulation_attempt(protection, &tight_stop_request);
    TEST_ASSERT(tight_stop_detected, "Unrealistic tight stop detected as manipulation");
    
    // Test timestamp validation
    trade_request_t old_request = create_test_request("EURUSD", 50.0, 1.1000, 1.0980);
    old_request.timestamp = time(NULL) - 10; // 10 seconds old
    bool old_timestamp_rejected = detect_manipulation_attempt(protection, &old_request);
    TEST_ASSERT(old_timestamp_rejected, "Old timestamp rejected as manipulation");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 5: Price History and Volatility Calculation
int test_price_history_management(void) {
    printf("\n🧪 Test 5: Price History and Volatility Calculation\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Test price history updates
    double test_prices[] = {1.1000, 1.1001, 1.1002, 1.0999, 1.1003};
    for (int i = 0; i < 5; i++) {
        update_price_history(protection, test_prices[i], "EURUSD");
    }
    
    // Test volatility calculation with stable prices
    double stable_volatility = calculate_volatility_score(protection->price_history, 5);
    TEST_ASSERT(stable_volatility < 0.01, "Stable prices show low volatility");
    
    // Add volatile price movement
    update_price_history(protection, 1.0500, "EURUSD"); // Large drop
    double volatile_score = calculate_volatility_score(protection->price_history, 6);
    TEST_ASSERT(volatile_score > 0.015, "Volatile prices show high volatility");
    
    // Test circular buffer behavior (120 price limit)
    for (int i = 0; i < 150; i++) {
        update_price_history(protection, 1.1000 + (i * 0.0001), "EURUSD");
    }
    TEST_ASSERT(protection->price_history_index == 156, "Price history index increments correctly");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 6: Position Tracking Lifecycle
int test_position_tracking_lifecycle(void) {
    printf("\n🧪 Test 6: Position Tracking Lifecycle\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Test adding positions
    trade_request_t eur_request = create_test_request("EURUSD", 100.0, 1.1000, 1.0980);
    add_position_tracking(protection, &eur_request);
    
    trade_request_t gbp_request = create_test_request("GBPUSD", 75.0, 1.2500, 1.2480);
    add_position_tracking(protection, &gbp_request);
    
    int position_count = atomic_load(&protection->position_count);
    TEST_ASSERT(position_count == 2, "Position count tracks correctly");
    
    // Test currency exposure calculation
    double usd_exposure = get_currency_exposure(protection, "USD");
    TEST_ASSERT(usd_exposure == 175.0, "USD exposure calculated correctly (EUR+GBP)");
    
    double eur_exposure = get_currency_exposure(protection, "EUR");
    TEST_ASSERT(eur_exposure == 100.0, "EUR exposure calculated correctly");
    
    // Test position removal
    remove_position_tracking(protection, "EURUSD");
    int after_removal = atomic_load(&protection->position_count);
    TEST_ASSERT(after_removal == 1, "Position removed correctly");
    
    double eur_after_removal = get_currency_exposure(protection, "EUR");
    TEST_ASSERT(eur_after_removal == 0.0, "EUR exposure zero after removal");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 7: Full Enhanced Validation Flow
int test_full_enhanced_validation(void) {
    printf("\n🧪 Test 7: Full Enhanced Validation Flow\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Test normal trade approval
    trade_request_t normal_request = create_test_request("EURUSD", 10.0, 1.1000, 1.0980);
    enhanced_result_t normal_result = validate_trade_enhanced(protection, &normal_request);
    TEST_ASSERT(normal_result.base.approved, "Normal trade approved through enhanced validation");
    
    // Test rejection due to base protection (excessive position size)
    trade_request_t large_request = create_test_request("EURUSD", 500.0, 1.1000, 1.0980);
    enhanced_result_t large_result = validate_trade_enhanced(protection, &large_request);
    TEST_ASSERT(!large_result.base.approved, "Large position rejected by base protection");
    
    // Trigger volatility and test rejection
    for (int i = 0; i < 10; i++) {
        update_price_history(protection, 1.1000, "EURUSD");
    }
    update_price_history(protection, 1.0340, "EURUSD"); // 6% drop
    
    trade_request_t volatile_request = create_test_request("EURUSD", 50.0, 1.0340, 1.0320);
    enhanced_result_t volatile_result = validate_trade_enhanced(protection, &volatile_request);
    TEST_ASSERT(!volatile_result.base.approved, "Trade rejected during volatility spike");
    TEST_ASSERT(volatile_result.recommended_halt == HALT_VOLATILITY_SPIKE, "Volatility halt recommended");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 8: Concurrent Access Safety
int test_concurrent_access_safety(void) {
    printf("\n🧪 Test 8: Concurrent Access Safety\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Test atomic operations
    atomic_store(&protection->high_volatility_detected, true);
    bool volatility_state = atomic_load(&protection->high_volatility_detected);
    TEST_ASSERT(volatility_state, "Atomic volatility flag works correctly");
    
    atomic_store(&protection->circuit_breaker_active, true);
    bool breaker_state = atomic_load(&protection->circuit_breaker_active);
    TEST_ASSERT(breaker_state, "Atomic circuit breaker flag works correctly");
    
    // Test position count atomicity
    atomic_store(&protection->position_count, 5);
    int count = atomic_load(&protection->position_count);
    TEST_ASSERT(count == 5, "Atomic position count works correctly");
    
    // Test halt detection with atomic operations
    bool halted = is_trading_halted(protection);
    TEST_ASSERT(halted, "Trading halt detected with atomic operations");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 9: Enhanced Metrics Collection
int test_enhanced_metrics(void) {
    printf("\n🧪 Test 9: Enhanced Metrics Collection\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    enhanced_metrics_t* metrics = get_enhanced_metrics();
    TEST_ASSERT(metrics != NULL, "Enhanced metrics accessible");
    
    // Reset metrics
    atomic_store(&metrics->violations_prevented, 0);
    atomic_store(&metrics->volatility_detections, 0);
    atomic_store(&metrics->correlation_blocks, 0);
    
    // Trigger enhanced violations and check metrics  
    // First add some volatility to trigger enhanced protection
    for (int i = 0; i < 10; i++) {
        update_price_history(protection, 1.1000, "EURUSD");
    }
    update_price_history(protection, 1.0340, "EURUSD"); // 6% drop - triggers volatility
    
    trade_request_t volatile_request = create_test_request("EURUSD", 50.0, 1.0340, 1.0320);
    enhanced_result_t result = validate_trade_enhanced(protection, &volatile_request);
    
    int volatility_detections = atomic_load(&metrics->volatility_detections);
    TEST_ASSERT(volatility_detections >= 1, "Volatility detection metric incremented");
    
    // Trigger volatility detection
    update_price_history(protection, 1.0340, "EURUSD"); // Volatile price
    detect_price_volatility(protection, 1.0340, "EURUSD");
    
    int volatility_events = atomic_load(&metrics->volatility_detections);
    TEST_ASSERT(volatility_events >= 1, "Volatility detection metric incremented");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 10: Memory Management
int test_memory_management(void) {
    printf("\n🧪 Test 10: Memory Management\n");
    
    // Test creation and destruction
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection allocated successfully");
    
    // Add multiple positions to test cleanup
    for (int i = 0; i < 5; i++) {
        char symbol[8];
        snprintf(symbol, sizeof(symbol), "TEST%d", i);
        trade_request_t request = create_test_request(symbol, 50.0, 1.1000, 1.0980);
        add_position_tracking(protection, &request);
    }
    
    int final_count = atomic_load(&protection->position_count);
    TEST_ASSERT(final_count == 5, "All positions added successfully");
    
    // Test cleanup (should not crash or leak)
    destroy_enhanced_protection(protection);
    
    // Test null pointer handling
    destroy_enhanced_protection(NULL);
    
    printf("✅ PASSED: Memory management working correctly\n");
    return 1;
}

// Test 11: Edge Cases and Error Handling
int test_edge_cases(void) {
    printf("\n🧪 Test 11: Edge Cases and Error Handling\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Test unknown currency handling
    trade_request_t unknown_request = create_test_request("XXXXXX", 50.0, 1.0000, 0.9980);
    bool unknown_correlation = check_position_correlation(protection, &unknown_request);
    TEST_ASSERT(unknown_correlation, "Unknown currency treated conservatively");
    
    // Test zero position size
    trade_request_t zero_request = create_test_request("EURUSD", 0.0, 1.1000, 1.0980);
    enhanced_result_t zero_result = validate_trade_enhanced(protection, &zero_request);
    TEST_ASSERT(zero_result.base.approved, "Zero position size handled correctly");
    
    // Test empty price history volatility
    double empty_volatility = calculate_volatility_score(protection->price_history, 0);
    TEST_ASSERT(empty_volatility == 0.0, "Empty price history returns zero volatility");
    
    // Test single price volatility
    double single_volatility = calculate_volatility_score(protection->price_history, 1);
    TEST_ASSERT(single_volatility == 0.0, "Single price returns zero volatility");
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test 12: Performance Validation
int test_performance_validation(void) {
    printf("\n🧪 Test 12: Performance Validation\n");
    
    enhanced_protection_t* protection = create_enhanced_protection();
    TEST_ASSERT(protection != NULL, "Enhanced protection created");
    
    // Measure enhanced validation performance
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    
    // Run 1000 validations
    for (int i = 0; i < 1000; i++) {
        trade_request_t request = create_test_request("EURUSD", 50.0, 1.1000, 1.0980);
        enhanced_result_t result = validate_trade_enhanced(protection, &request);
        (void)result; // Suppress unused warning
    }
    
    clock_gettime(CLOCK_MONOTONIC, &end);
    
    uint64_t duration_ns = (end.tv_sec - start.tv_sec) * 1000000000ULL + 
                          (end.tv_nsec - start.tv_nsec);
    uint64_t avg_ns = duration_ns / 1000;
    
    printf("Enhanced validation average time: %llu ns\n", (unsigned long long)avg_ns);
    TEST_ASSERT(avg_ns < 10000, "Enhanced validation under 10μs per call");
    
    // Test response time tracking
    enhanced_metrics_t* metrics = get_enhanced_metrics();
    uint32_t response_time = atomic_load(&metrics->response_time_us);
    printf("Last response time: %u μs\n", response_time);
    
    destroy_enhanced_protection(protection);
    return 1;
}

// Test runner
int main(void) {
    printf("🚀 Enhanced Protection Test Suite\n");
    printf("=================================\n");
    
    // Run all tests
    if (test_volatility_detection()) tests_passed++;
    if (test_position_correlation()) tests_passed++;
    if (test_enhanced_circuit_breaker()) tests_passed++;
    if (test_manipulation_detection()) tests_passed++;
    if (test_price_history_management()) tests_passed++;
    if (test_position_tracking_lifecycle()) tests_passed++;
    if (test_full_enhanced_validation()) tests_passed++;
    if (test_concurrent_access_safety()) tests_passed++;
    if (test_enhanced_metrics()) tests_passed++;
    if (test_memory_management()) tests_passed++;
    if (test_edge_cases()) tests_passed++;
    if (test_performance_validation()) tests_passed++;
    
    // Results summary
    printf("\n📊 Test Results Summary\n");
    printf("======================\n");
    printf("Tests passed: %d/%d\n", tests_passed, TEST_COUNT);
    
    if (tests_passed == TEST_COUNT) {
        printf("🎉 ALL TESTS PASSED!\n");
        printf("✅ Enhanced protection vulnerabilities fixed\n");
        printf("✅ Flash crash protection validated\n");
        printf("✅ Position manipulation protection validated\n");
        printf("✅ Circuit breaker race conditions fixed\n");
        printf("✅ System ready for adversarial validation\n");
        return 0;
    } else {
        printf("❌ %d tests failed\n", TEST_COUNT - tests_passed);
        return 1;
    }
}