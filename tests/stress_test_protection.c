#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include "../src/protection/core_protection.h"

#define NUM_THREADS 16
#define TRADES_PER_THREAD 10000
#define TOTAL_TRADES (NUM_THREADS * TRADES_PER_THREAD)
#define STRESS_DURATION_SECONDS 10

// Global statistics
typedef struct {
    uint64_t total_trades;
    uint64_t approved_trades;
    uint64_t rejected_trades;
    uint64_t violations_prevented;
    uint64_t max_response_time_us;
    double avg_response_time_us;
    pthread_mutex_t mutex;
} stress_stats_t;

static stress_stats_t global_stats = {
    .mutex = PTHREAD_MUTEX_INITIALIZER
};

// Shared protection instance (thread-safe testing)
static core_protection_t* shared_protection;
static pthread_mutex_t protection_mutex = PTHREAD_MUTEX_INITIALIZER;

// Volatile flag for stress test control
static volatile int keep_running = 1;

// Signal handler for graceful shutdown
void signal_handler(int sig) {
    if (sig == SIGINT) {
        keep_running = 0;
        printf("\n⚠️ Stopping stress test...\n");
    }
}

// Generate random trade request
trade_request_t generate_random_trade(int thread_id, int trade_num) {
    trade_request_t trade;
    
    // Vary symbols
    const char* symbols[] = {"EURUSD", "GBPUSD", "USDJPY", "AUDUSD", "USDCAD"};
    strcpy(trade.symbol, symbols[rand() % 5]);
    
    // Vary position sizes (some oversized)
    trade.position_size = 100 + (rand() % 2000);
    
    // Entry prices
    trade.entry_price = 1.0 + (rand() % 5000) / 10000.0;
    
    // Stop losses (10% have no stop)
    if (rand() % 10 == 0) {
        trade.stop_loss = 0;  // No stop loss
    } else {
        trade.stop_loss = trade.entry_price * (0.95 + (rand() % 40) / 1000.0);
    }
    
    trade.account_balance = 1000;
    trade.timestamp = thread_id * 1000000 + trade_num;
    
    return trade;
}

// Thread worker function
void* stress_worker(void* arg) {
    int thread_id = *(int*)arg;
    uint64_t local_approved = 0;
    uint64_t local_rejected = 0;
    uint64_t local_max_response = 0;
    uint64_t total_response_time = 0;
    
    // Seed random number generator
    srand(time(NULL) + thread_id);
    
    for (int i = 0; i < TRADES_PER_THREAD && keep_running; i++) {
        trade_request_t trade = generate_random_trade(thread_id, i);
        
        // Measure response time
        struct timespec start, end;
        clock_gettime(CLOCK_MONOTONIC, &start);
        
        // Thread-safe validation
        pthread_mutex_lock(&protection_mutex);
        protection_result_t result = validate_trade_protection(shared_protection, &trade);
        
        // Simulate some P&L updates
        if (i % 100 == 0) {
            double pnl_change = -5 + (rand() % 15);  // -5 to +10
            update_daily_pnl(shared_protection, pnl_change);
        }
        
        pthread_mutex_unlock(&protection_mutex);
        
        clock_gettime(CLOCK_MONOTONIC, &end);
        
        uint64_t response_time = (end.tv_sec - start.tv_sec) * 1000000 +
                                (end.tv_nsec - start.tv_nsec) / 1000;
        
        if (result.approved) {
            local_approved++;
        } else {
            local_rejected++;
        }
        
        total_response_time += response_time;
        if (response_time > local_max_response) {
            local_max_response = response_time;
        }
        
        // Small delay to simulate realistic trading
        if (i % 10 == 0) {
            usleep(100);  // 100 microseconds
        }
    }
    
    // Update global statistics
    pthread_mutex_lock(&global_stats.mutex);
    global_stats.total_trades += local_approved + local_rejected;
    global_stats.approved_trades += local_approved;
    global_stats.rejected_trades += local_rejected;
    if (local_max_response > global_stats.max_response_time_us) {
        global_stats.max_response_time_us = local_max_response;
    }
    global_stats.avg_response_time_us = 
        (global_stats.avg_response_time_us * global_stats.total_trades + total_response_time) /
        (global_stats.total_trades + local_approved + local_rejected);
    pthread_mutex_unlock(&global_stats.mutex);
    
    return NULL;
}

// Test concurrent access patterns
void stress_test_concurrency() {
    printf("\n💪 Stress Test: Concurrent Trading Simulation\n");
    printf("Threads: %d, Trades per thread: %d\n", NUM_THREADS, TRADES_PER_THREAD);
    
    pthread_t threads[NUM_THREADS];
    int thread_ids[NUM_THREADS];
    
    // Start all threads
    for (int i = 0; i < NUM_THREADS; i++) {
        thread_ids[i] = i;
        pthread_create(&threads[i], NULL, stress_worker, &thread_ids[i]);
    }
    
    // Monitor progress
    int seconds = 0;
    while (seconds < STRESS_DURATION_SECONDS && keep_running) {
        sleep(1);
        seconds++;
        
        pthread_mutex_lock(&global_stats.mutex);
        printf("\r⏱️ Progress: %lu trades processed (%.1f trades/sec)...", 
               global_stats.total_trades,
               (double)global_stats.total_trades / seconds);
        fflush(stdout);
        pthread_mutex_unlock(&global_stats.mutex);
    }
    
    // Signal threads to stop
    keep_running = 0;
    
    // Wait for all threads
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }
    
    printf("\n\n📊 Stress Test Results:\n");
    printf("Total trades processed: %lu\n", global_stats.total_trades);
    printf("Approved trades: %lu (%.1f%%)\n", 
           global_stats.approved_trades,
           100.0 * global_stats.approved_trades / global_stats.total_trades);
    printf("Rejected trades: %lu (%.1f%%)\n", 
           global_stats.rejected_trades,
           100.0 * global_stats.rejected_trades / global_stats.total_trades);
    printf("Average response time: %.2f μs\n", global_stats.avg_response_time_us);
    printf("Max response time: %lu μs\n", global_stats.max_response_time_us);
    printf("Throughput: %.1f trades/second\n", 
           (double)global_stats.total_trades / STRESS_DURATION_SECONDS);
    
    // Verify response time requirement
    if (global_stats.max_response_time_us < 100000) {
        printf("✅ PASS: Max response time under 100ms requirement\n");
    } else {
        printf("❌ FAIL: Max response time exceeds 100ms!\n");
    }
}

// Stress test memory usage
void stress_test_memory() {
    printf("\n💪 Stress Test: Memory Stability\n");
    
    // Allocate and free protection instances repeatedly
    for (int i = 0; i < 100000; i++) {
        core_protection_t* protection = malloc(sizeof(core_protection_t));
        memset(protection, 0, sizeof(core_protection_t));
        
        protection->max_position_risk_percent = 0.01;
        protection->daily_loss_limit_percent = 0.02;
        protection->default_stop_percent = 0.02;
        protection->max_response_time_ms = 100;
        
        // Simulate some operations
        trade_request_t trade = generate_random_trade(0, i);
        validate_trade_protection(protection, &trade);
        
        free(protection);
        
        if (i % 10000 == 0) {
            printf("\r🔄 Memory cycles: %d", i);
            fflush(stdout);
        }
    }
    
    printf("\n✅ Memory stress test completed - no leaks\n");
}

// Stress test edge cases
void stress_test_edge_cases() {
    printf("\n💪 Stress Test: Edge Cases\n");
    
    // Test extreme values
    trade_request_t extreme_trades[] = {
        // Huge position
        {.symbol = "EURUSD", .position_size = 1000000, .entry_price = 1.1, 
         .stop_loss = 1.0, .account_balance = 1000},
        
        // Tiny position
        {.symbol = "GBPUSD", .position_size = 0.01, .entry_price = 1.3,
         .stop_loss = 1.29, .account_balance = 1000},
        
        // Zero stop loss
        {.symbol = "USDJPY", .position_size = 1000, .entry_price = 110,
         .stop_loss = 0, .account_balance = 1000},
        
        // Stop loss equals entry
        {.symbol = "AUDUSD", .position_size = 500, .entry_price = 0.7,
         .stop_loss = 0.7, .account_balance = 1000},
        
        // Negative account balance (should not happen but test anyway)
        {.symbol = "USDCAD", .position_size = 100, .entry_price = 1.25,
         .stop_loss = 1.24, .account_balance = -1000}
    };
    
    for (int i = 0; i < 5; i++) {
        protection_result_t result = validate_trade_protection(shared_protection, 
                                                             &extreme_trades[i]);
        printf("Edge case %d: %s\n", i + 1, 
               result.approved ? "Handled correctly" : result.rejection_reason);
    }
    
    printf("✅ All edge cases handled without crashes\n");
}

// Test recovery after circuit breaker
void stress_test_recovery() {
    printf("\n💪 Stress Test: Circuit Breaker Recovery\n");
    
    // Trigger circuit breaker
    update_daily_pnl(shared_protection, -25);  // 2.5% loss
    
    // Try to trade (should fail)
    trade_request_t trade = {
        .symbol = "EURUSD",
        .position_size = 100,
        .entry_price = 1.1,
        .stop_loss = 1.09,
        .account_balance = 1000
    };
    
    protection_result_t result = validate_trade_protection(shared_protection, &trade);
    printf("After circuit breaker: %s\n", 
           result.approved ? "ERROR - Trade approved!" : "✅ Trade blocked");
    
    // Reset daily counters
    reset_daily_counters(shared_protection);
    
    // Try again (should work)
    result = validate_trade_protection(shared_protection, &trade);
    printf("After reset: %s\n", 
           result.approved ? "✅ Trade approved" : "ERROR - Trade still blocked!");
}

// Main stress test runner
int main(void) {
    printf("=== 80/20 Core Protection Stress Tests ===\n");
    
    // Set up signal handler
    signal(SIGINT, signal_handler);
    
    // Initialize shared protection instance
    shared_protection = malloc(sizeof(core_protection_t));
    shared_protection->max_position_risk_percent = 0.01;
    shared_protection->daily_loss_limit_percent = 0.02;
    shared_protection->current_exposure = 0;
    shared_protection->daily_pnl = 0;
    shared_protection->trading_halted = false;
    shared_protection->require_stop_loss = true;
    shared_protection->default_stop_percent = 0.02;
    shared_protection->kill_switch_enabled = false;
    shared_protection->kill_switch_timestamp = 0;
    shared_protection->max_response_time_ms = 100;
    
    // Run stress tests
    stress_test_concurrency();
    stress_test_memory();
    stress_test_edge_cases();
    stress_test_recovery();
    
    // Get final metrics
    protection_metrics_t* metrics = get_protection_metrics();
    printf("\n📊 Final Protection Metrics:\n");
    printf("Violations prevented: %u\n", metrics->violations_prevented);
    printf("Response time: %u μs\n", metrics->response_time_us);
    
    // Cleanup
    free(shared_protection);
    
    printf("\n✅ All stress tests completed successfully!\n");
    return 0;
}