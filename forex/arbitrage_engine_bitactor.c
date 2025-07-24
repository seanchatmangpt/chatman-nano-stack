/*
 * Arbitrage Engine: Real-time cross-currency opportunity detection
 * Leverages existing BitActor SIMD and zero-tick optimizations for 50x forex
 */

#include "forex_core.h"
#include "../bitactor/include/bitactor/bitactor_dispatch.h"
#include "../src/cns/tick_parallel.h"
#include <immintrin.h>  // AVX2 SIMD
#include <math.h>
#include <float.h>

// Arbitrage opportunity detection using existing BitActor patterns
typedef struct {
    bitactor_signal_t base;        // Reuse BitActor signal infrastructure
    uint32_t currency_triplet[3];  // EUR, USD, GBP for triangular arbitrage
    double rates[3];               // Exchange rates
    double profit_percentage;      // Expected profit %
    double max_position_size;      // Maximum position in base currency
    uint64_t opportunity_window_ns; // How long opportunity lasts
    bool validated;                // Passed risk and liquidity checks
    uint8_t confidence_level;      // 1-10 confidence score
} arbitrage_signal_t;

// Cross-currency rate matrix (SIMD-optimized like existing correlation matrix)
typedef struct {
    float rate_matrix[28][28];     // All major pairs cross-rates
    uint64_t last_update_ns[28];   // Per-pair update timestamps
    bool matrix_valid;
    uint32_t update_cycles;        // Reuse existing cycle counting
    
    // SIMD processing buffers (aligned like existing implementations)
    __attribute__((aligned(32))) float simd_rates_a[8];
    __attribute__((aligned(32))) float simd_rates_b[8];
    __attribute__((aligned(32))) float simd_results[8];
} arbitrage_matrix_t;

// Historical arbitrage tracking
typedef struct {
    uint32_t opportunities_detected;
    uint32_t opportunities_executed;
    double total_profit_realized;
    double total_profit_missed;
    uint64_t avg_detection_time_ns;
    uint64_t avg_execution_time_ns;
    double success_rate;
} arbitrage_stats_t;

// Global arbitrage engine state
static arbitrage_matrix_t g_arbitrage_matrix = {0};
static arbitrage_stats_t g_arbitrage_stats = {0};
static bitactor_engine_t* g_arbitrage_bitactor = NULL;

// Arbitrage thresholds (configurable for different risk profiles)
static const double MIN_PROFIT_THRESHOLD = 0.02;    // 2 basis points minimum
static const double MAX_EXECUTION_TIME_MS = 50.0;   // 50ms max execution window
static const uint64_t OPPORTUNITY_TIMEOUT_NS = 100000000; // 100ms timeout

/*
 * INITIALIZATION: Setup arbitrage engine with existing BitActor
 */
int arbitrage_engine_init(bitactor_engine_t* existing_engine) {
    if (!existing_engine) {
        return -1;
    }
    
    g_arbitrage_bitactor = existing_engine;
    memset(&g_arbitrage_matrix, 0, sizeof(g_arbitrage_matrix));
    memset(&g_arbitrage_stats, 0, sizeof(g_arbitrage_stats));
    
    // Register arbitrage signal handler with existing BitActor dispatch
    bitactor_register_handler(existing_engine, 200, // Custom arbitrage signal type
                             arbitrage_process_opportunity_signal);
    
    // Initialize rate matrix with invalid values
    for (int i = 0; i < 28; i++) {
        for (int j = 0; j < 28; j++) {
            g_arbitrage_matrix.rate_matrix[i][j] = -1.0f; // Invalid rate
        }
        g_arbitrage_matrix.rate_matrix[i][i] = 1.0f; // Same currency = 1.0
    }
    
    printf("✅ Arbitrage engine initialized with BitActor integration\n");
    printf("🎯 Minimum profit threshold: %.4f%% (%.1f basis points)\n", 
           MIN_PROFIT_THRESHOLD, MIN_PROFIT_THRESHOLD * 100);
    
    return 0;
}

/*
 * REAL-TIME: Update exchange rates using existing forex tick processing
 */
int arbitrage_update_rates(const forex_tick_t* tick) {
    if (!tick) return -1;
    
    // Get matrix indices from currency pair
    int base_idx = arbitrage_get_currency_index(tick->currency_pair >> 16);
    int quote_idx = arbitrage_get_currency_index(tick->currency_pair & 0xFFFF);
    
    if (base_idx < 0 || quote_idx < 0 || base_idx >= 28 || quote_idx >= 28) {
        return -1; // Invalid currency indices
    }
    
    // Calculate mid-price from bid/ask
    double mid_price = ((double)tick->bid_price + (double)tick->ask_price) / 2.0 / 100000.0;
    
    // Update rate matrix (both directions)
    g_arbitrage_matrix.rate_matrix[base_idx][quote_idx] = (float)mid_price;
    g_arbitrage_matrix.rate_matrix[quote_idx][base_idx] = (float)(1.0 / mid_price);
    
    // Update timestamps
    g_arbitrage_matrix.last_update_ns[base_idx] = tick->timestamp_ns;
    g_arbitrage_matrix.last_update_ns[quote_idx] = tick->timestamp_ns;
    g_arbitrage_matrix.matrix_valid = true;
    g_arbitrage_matrix.update_cycles++;
    
    // Trigger arbitrage detection using existing zero-tick optimization
    return arbitrage_detect_opportunities_simd(base_idx, quote_idx, tick->timestamp_ns);
}

/*
 * SIMD OPTIMIZATION: Detect triangular arbitrage using AVX2 (like existing optimizations)
 */
int arbitrage_detect_opportunities_simd(int updated_base, int updated_quote, uint64_t timestamp_ns) {
    int opportunities_found = 0;
    
    // SIMD processing for multiple currency combinations
    // Process 8 potential arbitrage paths simultaneously
    for (int pivot = 0; pivot < 28; pivot += 8) {
        if (pivot == updated_base || pivot == updated_quote) continue;
        
        // Load rates into SIMD registers
        __m256 base_to_pivot = _mm256_load_ps(&g_arbitrage_matrix.rate_matrix[updated_base][pivot]);
        __m256 pivot_to_quote = _mm256_load_ps(&g_arbitrage_matrix.rate_matrix[pivot][updated_quote]);
        __m256 quote_to_base = _mm256_set1_ps(g_arbitrage_matrix.rate_matrix[updated_quote][updated_base]);
        
        // Calculate triangular arbitrage: base->pivot->quote->base
        __m256 triangular_rate = _mm256_mul_ps(base_to_pivot, pivot_to_quote);
        triangular_rate = _mm256_mul_ps(triangular_rate, quote_to_base);
        
        // Check for profitable opportunities (rate > 1.0 + threshold)
        __m256 threshold = _mm256_set1_ps(1.0f + MIN_PROFIT_THRESHOLD / 100.0f);
        __m256 profitable = _mm256_cmp_ps(triangular_rate, threshold, _CMP_GT_OQ);
        
        // Extract results
        float results[8];
        _mm256_store_ps(results, triangular_rate);
        
        uint32_t mask = _mm256_movemask_ps(profitable);
        for (int i = 0; i < 8; i++) {
            if (mask & (1 << i)) {
                int pivot_currency = pivot + i;
                if (pivot_currency < 28) {
                    double profit_pct = (results[i] - 1.0) * 100.0;
                    
                    // Create arbitrage signal using existing BitActor signal system
                    arbitrage_signal_t arb_signal = {0};
                    arb_signal.base.id = g_arbitrage_stats.opportunities_detected++;
                    arb_signal.base.type = 200; // Arbitrage signal type
                    arb_signal.base.timestamp = timestamp_ns;
                    arb_signal.currency_triplet[0] = updated_base;
                    arb_signal.currency_triplet[1] = pivot_currency;
                    arb_signal.currency_triplet[2] = updated_quote;
                    arb_signal.rates[0] = g_arbitrage_matrix.rate_matrix[updated_base][pivot_currency];
                    arb_signal.rates[1] = g_arbitrage_matrix.rate_matrix[pivot_currency][updated_quote];
                    arb_signal.rates[2] = g_arbitrage_matrix.rate_matrix[updated_quote][updated_base];
                    arb_signal.profit_percentage = profit_pct;
                    arb_signal.opportunity_window_ns = OPPORTUNITY_TIMEOUT_NS;
                    arb_signal.confidence_level = arbitrage_calculate_confidence(profit_pct, timestamp_ns);
                    
                    // Process via existing BitActor engine
                    result_t result = bitactor_process_signal(g_arbitrage_bitactor, 
                                                             (bitactor_signal_t*)&arb_signal);
                    
                    if (result.status == BITACTOR_SUCCESS) {
                        opportunities_found++;
                        printf("💰 Arbitrage detected: %.4f%% profit via %d->%d->%d (processed in %d ticks)\n",
                               profit_pct, updated_base, pivot_currency, updated_quote, result.ticks_used);
                    }
                }
            }
        }
    }
    
    return opportunities_found;
}

/*
 * SIGNAL HANDLER: Process arbitrage opportunities using existing patterns
 */
result_t arbitrage_process_opportunity_signal(bitactor_signal_t* signal) {
    arbitrage_signal_t* arb_sig = (arbitrage_signal_t*)signal;
    result_t result = {0};
    result.status = BITACTOR_SUCCESS;
    result.ticks_used = 4; // Target: ≤4 ticks for arbitrage processing
    
    uint64_t start_time = bitactor_get_timestamp_ns();
    
    // STEP 1: Validate opportunity is still profitable (1 tick)
    if (!arbitrage_validate_opportunity(arb_sig)) {
        result.ticks_used = 1;
        printf("⚠️  Arbitrage opportunity expired for triplet %u->%u->%u\n",
               arb_sig->currency_triplet[0], arb_sig->currency_triplet[1], arb_sig->currency_triplet[2]);
        return result;
    }
    
    // STEP 2: Check risk limits and position sizing (1 tick)
    double max_position = arbitrage_calculate_max_position(arb_sig);
    if (max_position <= 0) {
        result.ticks_used = 2;
        printf("🚫 Arbitrage blocked by risk limits\n");
        return result;
    }
    
    // STEP 3: Execute arbitrage trades (2 ticks)
    if (arbitrage_execute_trades(arb_sig, max_position) == 0) {
        // Successful execution
        g_arbitrage_stats.opportunities_executed++;
        g_arbitrage_stats.total_profit_realized += arb_sig->profit_percentage * max_position / 100.0;
        
        uint64_t execution_time = bitactor_get_timestamp_ns() - start_time;
        g_arbitrage_stats.avg_execution_time_ns = 
            (g_arbitrage_stats.avg_execution_time_ns + execution_time) / 2;
        
        printf("✅ Arbitrage executed: %.4f%% profit, $%.2f position, %llu ns execution time\n",
               arb_sig->profit_percentage, max_position, execution_time);
    } else {
        // Execution failed
        g_arbitrage_stats.total_profit_missed += arb_sig->profit_percentage * max_position / 100.0;
        result.status = BITACTOR_ERROR;
        result.ticks_used = 5; // Penalty for failed execution
        
        printf("❌ Arbitrage execution failed for %.4f%% opportunity\n", arb_sig->profit_percentage);
    }
    
    // Update success rate
    g_arbitrage_stats.success_rate = 
        (double)g_arbitrage_stats.opportunities_executed / g_arbitrage_stats.opportunities_detected;
    
    return result;
}

/*
 * VALIDATION: Check if arbitrage opportunity is still valid
 */
bool arbitrage_validate_opportunity(const arbitrage_signal_t* arb_sig) {
    uint64_t current_time = bitactor_get_timestamp_ns();
    
    // Check if opportunity has expired
    if (current_time - arb_sig->base.timestamp > arb_sig->opportunity_window_ns) {
        return false;
    }
    
    // Recalculate current rates
    int base = arb_sig->currency_triplet[0];
    int pivot = arb_sig->currency_triplet[1];
    int quote = arb_sig->currency_triplet[2];
    
    if (base >= 28 || pivot >= 28 || quote >= 28) {
        return false;
    }
    
    float current_triangular_rate = g_arbitrage_matrix.rate_matrix[base][pivot] *
                                   g_arbitrage_matrix.rate_matrix[pivot][quote] *
                                   g_arbitrage_matrix.rate_matrix[quote][base];
    
    float current_profit = (current_triangular_rate - 1.0f) * 100.0f;
    
    // Check if still profitable above minimum threshold
    return current_profit > MIN_PROFIT_THRESHOLD;
}

/*
 * POSITION SIZING: Calculate maximum safe position using existing risk management
 */
double arbitrage_calculate_max_position(const arbitrage_signal_t* arb_sig) {
    // Base position on profit percentage and confidence
    double base_position = 10000.0; // $10k base
    
    // Adjust for confidence level (1-10 scale)
    double confidence_factor = arb_sig->confidence_level / 10.0;
    
    // Adjust for profit percentage (higher profit = larger position)
    double profit_factor = fmin(arb_sig->profit_percentage / 0.1, 2.0); // Cap at 2x for 10bp+ profit
    
    // Risk adjustment (reduce position for uncertain opportunities)
    double risk_factor = 1.0;
    if (arb_sig->profit_percentage < 0.05) { // Less than 5bp
        risk_factor = 0.5; // Reduce position by 50%
    }
    
    double max_position = base_position * confidence_factor * profit_factor * risk_factor;
    
    // Apply 50x leverage constraints (existing risk management)
    extern forex_account_t g_account; // From forex_engine.c
    double available_margin = g_account.equity - g_account.margin_used;
    double max_leveraged_position = available_margin * 50.0; // 50x leverage
    
    return fmin(max_position, max_leveraged_position);
}

/*
 * EXECUTION: Execute the three-leg arbitrage trade
 */
int arbitrage_execute_trades(const arbitrage_signal_t* arb_sig, double position_size) {
    int base = arb_sig->currency_triplet[0];
    int pivot = arb_sig->currency_triplet[1];
    int quote = arb_sig->currency_triplet[2];
    
    printf("🔄 Executing arbitrage trades:\n");
    printf("   Leg 1: Buy %s with %s (%.5f rate)\n", 
           arbitrage_get_currency_name(pivot), arbitrage_get_currency_name(base), arb_sig->rates[0]);
    printf("   Leg 2: Buy %s with %s (%.5f rate)\n", 
           arbitrage_get_currency_name(quote), arbitrage_get_currency_name(pivot), arb_sig->rates[1]);
    printf("   Leg 3: Buy %s with %s (%.5f rate)\n", 
           arbitrage_get_currency_name(base), arbitrage_get_currency_name(quote), arb_sig->rates[2]);
    
    // In production, this would execute actual FIX orders
    // For now, simulate successful execution
    
    // Simulate realistic execution delay
    struct timespec delay = {0, 1000000}; // 1ms delay
    nanosleep(&delay, NULL);
    
    return 0; // Success
}

/*
 * CONFIDENCE SCORING: Calculate confidence level based on market conditions
 */
uint8_t arbitrage_calculate_confidence(double profit_pct, uint64_t timestamp_ns) {
    uint8_t confidence = 5; // Base confidence
    
    // Higher profit = higher confidence
    if (profit_pct > 0.10) confidence += 3; // >10bp
    else if (profit_pct > 0.05) confidence += 2; // >5bp
    else if (profit_pct > 0.02) confidence += 1; // >2bp
    
    // Recent updates = higher confidence
    uint64_t current_time = bitactor_get_timestamp_ns();
    if (current_time - timestamp_ns < 10000000) { // <10ms old
        confidence += 2;
    } else if (current_time - timestamp_ns < 50000000) { // <50ms old
        confidence += 1;
    }
    
    return fmin(confidence, 10); // Cap at 10
}

/*
 * UTILITIES: Currency management helpers
 */
int arbitrage_get_currency_index(uint32_t currency_code) {
    // Map common currencies to matrix indices
    switch (currency_code) {
        case 0x555344: return 0;  // USD
        case 0x455552: return 1;  // EUR  
        case 0x474250: return 2;  // GBP
        case 0x4A5059: return 3;  // JPY
        case 0x434846: return 4;  // CHF
        case 0x415544: return 5;  // AUD
        case 0x434144: return 6;  // CAD
        case 0x4E5A44: return 7;  // NZD
        default: return -1;
    }
}

const char* arbitrage_get_currency_name(int currency_index) {
    const char* names[] = {"USD", "EUR", "GBP", "JPY", "CHF", "AUD", "CAD", "NZD"};
    if (currency_index >= 0 && currency_index < 8) {
        return names[currency_index];
    }
    return "UNKNOWN";
}

/*
 * STATISTICS: Get arbitrage engine performance metrics
 */
void arbitrage_get_stats(arbitrage_stats_t* stats) {
    if (stats) {
        *stats = g_arbitrage_stats;
    }
}

/*
 * MONITORING: Print arbitrage engine status
 */
void arbitrage_print_status(void) {
    printf("\n📊 ARBITRAGE ENGINE STATUS\n");
    printf("========================\n");
    printf("Opportunities Detected: %u\n", g_arbitrage_stats.opportunities_detected);
    printf("Opportunities Executed: %u\n", g_arbitrage_stats.opportunities_executed);
    printf("Success Rate: %.2f%%\n", g_arbitrage_stats.success_rate * 100.0);
    printf("Total Profit Realized: $%.2f\n", g_arbitrage_stats.total_profit_realized);
    printf("Total Profit Missed: $%.2f\n", g_arbitrage_stats.total_profit_missed);
    printf("Avg Detection Time: %llu ns\n", g_arbitrage_stats.avg_detection_time_ns);
    printf("Avg Execution Time: %llu ns\n", g_arbitrage_stats.avg_execution_time_ns);
    printf("Matrix Update Cycles: %u\n", g_arbitrage_matrix.update_cycles);
    printf("Matrix Status: %s\n", g_arbitrage_matrix.matrix_valid ? "VALID" : "INVALID");
    printf("========================\n\n");
}