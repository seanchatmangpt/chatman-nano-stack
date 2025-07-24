
/*
 * AOT STRATEGIES CNS FOREX INTEGRATION
 * Connects AOT-compiled strategies to CNS forex system
 */

#include "../cns_forex_integration.h"

// AOT strategy function signatures
typedef int (*aot_forex_strategy_fn)(const double* prices, const double* volumes,
                                    uint32_t length, double* signals);

// Strategy registry using existing perfect hash system
static aot_forex_strategy_fn aot_strategy_registry[256] = {0};

// Register AOT strategy with CNS forex system
int cns_forex_aot_register_strategy(uint8_t strategy_id, aot_forex_strategy_fn strategy_fn) {
    uint32_t hash = cns_forex_hash_currency_pair(strategy_id);
    aot_strategy_registry[hash] = strategy_fn;
    
    printf("✅ AOT strategy registered: ID=%u, Hash=0x%02X\n", strategy_id, hash);
    return 0;
}

// Execute AOT strategy through CNS forex system
result_t cns_forex_aot_execute_strategy(signal_t* signal, void* context) {
    uint8_t strategy_id = signal->type;
    uint32_t hash = cns_forex_hash_currency_pair(strategy_id);
    
    aot_forex_strategy_fn strategy = aot_strategy_registry[hash];
    if (!strategy) {
        return (result_t){.status = BITACTOR_INVALID_SIGNAL};
    }
    
    cns_forex_engine_t* engine = (cns_forex_engine_t*)context;
    
    // Extract price data from CNS forex system
    double prices[1000];  // Buffer for price data
    double volumes[1000]; // Buffer for volume data
    double signals[1000]; // Buffer for signals
    
    // Convert CNS forex ticks to arrays
    uint32_t data_length = 0;
    for (uint32_t i = 0; i < 1000 && i < engine->total_ticks_processed; i++) {
        // This would extract from engine->tick_buffer or historical data
        prices[i] = (double)engine->tick_buffer[i % CNS_FOREX_SIMD_BATCH_SIZE].bid_price_scaled / 100000.0;
        volumes[i] = (double)engine->tick_buffer[i % CNS_FOREX_SIMD_BATCH_SIZE].bid_volume;
        data_length++;
    }
    
    // Execute AOT-compiled strategy
    uint64_t start_cycles = bitactor_rdtsc();
    int result = strategy(prices, volumes, data_length, signals);
    uint64_t end_cycles = bitactor_rdtsc();
    
    if (result == 0) {
        // Process generated signals
        for (uint32_t i = 0; i < data_length; i++) {
            if (fabs(signals[i]) > 0.1) {  // Significant signal
                // Create order signal for CNS forex system
                signal_t order_signal = {
                    .id = i,
                    .type = CNS_FOREX_SIGNAL_ORDER,
                    .payload = (uint64_t)(signals[i] * 1000000), // Scale signal
                    .timestamp = bitactor_rdtsc()
                };
                
                // Enqueue order signal
                bitactor_enqueue(engine->bitactor_engine, &order_signal);
            }
        }
    }
    
    return (result_t){
        .status = result == 0 ? BITACTOR_OK : BITACTOR_ERROR,
        .ticks = (uint8_t)((end_cycles - start_cycles) / 1000),
        .result = data_length
    };
}

// Initialize AOT strategies integration
int cns_forex_aot_init(cns_forex_engine_t* engine) {
    // Register AOT strategy handler with BitActor
    int result = bitactor_register(engine->bitactor_engine, 
                                  0x20, // AOT strategy signal type
                                  cns_forex_aot_execute_strategy);
    
    if (result == 0) {
        printf("✅ CNS Forex AOT integration initialized\n");
    }
    
    return result;
}
