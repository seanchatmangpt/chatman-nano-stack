/*
 * BitActor Fast Dispatch - 80/20 Win #4
 * SWARM: Algorithm_Optimizer Implementation
 * 
 * Optimizes dispatch algorithms for 50% CPU cycle reduction
 * Target: -100+ cycles through better algorithms
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#ifdef __x86_64__
#include <immintrin.h>
#endif
#include "../include/bitactor/bitactor.h"
#include "../include/bitactor/bitactor_cache_aligned.h"

/* Fast dispatch table using perfect hash */
#define DISPATCH_TABLE_SIZE 256
#define DISPATCH_HASH(type) ((type) & 0xFF)

/* Handler function type */
typedef void (*fast_handler_fn)(const signal_opt_t*, result_opt_t*);

/* Fast dispatch table with cache-aligned entries */
typedef struct {
    dispatch_entry_opt_t entries[DISPATCH_TABLE_SIZE];
    uint32_t active_count;
    uint32_t total_dispatches;
} fast_dispatch_table_t;

static fast_dispatch_table_t g_dispatch_table CACHE_ALIGNED = {0};

/* Branch-free signal validation using bit manipulation */
static inline uint32_t validate_signal_batch_simd(const signal_opt_t* signals, uint32_t count) {
    uint32_t valid_mask = 0;
    
#ifdef __AVX2__
    /* Process 8 signals at once with AVX2 */
    if (count >= 8) {
        /* Load signal types */
        __m256i types = _mm256_set_epi32(
            signals[7].type, signals[6].type, signals[5].type, signals[4].type,
            signals[3].type, signals[2].type, signals[1].type, signals[0].type
        );
        
        /* Check for valid range (0-255) - always true for uint8_t */
        __m256i valid = _mm256_cmpgt_epi32(types, _mm256_set1_epi32(-1));
        
        /* Check for non-zero */
        __m256i nonzero = _mm256_cmpgt_epi32(types, _mm256_setzero_si256());
        
        /* Combine checks */
        valid = _mm256_and_si256(valid, nonzero);
        
        /* Extract mask */
        valid_mask = _mm256_movemask_epi8(valid);
        valid_mask = (valid_mask & 0x88888888) ? 0xFF : 0x00;
    }
#endif
    
    /* Fallback or remainder */
    for (uint32_t i = (valid_mask ? 8 : 0); i < count; i++) {
        /* Branch-free validation */
        uint32_t valid = (signals[i].type != 0) & 
                        (signals[i].id != 0) &
                        ((signals[i].flags & 0xF0) == 0);
        valid_mask |= (valid << i);
    }
    
    return valid_mask;
}

/* Perfect hash dispatch - O(1) lookup */
static inline fast_handler_fn get_handler_perfect_hash(uint8_t signal_type) {
    /* Direct index lookup - no branching */
    dispatch_entry_opt_t* entry = &g_dispatch_table.entries[DISPATCH_HASH(signal_type)];
    
    /* Prefetch handler code */
    PREFETCH_READ(entry->handler);
    
    return (fast_handler_fn)entry->handler;
}

/* Batch dispatch with CPU pipeline optimization */
static void dispatch_signal_batch_fast(const signal_opt_t* signals, 
                                      result_opt_t* results, 
                                      uint32_t count) {
    /* Validate entire batch first */
    uint32_t valid_mask = validate_signal_batch_simd(signals, count);
    
    /* Process valid signals with loop unrolling */
    uint32_t i = 0;
    
    /* Unroll by 4 for better pipeline utilization */
    for (; i + 3 < count; i += 4) {
        /* Prefetch next batch */
        if (i + 4 < count) {
            PREFETCH_READ(&signals[i + 4]);
            PREFETCH_WRITE(&results[i + 4]);
        }
        
        /* Get handlers */
        fast_handler_fn h0 = get_handler_perfect_hash(signals[i + 0].type);
        fast_handler_fn h1 = get_handler_perfect_hash(signals[i + 1].type);
        fast_handler_fn h2 = get_handler_perfect_hash(signals[i + 2].type);
        fast_handler_fn h3 = get_handler_perfect_hash(signals[i + 3].type);
        
        /* Execute handlers if valid */
        if (valid_mask & (1 << (i + 0))) h0(&signals[i + 0], &results[i + 0]);
        if (valid_mask & (1 << (i + 1))) h1(&signals[i + 1], &results[i + 1]);
        if (valid_mask & (1 << (i + 2))) h2(&signals[i + 2], &results[i + 2]);
        if (valid_mask & (1 << (i + 3))) h3(&signals[i + 3], &results[i + 3]);
        
        /* Update statistics */
        g_dispatch_table.total_dispatches += 4;
    }
    
    /* Handle remainder */
    for (; i < count; i++) {
        if (valid_mask & (1 << i)) {
            fast_handler_fn handler = get_handler_perfect_hash(signals[i].type);
            handler(&signals[i], &results[i]);
            g_dispatch_table.total_dispatches++;
        }
    }
}

/* Example optimized handlers */

/* Heartbeat handler - minimal work */
static void handler_heartbeat_fast(const signal_opt_t* sig, result_opt_t* res) {
    res->signal_id = sig->id;
    res->status = 0;  /* OK */
    res->ticks = 0;
    res->exec_hash = 0x48454152;  /* "HEAR" in hex */
    res->result = sig->timestamp;
    res->flags = 0;
}

/* Market data handler - optimized */
static void handler_market_fast(const signal_opt_t* sig, result_opt_t* res) {
    /* Extract price/volume using bit manipulation */
    uint32_t price = (uint32_t)(sig->payload & 0xFFFFFFFF);
    uint32_t volume = (uint32_t)(sig->payload >> 32);
    
    /* Simple calculation without branches */
    uint64_t vwap = ((uint64_t)price * volume) >> 16;
    
    res->signal_id = sig->id;
    res->status = 0;
    res->ticks = 2;
    res->exec_hash = 0x12345678;
    res->result = vwap;
    res->flags = 0;
}

/* Control handler - state machine without branches */
static void handler_control_fast(const signal_opt_t* sig, result_opt_t* res) {
    static const uint8_t state_table[256] = {
        /* Pre-computed state transitions */
        [0] = 1, [1] = 2, [2] = 3, [3] = 0,
        /* ... more states ... */
    };
    
    uint8_t cmd = sig->payload & 0xFF;
    uint8_t next_state = state_table[cmd];
    
    res->signal_id = sig->id;
    res->status = 0;
    res->ticks = 1;
    res->exec_hash = 0xC0010000 | next_state;  /* CTRL marker */
    res->result = next_state;
    res->flags = 0;
}

/* Initialize fast dispatch table */
int fast_dispatch_init(void) {
    memset(&g_dispatch_table, 0, sizeof(g_dispatch_table));
    
    /* Default handler for all types */
    for (int i = 0; i < DISPATCH_TABLE_SIZE; i++) {
        g_dispatch_table.entries[i].handler = (void*)handler_heartbeat_fast;
        g_dispatch_table.entries[i].signal_type = i;
        g_dispatch_table.entries[i].min_priority = 0;
        g_dispatch_table.entries[i].max_ticks = 8;
        g_dispatch_table.entries[i].flags = 0;
    }
    
    /* Register specific handlers */
    g_dispatch_table.entries[0xFF].handler = (void*)handler_heartbeat_fast;
    g_dispatch_table.entries[0x10].handler = (void*)handler_market_fast;
    g_dispatch_table.entries[0x20].handler = (void*)handler_market_fast;
    g_dispatch_table.entries[0x30].handler = (void*)handler_control_fast;
    
    g_dispatch_table.active_count = 4;
    
    return 0;
}

/* Register handler with perfect hash */
int fast_dispatch_register(uint8_t signal_type, fast_handler_fn handler) {
    if (!handler) return -1;
    
    dispatch_entry_opt_t* entry = &g_dispatch_table.entries[DISPATCH_HASH(signal_type)];
    entry->handler = (void*)handler;
    entry->signal_type = signal_type;
    entry->call_count = 0;
    entry->total_cycles = 0;
    
    return 0;
}

/* High-performance signal processing entry point */
int process_signals_fast(const signal_opt_t* signals, 
                        result_opt_t* results,
                        uint32_t count) {
    if (!signals || !results || count == 0) return -1;
    
    /* Process in optimal batch sizes */
    const uint32_t OPTIMAL_BATCH = 32;  /* L1 cache friendly */
    
    for (uint32_t offset = 0; offset < count; offset += OPTIMAL_BATCH) {
        uint32_t batch_size = (count - offset) > OPTIMAL_BATCH ? 
                             OPTIMAL_BATCH : (count - offset);
        
        /* Prefetch next batch while processing current */
        if (offset + OPTIMAL_BATCH < count) {
            prefetch_signal_batch(&signals[offset + OPTIMAL_BATCH], 
                                 batch_size);
        }
        
        /* Process current batch */
        dispatch_signal_batch_fast(&signals[offset], 
                                  &results[offset], 
                                  batch_size);
    }
    
    return 0;
}

/* Get dispatch statistics */
void fast_dispatch_get_stats(void) {
    printf("\n=== Fast Dispatch Statistics ===\n");
    printf("Total dispatches: %u\n", g_dispatch_table.total_dispatches);
    printf("Active handlers: %u\n", g_dispatch_table.active_count);
    
    uint64_t total_calls = 0;
    uint64_t total_cycles = 0;
    
    for (int i = 0; i < DISPATCH_TABLE_SIZE; i++) {
        if (g_dispatch_table.entries[i].call_count > 0) {
            total_calls += g_dispatch_table.entries[i].call_count;
            total_cycles += g_dispatch_table.entries[i].total_cycles;
        }
    }
    
    printf("Total handler calls: %llu\n", total_calls);
    printf("Average cycles per call: %.1f\n", 
           total_calls > 0 ? (double)total_cycles / total_calls : 0.0);
}