#include "bitactor_integration.h"
#include <string.h>
#include <stdio.h>

// Static telemetry buffer
static telemetry_extended_t telemetry_buffer[BITACTOR_TELEMETRY_SIZE];

// Initialize CNS BitActor integration
void cns_bitactor_init(cns_bitactor_t* cba) {
    memset(cba, 0, sizeof(cns_bitactor_t));
    
    // Initialize subsystems
    bitactor_parallel_init(&cba->parallel);
    dispatch_table_init(&cba->dispatch);
    telemetry_init(&cba->telemetry, telemetry_buffer, BITACTOR_TELEMETRY_SIZE);
    
    // Spawn initial actor groups
    bitactor_parallel_spawn_group(&cba->parallel);
    
    // Register default handlers
    cns_bitactor_register_handlers(cba);
}

// Register market data handlers
void cns_bitactor_register_handlers(cns_bitactor_t* cba) {
    // Quote validation
    dispatch_register(&cba->dispatch, 0x01, bitactor_handle_quote_validate, "quote_validate");
    
    // Risk check
    dispatch_register(&cba->dispatch, 0x02, bitactor_handle_risk_check, "risk_check");
    
    // Order formatting
    dispatch_register(&cba->dispatch, 0x03, bitactor_handle_order_format, "order_format");
    
    // Install dispatch table to all actors
    for (uint32_t g = 0; g < cba->parallel.group_count; g++) {
        for (uint32_t a = 0; a < BITACTOR_GROUP_SIZE; a++) {
            bitactor_t* actor = &cba->parallel.groups[g].actors[a].actor;
            dispatch_install(actor, &cba->dispatch);
        }
    }
}

// Process incoming quote
void cns_bitactor_process_quote(cns_bitactor_t* cba, quote_t* quote) {
    signal_t sig = {
        .kind = 0x01,  // Quote signal
        .flags = 0,
        .timestamp = __rdtsc(),
        .payload = (uint64_t)quote
    };
    
    // Find best actor group to handle
    uint32_t best_group = 0;
    uint32_t min_load = UINT32_MAX;
    
    for (uint32_t g = 0; g < cba->parallel.group_count; g++) {
        uint32_t load = 0;
        actor_group_t* group = &cba->parallel.groups[g];
        
        for (uint32_t a = 0; a < BITACTOR_GROUP_SIZE; a++) {
            if (group->active_mask & (1ULL << a)) {
                bitactor_t* actor = &group->actors[a].actor;
                load += actor->signal_head - actor->signal_tail;
            }
        }
        
        if (load < min_load) {
            min_load = load;
            best_group = g;
        }
    }
    
    // Enqueue to least loaded actor
    bitactor_parallel_enqueue_batch(&cba->parallel, best_group, &sig, 1);
    cba->quotes_processed++;
}

// Quote validation handler
__attribute__((hot))
void bitactor_handle_quote_validate(signal_t* sig, void* scratch) {
    quote_t* quote = (quote_t*)sig->payload;
    
    // Validation logic (8-tick optimized)
    bool valid = (quote->symbol != 0) &&
                 (quote->price > 0 && quote->price < UINT64_MAX/2) &&
                 (quote->volume > 0) &&
                 (quote->timestamp > 0);
    
    // Store result in scratch
    *(bool*)scratch = valid;
    
    // Record telemetry
    telemetry_extended_t frame = {0};
    telemetry_start(&frame, sig->kind);
    frame.base.exec_hash = 0x01; // Quote validate
    frame.base.trace_ops[1] = valid ? TELEMETRY_OP_EXECUTE : TELEMETRY_OP_ERROR;
    telemetry_end(&frame);
}

// Risk check handler
__attribute__((hot))
void bitactor_handle_risk_check(signal_t* sig, void* scratch) {
    order_t* order = (order_t*)sig->payload;
    
    // Simple risk check
    uint64_t max_quantity = 10000;
    uint64_t max_price = 1000000;
    
    bool pass = (order->quantity <= max_quantity) &&
                (order->price <= max_price);
    
    *(bool*)scratch = pass;
    
    // Record telemetry
    telemetry_extended_t frame = {0};
    telemetry_start(&frame, sig->kind);
    frame.base.exec_hash = 0x02; // Risk check
    frame.base.trace_ops[1] = pass ? TELEMETRY_OP_EXECUTE : TELEMETRY_OP_ERROR;
    telemetry_end(&frame);
}

// Order format handler
__attribute__((hot))
void bitactor_handle_order_format(signal_t* sig, void* scratch) {
    order_t* order = (order_t*)sig->payload;
    
    // Format order for transmission
    order->flags |= 0x8000; // Set formatted flag
    
    // Store in scratch for pipeline
    memcpy(scratch, order, sizeof(order_t));
    
    // Record telemetry
    telemetry_extended_t frame = {0};
    telemetry_start(&frame, sig->kind);
    frame.base.exec_hash = 0x03; // Order format
    frame.base.trace_ops[1] = TELEMETRY_OP_EXECUTE;
    telemetry_end(&frame);
}

// Main tick function
void cns_bitactor_tick(cns_bitactor_t* cba) {
    uint64_t start = __rdtsc();
    
    // Execute parallel actor tick
    bitactor_parallel_tick(&cba->parallel);
    
    // Execute pipeline tick if integrated
    if (cba->pipeline_units) {
        bitactor_pipeline_tick(cba);
    }
    
    // Update latency tracking
    uint64_t latency = __rdtsc() - start;
    cba->average_latency = (cba->average_latency * 7 + latency) / 8;
}

// Pipeline integration
void bitactor_integrate_pipeline(cns_bitactor_t* cba, tick_unit_t* units, uint32_t count) {
    cba->pipeline_units = units;
    cba->pipeline_count = count;
}

void bitactor_pipeline_tick(cns_bitactor_t* cba) {
    // Execute pipeline units
    for (uint32_t i = 0; i < cba->pipeline_count; i++) {
        tick_execute(&cba->pipeline_units[i]);
    }
}

// Get performance statistics
void bitactor_get_stats(const cns_bitactor_t* cba, bitactor_stats_t* stats) {
    stats->tick_count = cba->parallel.global_tick;
    stats->signal_count = cba->parallel.total_signals;
    stats->cycle_total = cba->parallel.total_cycles;
    
    // Calculate min/max/avg from all actors
    stats->cycle_min = UINT64_MAX;
    stats->cycle_max = 0;
    uint64_t total_cycles = 0;
    uint32_t actor_count = 0;
    
    for (uint32_t g = 0; g < cba->parallel.group_count; g++) {
        for (uint32_t a = 0; a < BITACTOR_GROUP_SIZE; a++) {
            const bitactor_t* actor = &cba->parallel.groups[g].actors[a].actor;
            if (actor->tick_count > 0) {
                uint64_t avg = actor->cycle_count / actor->tick_count;
                if (avg < stats->cycle_min) stats->cycle_min = avg;
                if (avg > stats->cycle_max) stats->cycle_max = avg;
                total_cycles += actor->cycle_count;
                actor_count++;
            }
        }
    }
    
    if (actor_count > 0) {
        stats->cycle_avg = (double)total_cycles / actor_count;
    }
    
    stats->efficiency = (double)cba->parallel.parallel_efficiency;
}