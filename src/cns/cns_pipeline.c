#include "cns_pipeline.h"
#include <string.h>
#include <stdio.h>

static char g_arena_memory[1024 * 1024];  // 1MB arena
static void* g_arena_ptr = g_arena_memory;
static order_t* g_order_buffer = NULL;
static fast_proof_t g_proof = {.capability = 0x1234567890ABCDEF, .hash = 0xB3C4D5E6F7A8B9C0};
static rule_t g_rule = {.mask = 0xFFFF, .threshold = 100, .action = 1};
static uint64_t g_price_buffer = 0;
static uint64_t g_risk_result = 0;
static char g_fix_buffer[256];
static fast_nic_t g_nic = {.rx_ring = NULL, .tx_ring = NULL};

void op_validate_quote(void* data) {
    quote_t* quote = (quote_t*)data;
    if (quote->price == 0 || quote->volume == 0) {
        quote->timestamp = 0;
    }
}

void op_arena_alloc(void* data) {
    g_order_buffer = (order_t*)tick_arena_alloc(&g_arena_ptr, sizeof(order_t));
    if (g_order_buffer && data) {
        // Initialize order from quote data
        quote_t* quote = (quote_t*)data;
        g_order_buffer->symbol = quote->symbol;
        g_order_buffer->price = quote->price;
        g_order_buffer->quantity = quote->volume;
        // timestamp field might be in a different structure
    }
}

void op_bitactor_verify(void* data) {
    fast_proof_t* proof = (fast_proof_t*)data;
    if (!bitactor_verify_fast(proof)) {
        g_order_buffer = NULL;
    }
}

void op_apply_rule(void* data) {
    rule_t* rule = (rule_t*)data;
    if (!g_order_buffer) {
        g_risk_result = 0;
        return;
    }
    uint64_t match = ((g_order_buffer->price & rule->mask) > rule->threshold);
    g_risk_result = rule->action & -match;
}

void op_calculate_price(void* data) {
    uint64_t* buffer = (uint64_t*)data;
    if (!g_order_buffer) {
        *buffer = 0;
        return;
    }
    *buffer = g_order_buffer->price + (g_order_buffer->quantity / 1000);
}

void op_risk_check(void* data) {
    uint64_t* risk = (uint64_t*)data;
    if (g_price_buffer > 1000000) {
        *risk = 0;
    } else {
        *risk = 1;
    }
}

void op_format_order(void* data) {
    char* buffer = (char*)data;
    if (g_order_buffer && g_risk_result) {
        sprintf(buffer, "35=D|55=%llu|54=1|38=%llu|44=%llu|",
                (unsigned long long)g_order_buffer->symbol,
                (unsigned long long)g_order_buffer->quantity,
                (unsigned long long)g_order_buffer->price);
    }
}

void op_send_order(void* data) {
    fast_nic_t* nic = (fast_nic_t*)data;
    if (g_fix_buffer[0] && nic->tx_ring) {
        *nic->tx_ring = (uint64_t)(uintptr_t)g_fix_buffer;
    }
}

__attribute__((hot)) void process_quote_8tick(quote_t* quote) {
    tick_unit_t unit = {
        .ops = {
            op_validate_quote,
            op_arena_alloc,
            op_bitactor_verify,
            op_apply_rule,
            op_calculate_price,
            op_risk_check,
            op_format_order,
            op_send_order
        },
        .data = {quote, quote, &g_proof, &g_rule, 
                 &g_price_buffer, &g_risk_result, g_fix_buffer, &g_nic},
        .tick_mask = 0xFF
    };
    
    tick_execute(&unit);
}