#ifndef BITACTOR_DISPATCH_H
#define BITACTOR_DISPATCH_H

#include "bitactor.h"
#include <stdint.h>

// Dispatch table size (must be power of 2)
#define DISPATCH_TABLE_SIZE 1024

// Signal handler registration
typedef struct {
    uint32_t signal_kind;
    handler_fn handler;
    const char* name;
    uint32_t flags;
} dispatch_entry_t;

// Perfect hash dispatcher
typedef struct {
    dispatch_entry_t entries[DISPATCH_TABLE_SIZE];
    uint32_t entry_count;
    uint64_t collision_mask;
} dispatch_table_t;

// Dispatch API
void dispatch_table_init(dispatch_table_t* table);
bool dispatch_register(dispatch_table_t* table, uint32_t kind, handler_fn handler, const char* name);
handler_fn dispatch_lookup(const dispatch_table_t* table, uint32_t kind);
void dispatch_install(bitactor_t* ba, const dispatch_table_t* table);

// Perfect hash function for dispatch
__attribute__((always_inline, const))
static inline uint32_t dispatch_hash(uint32_t kind) {
    // MurmurHash3 finalizer for good distribution
    kind ^= kind >> 16;
    kind *= 0x85ebca6b;
    kind ^= kind >> 13;
    kind *= 0xc2b2ae35;
    kind ^= kind >> 16;
    return kind & (DISPATCH_TABLE_SIZE - 1);
}

// Branchless dispatch
__attribute__((always_inline, hot))
static inline void dispatch_signal(bitactor_t* ba, signal_t* sig) {
    uint32_t idx = dispatch_hash(sig->kind);
    handler_fn handler = ba->dispatch[idx];
    
    // Branchless execution using function pointer
    void (*handlers[2])(signal_t*, void*) = {NULL, handler};
    handlers[handler != NULL](sig, ba->scratch);
}

#endif