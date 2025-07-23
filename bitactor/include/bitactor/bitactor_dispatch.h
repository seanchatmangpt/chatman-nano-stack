/*
 * BitActor Dispatch - Static dispatch table with perfect hashing
 * Branchless execution path
 */
#ifndef BITACTOR_DISPATCH_H
#define BITACTOR_DISPATCH_H

#include <stdint.h>
#include "bitactor.h"

/* Handler function type - executes in bounded ticks */
typedef result_t (*signal_handler_fn)(signal_t* signal, void* context);

/* Dispatch entry - fixed size */
typedef struct {
    signal_handler_fn handler;  /* Handler function */
    void* context;             /* Handler context */
    uint8_t min_priority;      /* Minimum priority */
    uint8_t max_ticks;         /* Max tick budget */
    uint16_t flags;            /* Handler flags */
} dispatch_entry_t;

/* Dispatch table - statically allocated */
typedef struct {
    dispatch_entry_t entries[BITACTOR_DISPATCH_SIZE];
    uint32_t active_count;
} dispatch_table_t;

/* Initialize dispatch table */
void dispatch_init(dispatch_table_t* table);

/* Register a handler - returns 0 on success */
int dispatch_register(dispatch_table_t* table, uint8_t kind, 
                      signal_handler_fn handler, void* context);

/* Dispatch a signal - branchless execution */
static inline result_t dispatch_signal(dispatch_table_t* table, signal_t* signal) {
    /* Perfect hash - direct index lookup */
    dispatch_entry_t* entry = &table->entries[signal->kind];
    
    /* Branchless execution with default handler */
    return entry->handler(signal, entry->context);
}

/* Default handlers */
result_t dispatch_noop(signal_t* signal, void* context);
result_t dispatch_error(signal_t* signal, void* context);

#endif /* BITACTOR_DISPATCH_H */