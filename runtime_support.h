/*
 * Runtime Support Library for AOT Validation
 * Provides core functions for graph access and constraint checking
 */

#ifndef RUNTIME_SUPPORT_H
#define RUNTIME_SUPPORT_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Simple triple store for testing
typedef struct triple {
    const char* subject;
    const char* predicate;
    const char* object;
    struct triple* next;
} triple_t;

typedef struct {
    triple_t* triples;
    size_t count;
} graph_t;

// Graph operations
static inline graph_t* graph_create() {
    return calloc(1, sizeof(graph_t));
}

static inline void graph_add_triple(graph_t* g, const char* s, const char* p, const char* o) {
    triple_t* t = calloc(1, sizeof(triple_t));
    t->subject = s;
    t->predicate = p;
    t->object = o;
    t->next = g->triples;
    g->triples = t;
    g->count++;
}

static inline bool has_property(const void* node, const char* property) {
    // For testing, assume node is a string subject
    const char* subject = (const char*)node;
    graph_t* g = (graph_t*)node; // Hack: pass graph as node for testing
    
    triple_t* t = g->triples;
    while (t) {
        if (strcmp(t->predicate, property) == 0) {
            return true;
        }
        t = t->next;
    }
    return false;
}

static inline uint32_t count_property_values(const void* node, const char* property) {
    const char* subject = (const char*)node;
    graph_t* g = (graph_t*)node; // Hack for testing
    uint32_t count = 0;
    
    triple_t* t = g->triples;
    while (t) {
        if (strcmp(t->predicate, property) == 0) {
            count++;
        }
        t = t->next;
    }
    return count;
}

static inline const char* get_property_value(const void* node, const char* property) {
    graph_t* g = (graph_t*)node;
    triple_t* t = g->triples;
    while (t) {
        if (strcmp(t->predicate, property) == 0) {
            return t->object;
        }
        t = t->next;
    }
    return NULL;
}

static inline bool check_datatype(const void* node, const char* property, const char* datatype) {
    // Simplified: just check if property exists
    return has_property(node, property);
}

static inline bool match_pattern(const char* value, const char* pattern) {
    // Simplified: just check non-null
    return value != NULL;
}

static inline bool is_instance_of(const void* node, const char* class) {
    // Simplified: always true for testing
    return true;
}

static inline bool validate_datatype_fast(const void* node, const char* property, const char* datatype) {
    return check_datatype(node, property, datatype);
}

// Test data population
static inline void populate_test_data(graph_t* g) {
    graph_add_triple(g, "http://example.org/test#instance1", 
                        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                        "http://example.org/test#TestClass");
    graph_add_triple(g, "http://example.org/test#instance1",
                        "http://example.org/test#testProperty", 
                        "Test Value");
}

#endif // RUNTIME_SUPPORT_H