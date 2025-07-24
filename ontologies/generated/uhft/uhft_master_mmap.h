/*
 * Generated Memory-Mapped Triple Store: uhft_master
 * Generated: 2025-07-23 22:19:34
 * Triples: 2
 * Zero-parse overhead semantic processing
 */

#ifndef UHFT_MASTER_MMAP_H
#define UHFT_MASTER_MMAP_H

#include <stdint.h>
#include <stdbool.h>

// Triple structure (24 bytes, cache-aligned)
typedef struct {
    uint64_t subject;   // Subject URI hash
    uint64_t predicate; // Predicate URI hash  
    uint64_t object;    // Object URI/literal hash
} __attribute__((packed)) triple_t;

// Memory-mapped triple store
typedef struct {
    uint32_t count;
    uint32_t capacity;
    triple_t* triples;
} triple_store_t;

// URI Hash Constants
#define __HASH 0xD03502C43D74A30BULL
#define A_HASH 0xCA978112CA1BBDCAULL
#define CORE_HASH 0xF2AC8608D9E1FA83ULL
#define ONTOLOGY___HASH 0xEBFB2CA589BE5644ULL
#define IMPORTS_HASH 0x20B9E5488DE52D00ULL
#define UHFT_CORE_TTL_HASH 0xE4F83730B6AD2B2EULL

// Compiled triple data
static triple_t uhft_master_triples[] = {
    {0xF2AC8608D9E1FA83ULL, 0xCA978112CA1BBDCAULL, 0xEBFB2CA589BE5644ULL},
    {0x20B9E5488DE52D00ULL, 0xE4F83730B6AD2B2EULL, 0xD03502C43D74A30BULL},
};

static triple_store_t uhft_master_store = {
    .count = 2,
    .capacity = 2,
    .triples = uhft_master_triples
};

// Fast triple lookup (8-tick optimized)
static inline bool uhft_master_has_triple(uint64_t s, uint64_t p, uint64_t o) {
    triple_t* t = uhft_master_store.triples;
    uint32_t count = uhft_master_store.count;
    
    // Linear search with SIMD potential
    for (uint32_t i = 0; i < count; i++) {
        if (t[i].subject == s && t[i].predicate == p && t[i].object == o) {
            return true;
        }
    }
    return false;
}

// Get objects for subject/predicate pair
static inline uint64_t uhft_master_get_object(uint64_t s, uint64_t p) {
    triple_t* t = uhft_master_store.triples;
    uint32_t count = uhft_master_store.count;
    
    for (uint32_t i = 0; i < count; i++) {
        if (t[i].subject == s && t[i].predicate == p) {
            return t[i].object;
        }
    }
    return 0; // Not found
}

// Count triples matching pattern
static inline uint32_t uhft_master_count_pattern(uint64_t s, uint64_t p, uint64_t o) {
    triple_t* t = uhft_master_store.triples;
    uint32_t count = uhft_master_store.count;
    uint32_t matches = 0;
    
    for (uint32_t i = 0; i < count; i++) {
        if ((s == 0 || t[i].subject == s) &&
            (p == 0 || t[i].predicate == p) &&
            (o == 0 || t[i].object == o)) {
            matches++;
        }
    }
    return matches;
}

#endif // UHFT_MASTER_MMAP_H
